# =============================================================================
# R-native .mdm binary writer.
# =============================================================================
#
# Writes HLM's binary .mdm files directly from R, eliminating the need for
# hlm*.exe -w (and the whlm.exe warm-session workaround).
#
# Byte layouts derived from RE of hlm2/3/4.exe writers and cross-validated
# against live .mdm files. See re/notes/04_mdm_writer.md (HLM3),
# re/notes/07_mdm_hlm2.md (HLM2), re/notes/08_mdm_hlm4.md (HLM4).
#
# Format overview (HLM3 Normal-outcome):
#   [fixed header: magic + version + counts + flags + var names + source paths]
#   [grand means: L1/L2/L3 per-variable means]
#   [L3 IDs]
#   [L3 row matrix: n_L3 × n_L3_vars doubles]
#   [per-L2 blocks: one variable-length block per L2 unit, containing
#    nrows + L1-means + L1-rows + L3-id + L2-id + L2-row]
#   [no trailer]

# Missing-data sentinel. HLM uses a large negative double.
# The exact value is DAT_004d39d8 in hlm3.exe — set to -9999.0 in the
# HLM2 writer spec and likely similar in HLM3/4.
.HLM_MISSING_SENTINEL <- -9999.0

#' Encode a variable name into HLM's 13-byte slot format.
#'
#' HLM stores names as: 8 chars RIGHT-justified (space-padded on the left),
#' then 4 trailing spaces, then 1 NUL byte. Total = 13 bytes. Discovered
#' by comparing our R-written .mdm against hlm3.exe-written reference.
#'
#' @return a raw vector of exactly `width` bytes
.pad_name <- function(name, width = 13L) {
  s <- toupper(substr(name, 1L, 8L))
  rj <- sprintf("%8s", s)                     # right-justify in 8 chars
  padded <- paste0(rj, "    ")                 # + 4 trailing spaces = 12
  raw_s <- charToRaw(padded)
  c(raw_s, raw(width - length(raw_s)))         # + NUL pad to 13
}

# ---- low-level binary writers ----

.write_int32 <- function(con, x) writeBin(as.integer(x), con, size = 4L, endian = "little")
.write_float32 <- function(con, x) writeBin(as.double(x), con, size = 4L, endian = "little")
.write_double <- function(con, x) writeBin(as.double(x), con, size = 8L, endian = "little")
.write_raw_string <- function(con, s) {
  if (is.raw(s)) writeBin(s, con)
  else writeBin(charToRaw(s), con)
}

#' Write an HLM3 .mdm file natively.
#'
#' Takes the same inputs as hlm_build_mdm3 (after sorting, truncation, and
#' sav-writing have already been done) and emits a binary .mdm that
#' hlm3.exe can read directly for model fitting.
#'
#' @param l1  data.frame: sorted level-1 data (all columns)
#' @param l2  data.frame: level-2 data (one row per L2 unit)
#' @param l3  data.frame: level-3 data (one row per L3 unit)
#' @param l3_id,l2_id  character: ID column names
#' @param l1_vars,l2_vars,l3_vars  character vectors of analysis variable names
#'            (in source order, matching what would go in the .mdmt)
#' @param mdm_path  output path for the .mdm file
#' @param l1_sav_win,l2_sav_win,l3_sav_win  Windows paths to the .sav files
#'            (stored verbatim in the .mdm header as source-file metadata)
#' @param l1_missing  logical: level-1 has missing data?
#' @param listwise_delete  logical: delete missing at MDM-creation time?
#'
#' @return invisible(mdm_path)
hlm_write_mdm3 <- function(l1, l2, l3,
                           l3_id, l2_id,
                           l1_vars, l2_vars, l3_vars,
                           mdm_path,
                           l1_sav_win = "", l2_sav_win = "", l3_sav_win = "",
                           l1_missing = TRUE, listwise_delete = FALSE) {
  # Validate
  stopifnot(is.data.frame(l1), is.data.frame(l2), is.data.frame(l3))
  n_l1_vars <- length(l1_vars)
  n_l2_vars <- length(l2_vars)
  n_l3_vars <- length(l3_vars)

  # Build the per-L2 unit structure via sort-merge (trivial since input is
  # pre-sorted). Group L1 by (l3_id, l2_id).
  l1_key <- paste0(as.character(l1[[l3_id]]), "\x01", as.character(l1[[l2_id]]))
  l2_key <- paste0(as.character(l2[[l3_id]]), "\x01", as.character(l2[[l2_id]]))
  l3_key <- as.character(l3[[l3_id]])

  # All L1 data as a numeric matrix (analysis vars only)
  l1_mat <- as.matrix(l1[, l1_vars, drop = FALSE])
  l1_mat[is.na(l1_mat)] <- .HLM_MISSING_SENTINEL
  storage.mode(l1_mat) <- "double"

  l2_mat <- as.matrix(l2[, l2_vars, drop = FALSE])
  l2_mat[is.na(l2_mat)] <- .HLM_MISSING_SENTINEL
  storage.mode(l2_mat) <- "double"

  l3_mat <- as.matrix(l3[, l3_vars, drop = FALSE])
  l3_mat[is.na(l3_mat)] <- .HLM_MISSING_SENTINEL
  storage.mode(l3_mat) <- "double"

  # Group L1 rows by their L2 key
  l2_units <- unique(l2_key)
  n_l2_units <- length(l2_units)
  n_l3_units <- nrow(l3)
  n_l1_total <- nrow(l1)

  # Determine if IDs are numeric (HLM encodes this via sign bit on mdmtype)
  ids_numeric <- is.numeric(l1[[l3_id]]) && is.numeric(l1[[l2_id]])

  # Compute grand means
  l1_means <- colMeans(l1_mat, na.rm = TRUE)
  l2_means <- colMeans(l2_mat, na.rm = TRUE)
  l3_means <- colMeans(l3_mat, na.rm = TRUE)

  # Compute max rows per L2 unit
  l1_split <- split(seq_len(nrow(l1)), l1_key)
  max_rows_per_l2 <- max(vapply(l1_split, length, integer(1)))

  # Signed mdmtype: internal HLM3 mdmtype is 6 (from test_thomas_3 analysis).
  # Sign encodes numeric IDs.
  internal_mdmtype <- 6L
  signed_mdmtype <- if (ids_numeric) -internal_mdmtype else internal_mdmtype

  # ---- WRITE ----
  con <- file(mdm_path, "wb")
  on.exit(close(con))

  # B.1 — Fixed header
  .write_raw_string(con, "SSHLM3")                         # magic (6 bytes)
  .write_float32(con, 8.2)                                  # version (float32)
  .write_int32(con, n_l1_vars)                              # n_L1_vars
  .write_int32(con, n_l2_vars)                              # n_L2_vars
  .write_int32(con, n_l3_vars)                              # n_L3_vars
  .write_int32(con, n_l1_total)                             # n_L1_records_total
  .write_int32(con, n_l2_units)                             # n_L2_units
  .write_int32(con, n_l3_units)                             # n_L3_units
  .write_int32(con, as.integer(l1_missing))                 # flag_level1_may_miss
  .write_int32(con, as.integer(listwise_delete))            # flag_listwise_delete
  .write_int32(con, 0L)                                     # outcome_family = Normal
  .write_int32(con, signed_mdmtype)                         # signed_mdmtype
  .write_int32(con, max_rows_per_l2)                        # max_rows_per_L2

  # Var name arrays: INTRCPT + each var, 13-byte padded slots
  for (v in c("INTRCPT1", l1_vars)) .write_raw_string(con, .pad_name(v))
  for (v in c("INTRCPT2", l2_vars)) .write_raw_string(con, .pad_name(v))
  for (v in c("INTRCPT3", l3_vars)) .write_raw_string(con, .pad_name(v))

  # Path field length + source paths (260 bytes each, null-padded)
  .write_int32(con, 260L)                                   # path_field_len
  for (p in c(l1_sav_win, l2_sav_win, l3_sav_win)) {
    raw <- charToRaw(substr(p, 1, 259))
    pad <- raw(260L - length(raw))
    writeBin(c(raw, pad), con)
  }

  # B.2 — Grand means
  .write_double(con, l1_means)
  .write_double(con, l2_means)
  .write_double(con, l3_means)

  # L3 IDs
  if (ids_numeric) {
    .write_double(con, as.double(l3[[l3_id]]))
  } else {
    for (id in as.character(l3[[l3_id]])) .write_raw_string(con, .pad_name(id))
  }

  # L3 row matrix: n_L3 × n_L3_vars doubles, row-major
  for (i in seq_len(n_l3_units)) .write_double(con, l3_mat[i, ])

  # B.3 — Per-L2 blocks
  for (uk in l2_units) {
    l1_idx <- l1_split[[uk]]
    nrows  <- length(l1_idx)
    l2_row_idx <- match(uk, l2_key)
    l3_id_val <- l2[[l3_id]][l2_row_idx]
    l2_id_val <- l2[[l2_id]][l2_row_idx]

    # Per-L2 L1 means (within-unit)
    l1_block <- l1_mat[l1_idx, , drop = FALSE]
    l1_unit_means <- colMeans(l1_block, na.rm = TRUE)

    .write_int32(con, nrows)
    .write_double(con, l1_unit_means)
    for (r in seq_len(nrows)) .write_double(con, l1_block[r, ])

    # IDs
    if (ids_numeric) {
      .write_double(con, as.double(l3_id_val))
      .write_double(con, as.double(l2_id_val))
    } else {
      .write_raw_string(con, .pad_name(as.character(l3_id_val)))
      .write_raw_string(con, .pad_name(as.character(l2_id_val)))
    }

    # L2 row
    .write_double(con, l2_mat[l2_row_idx, ])
  }

  invisible(mdm_path)
}

#' Write an HLM2 .mdm file natively.
#'
#' Simpler than HLM3: no L3 data, no L3 IDs, no L3 matrix. Per-L2 blocks
#' carry only L1 data + the L2 ID + the L2 row.
#' Layout per re/notes/07_mdm_hlm2.md.
hlm_write_mdm2 <- function(l1, l2,
                           l2_id,
                           l1_vars, l2_vars,
                           mdm_path,
                           l1_sav_win = "", l2_sav_win = "",
                           l1_missing = TRUE, listwise_delete = FALSE) {
  stopifnot(is.data.frame(l1), is.data.frame(l2))
  n_l1_vars <- length(l1_vars)
  n_l2_vars <- length(l2_vars)

  l1_mat <- as.matrix(l1[, l1_vars, drop = FALSE])
  l1_mat[is.na(l1_mat)] <- .HLM_MISSING_SENTINEL
  storage.mode(l1_mat) <- "double"

  l2_mat <- as.matrix(l2[, l2_vars, drop = FALSE])
  l2_mat[is.na(l2_mat)] <- .HLM_MISSING_SENTINEL
  storage.mode(l2_mat) <- "double"

  l2_key <- as.character(l2[[l2_id]])
  l1_key <- as.character(l1[[l2_id]])
  n_l2_units <- nrow(l2)
  n_l1_total <- nrow(l1)

  ids_numeric <- is.numeric(l1[[l2_id]])
  l1_means <- colMeans(l1_mat, na.rm = TRUE)
  l2_means <- colMeans(l2_mat, na.rm = TRUE)

  l1_split <- split(seq_len(nrow(l1)), l1_key)
  max_rows_per_l2 <- max(vapply(l1_split, length, integer(1)))

  internal_mdmtype <- 1L  # HLM2 internal type
  signed_mdmtype <- if (ids_numeric) -internal_mdmtype else internal_mdmtype

  con <- file(mdm_path, "wb")
  on.exit(close(con))

  # Header
  .write_raw_string(con, "SSHLM2")
  .write_float32(con, 8.2)
  .write_int32(con, n_l1_vars)
  .write_int32(con, n_l2_vars)
  .write_int32(con, n_l1_total)
  .write_int32(con, n_l2_units)
  .write_int32(con, as.integer(l1_missing))
  .write_int32(con, as.integer(listwise_delete))
  .write_int32(con, 0L)                     # outcome_family = Normal
  .write_int32(con, signed_mdmtype)
  .write_int32(con, max_rows_per_l2)

  # Var names
  for (v in c("INTRCPT1", l1_vars)) .write_raw_string(con, .pad_name(v))
  for (v in c("INTRCPT2", l2_vars)) .write_raw_string(con, .pad_name(v))

  # Path field + source paths (2 for HLM2)
  .write_int32(con, 260L)
  for (p in c(l1_sav_win, l2_sav_win)) {
    raw <- charToRaw(substr(p, 1, 259))
    pad <- raw(260L - length(raw))
    writeBin(c(raw, pad), con)
  }

  # Grand means (L1 + L2 only)
  .write_double(con, l1_means)
  .write_double(con, l2_means)

  # L2 IDs
  if (ids_numeric) {
    .write_double(con, as.double(l2[[l2_id]]))
  } else {
    for (id in as.character(l2[[l2_id]])) .write_raw_string(con, .pad_name(id))
  }

  # Per-L2 blocks (HLM2: no L3 id, just L2 id + L1 data + L2 row)
  for (i in seq_len(n_l2_units)) {
    uk <- l2_key[i]
    l1_idx <- l1_split[[uk]]
    nrows <- length(l1_idx)
    l1_block <- l1_mat[l1_idx, , drop = FALSE]
    l1_unit_means <- colMeans(l1_block, na.rm = TRUE)

    .write_int32(con, nrows)
    .write_double(con, l1_unit_means)
    for (r in seq_len(nrows)) .write_double(con, l1_block[r, ])

    if (ids_numeric) {
      .write_double(con, as.double(l2[[l2_id]][i]))
    } else {
      .write_raw_string(con, .pad_name(as.character(l2[[l2_id]][i])))
    }
    .write_double(con, l2_mat[i, ])
  }

  invisible(mdm_path)
}
