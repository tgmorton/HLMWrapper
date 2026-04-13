# =============================================================================
# R-native .mdm binary writer for HLM2, HLM3, and HLM4.
# =============================================================================
#
# Writes valid .mdm files directly from R, eliminating Wine for MDM build.
# Byte layouts from re/notes/04_mdm_writer.md (HLM3), 07_mdm_hlm2.md (HLM2),
# 08_mdm_hlm4.md (HLM4). HLM3 writer is byte-identical to hlm3.exe output.

.HLM_MISSING <- -9999.0

# ---- low-level helpers ----
.w_int32   <- function(con, x) writeBin(as.integer(x),  con, size = 4L, endian = "little")
.w_float32 <- function(con, x) writeBin(as.double(x),   con, size = 4L, endian = "little")
.w_double  <- function(con, x) writeBin(as.double(x),   con, size = 8L, endian = "little")
.w_raw     <- function(con, x) { if (is.raw(x)) writeBin(x, con) else writeBin(charToRaw(x), con) }

#' HLM name slot: 8-char right-justified + 4 trailing spaces + NUL = 13 bytes.
.pad13 <- function(name) {
  s <- toupper(substr(name, 1L, 8L))
  rj <- sprintf("%8s", s)
  raw_s <- charToRaw(paste0(rj, "    "))
  c(raw_s, raw(13L - length(raw_s)))
}

#' Write a 260-byte NUL-padded path field.
.w_path <- function(con, p) {
  raw <- if (nzchar(p)) charToRaw(substr(p, 1, 259)) else raw(0)
  writeBin(c(raw, raw(260L - length(raw))), con)
}

#' Prepare a numeric matrix from a data.frame + var names.
.as_dmat <- function(df, vars) {
  m <- as.matrix(df[, vars, drop = FALSE])
  m[is.na(m)] <- .HLM_MISSING
  storage.mode(m) <- "double"
  m
}

# ============================================================================
# HLM3
# ============================================================================
hlm_write_mdm3 <- function(l1, l2, l3,
                           l3_id, l2_id,
                           l1_vars, l2_vars, l3_vars,
                           mdm_path,
                           l1_sav_win = "", l2_sav_win = "", l3_sav_win = "",
                           l1_missing = TRUE, listwise_delete = FALSE) {
  l1_mat <- .as_dmat(l1, l1_vars); l2_mat <- .as_dmat(l2, l2_vars); l3_mat <- .as_dmat(l3, l3_vars)
  l1_key <- paste0(as.character(l1[[l3_id]]), "\x01", as.character(l1[[l2_id]]))
  l2_key <- paste0(as.character(l2[[l3_id]]), "\x01", as.character(l2[[l2_id]]))
  ids_numeric <- is.numeric(l1[[l3_id]]) && is.numeric(l1[[l2_id]])
  l1_split <- split(seq_len(nrow(l1)), l1_key)
  l2_units <- unique(l2_key)
  internal_mdmtype <- 6L
  signed_mdmtype <- if (ids_numeric) -internal_mdmtype else internal_mdmtype

  con <- file(mdm_path, "wb"); on.exit(close(con))
  # Header
  .w_raw(con, "SSHLM3")
  .w_float32(con, 8.2)
  .w_int32(con, length(l1_vars)); .w_int32(con, length(l2_vars)); .w_int32(con, length(l3_vars))
  .w_int32(con, nrow(l1)); .w_int32(con, length(l2_units)); .w_int32(con, nrow(l3))
  .w_int32(con, as.integer(l1_missing)); .w_int32(con, as.integer(listwise_delete))
  .w_int32(con, 0L)  # outcome_family = Normal
  .w_int32(con, signed_mdmtype)
  .w_int32(con, max(vapply(l1_split, length, integer(1))))
  # Var names
  for (v in c("INTRCPT1", l1_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT2", l2_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT3", l3_vars)) .w_raw(con, .pad13(v))
  # Paths
  .w_int32(con, 260L)
  .w_path(con, l1_sav_win); .w_path(con, l2_sav_win); .w_path(con, l3_sav_win)
  # Grand means
  .w_double(con, colMeans(l1_mat, na.rm = TRUE))
  .w_double(con, colMeans(l2_mat, na.rm = TRUE))
  .w_double(con, colMeans(l3_mat, na.rm = TRUE))
  # L3 IDs
  if (ids_numeric) .w_double(con, as.double(l3[[l3_id]]))
  else for (id in as.character(l3[[l3_id]])) .w_raw(con, .pad13(id))
  # L3 row matrix
  for (i in seq_len(nrow(l3))) .w_double(con, l3_mat[i, ])
  # Per-L2 blocks
  for (uk in l2_units) {
    idx <- l1_split[[uk]]; nrows <- length(idx)
    l2i <- match(uk, l2_key)
    blk <- l1_mat[idx, , drop = FALSE]
    .w_int32(con, nrows)
    .w_double(con, colMeans(blk, na.rm = TRUE))
    for (r in seq_len(nrows)) .w_double(con, blk[r, ])
    if (ids_numeric) { .w_double(con, as.double(l2[[l3_id]][l2i])); .w_double(con, as.double(l2[[l2_id]][l2i])) }
    else { .w_raw(con, .pad13(as.character(l2[[l3_id]][l2i]))); .w_raw(con, .pad13(as.character(l2[[l2_id]][l2i]))) }
    .w_double(con, l2_mat[l2i, ])
  }
  invisible(mdm_path)
}

# ============================================================================
# HLM2 — per re/notes/07_mdm_hlm2.md
# ============================================================================
hlm_write_mdm2 <- function(l1, l2,
                           l2_id,
                           l1_vars, l2_vars,
                           mdm_path,
                           l1_sav_win = "", l2_sav_win = "",
                           l1_missing = TRUE, listwise_delete = FALSE) {
  l1_mat <- .as_dmat(l1, l1_vars); l2_mat <- .as_dmat(l2, l2_vars)
  l1_key <- as.character(l1[[l2_id]]); l2_key <- as.character(l2[[l2_id]])
  ids_numeric <- is.numeric(l1[[l2_id]])
  l1_split <- split(seq_len(nrow(l1)), l1_key)
  internal_mdmtype <- 6L
  signed_mdmtype <- if (ids_numeric) -internal_mdmtype else internal_mdmtype

  con <- file(mdm_path, "wb"); on.exit(close(con))
  # Header
  .w_raw(con, "SSHLM2")
  .w_float32(con, 8.2)
  .w_int32(con, length(l1_vars)); .w_int32(con, length(l2_vars))
  .w_int32(con, nrow(l1)); .w_int32(con, nrow(l2))
  .w_int32(con, as.integer(l1_missing)); .w_int32(con, as.integer(listwise_delete))
  .w_int32(con, 0L)  # outcome_family
  .w_int32(con, 0L)  # n_constraints (version > 7.29)
  .w_int32(con, 0L)  # extra_level (version > 6.209)
  .w_int32(con, signed_mdmtype)
  .w_int32(con, max(vapply(l1_split, length, integer(1))))
  # Var names
  for (v in c("INTRCPT1", l1_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT2", l2_vars)) .w_raw(con, .pad13(v))
  # Paths
  .w_int32(con, 260L)
  .w_path(con, l1_sav_win); .w_path(con, l2_sav_win)
  # Grand means + SDs (version > 7.215 requires SDs)
  l1_means <- colMeans(l1_mat, na.rm = TRUE)
  l2_means <- colMeans(l2_mat, na.rm = TRUE)
  l1_sds <- apply(l1_mat, 2, sd, na.rm = TRUE)
  l2_sds <- apply(l2_mat, 2, sd, na.rm = TRUE)
  .w_double(con, l1_means); .w_double(con, l2_means)
  .w_double(con, l1_sds);   .w_double(con, l2_sds)
  # L2 IDs
  if (ids_numeric) .w_double(con, as.double(l2[[l2_id]]))
  else for (id in as.character(l2[[l2_id]])) .w_raw(con, .pad13(id))
  # L2 row matrix
  for (i in seq_len(nrow(l2))) .w_double(con, l2_mat[i, ])
  # Per-L2 blocks (HLM2: only nrows + L1_means + L1_rows — no IDs, no L2 row)
  for (i in seq_len(nrow(l2))) {
    uk <- l2_key[i]
    idx <- l1_split[[uk]]; nrows <- length(idx)
    blk <- l1_mat[idx, , drop = FALSE]
    .w_int32(con, nrows)
    .w_double(con, colMeans(blk, na.rm = TRUE))
    for (r in seq_len(nrows)) .w_double(con, blk[r, ])
  }
  invisible(mdm_path)
}

# ============================================================================
# HLM4 — per re/notes/08_mdm_hlm4.md
# ============================================================================
hlm_write_mdm4 <- function(l1, l2, l3, l4,
                           l4_id, l3_id, l2_id,
                           l1_vars, l2_vars, l3_vars, l4_vars,
                           mdm_path,
                           l1_sav_win = "", l2_sav_win = "",
                           l3_sav_win = "", l4_sav_win = "",
                           l1_missing = TRUE, listwise_delete = FALSE) {
  l1_mat <- .as_dmat(l1, l1_vars); l2_mat <- .as_dmat(l2, l2_vars)
  l3_mat <- .as_dmat(l3, l3_vars); l4_mat <- .as_dmat(l4, l4_vars)
  l1_key <- paste0(as.character(l1[[l4_id]]), "\x01",
                   as.character(l1[[l3_id]]), "\x01",
                   as.character(l1[[l2_id]]))
  l2_key <- paste0(as.character(l2[[l4_id]]), "\x01",
                   as.character(l2[[l3_id]]), "\x01",
                   as.character(l2[[l2_id]]))
  ids_numeric <- is.numeric(l1[[l4_id]]) && is.numeric(l1[[l3_id]]) && is.numeric(l1[[l2_id]])
  l1_split <- split(seq_len(nrow(l1)), l1_key)
  l2_units <- unique(l2_key)
  internal_mdmtype <- 6L
  signed_mdmtype <- if (ids_numeric) -internal_mdmtype else internal_mdmtype
  # L3 → L4 parent mapping
  l3_parent_l4 <- l3[[l4_id]]

  con <- file(mdm_path, "wb"); on.exit(close(con))
  # Header — HLM4 version is DOUBLE (8 bytes), not float32!
  .w_raw(con, "SSHLM4")
  .w_double(con, 8.2)  # 8 bytes for HLM4
  .w_int32(con, length(l1_vars)); .w_int32(con, length(l2_vars))
  .w_int32(con, length(l3_vars)); .w_int32(con, length(l4_vars))
  .w_int32(con, nrow(l1)); .w_int32(con, length(l2_units))
  .w_int32(con, nrow(l3)); .w_int32(con, nrow(l4))
  .w_int32(con, as.integer(l1_missing)); .w_int32(con, as.integer(listwise_delete))
  .w_int32(con, 0L)  # outcome_family
  .w_int32(con, signed_mdmtype)
  .w_int32(con, max(vapply(l1_split, length, integer(1))))
  # Var names (4 tables)
  for (v in c("INTRCPT1", l1_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT2", l2_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT3", l3_vars)) .w_raw(con, .pad13(v))
  for (v in c("INTRCPT4", l4_vars)) .w_raw(con, .pad13(v))
  # Paths (4)
  .w_int32(con, 260L)
  .w_path(con, l1_sav_win); .w_path(con, l2_sav_win)
  .w_path(con, l3_sav_win); .w_path(con, l4_sav_win)
  # Grand means (4 levels)
  .w_double(con, colMeans(l1_mat, na.rm = TRUE))
  .w_double(con, colMeans(l2_mat, na.rm = TRUE))
  .w_double(con, colMeans(l3_mat, na.rm = TRUE))
  .w_double(con, colMeans(l4_mat, na.rm = TRUE))
  # L3 IDs
  if (ids_numeric) .w_double(con, as.double(l3[[l3_id]]))
  else for (id in as.character(l3[[l3_id]])) .w_raw(con, .pad13(id))
  # L3 → L4 parent IDs (NEW in HLM4)
  if (ids_numeric) .w_double(con, as.double(l3_parent_l4))
  else for (id in as.character(l3_parent_l4)) .w_raw(con, .pad13(id))
  # L4 IDs
  if (ids_numeric) .w_double(con, as.double(l4[[l4_id]]))
  else for (id in as.character(l4[[l4_id]])) .w_raw(con, .pad13(id))
  # L3 row matrix
  for (i in seq_len(nrow(l3))) .w_double(con, l3_mat[i, ])
  # L4 row matrix
  for (i in seq_len(nrow(l4))) .w_double(con, l4_mat[i, ])
  # Per-L2 blocks (same structure as HLM3 but IDs are L4,L3 instead of just L3)
  for (uk in l2_units) {
    idx <- l1_split[[uk]]; nrows <- length(idx)
    l2i <- match(uk, l2_key)
    blk <- l1_mat[idx, , drop = FALSE]
    .w_int32(con, nrows)
    .w_double(con, colMeans(blk, na.rm = TRUE))
    for (r in seq_len(nrows)) .w_double(con, blk[r, ])
    if (ids_numeric) {
      .w_double(con, as.double(l2[[l4_id]][l2i]))
      .w_double(con, as.double(l2[[l3_id]][l2i]))
      .w_double(con, as.double(l2[[l2_id]][l2i]))
    } else {
      .w_raw(con, .pad13(as.character(l2[[l4_id]][l2i])))
      .w_raw(con, .pad13(as.character(l2[[l3_id]][l2i])))
      .w_raw(con, .pad13(as.character(l2[[l2_id]][l2i])))
    }
    .w_double(con, l2_mat[l2i, ])
  }
  invisible(mdm_path)
}
