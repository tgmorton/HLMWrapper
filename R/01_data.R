# =============================================================================
# Input data preparation: sort fix + write .sav into a workspace.
# =============================================================================
#
# This module bakes in the most important guarantee of the wrapper:
# **level-1 data is always sorted by (level3id, level2id) before being
# handed to HLM.** Without this, HLM3 silently drops level-2 units (the
# bug we documented in re/notes/01_bug_report.md).

#' Validate and sort a level-1 data.frame for HLM.
#'
#' @param df         a data.frame
#' @param l3_id      column name of the level-3 identifier (e.g. "PID")
#' @param l2_id      column name of the level-2 identifier; NULL for 2-level data
#' @param tiebreaker optional column to break ties within a (l3,l2) group
#'                   so the row order is deterministic
#'
#' @return a tibble sorted by (l3_id, l2_id, tiebreaker), with attributes
#'   `original_rows` and `was_sorted` indicating whether the input was already
#'   in the right order.
hlm_sort_l1 <- function(df, l3_id, l2_id = NULL, tiebreaker = NULL) {
  stopifnot(is.data.frame(df), is.character(l3_id), length(l3_id) == 1L)
  if (!l3_id %in% names(df))
    stop("level-3 id column '", l3_id, "' not found in data; have: ",
         paste(names(df), collapse = ", "))
  cols <- l3_id
  if (!is.null(l2_id)) {
    if (!l2_id %in% names(df)) stop("level-2 id column '", l2_id, "' not found")
    cols <- c(cols, l2_id)
  }
  if (!is.null(tiebreaker) && tiebreaker %in% names(df)) cols <- c(cols, tiebreaker)
  ord <- do.call(order, lapply(cols, function(k) df[[k]]))
  was_sorted <- identical(ord, seq_len(nrow(df)))
  out <- df[ord, , drop = FALSE]
  attr(out, "original_rows") <- nrow(df)
  attr(out, "was_sorted")    <- was_sorted
  attr(out, "sorted_by")     <- cols
  out
}

#' Validate level-2 / level-3 data: check ID column exists and uniqueness.
hlm_check_higher <- function(df, id_cols, level) {
  for (k in id_cols) {
    if (!k %in% names(df))
      stop("level-", level, " id column '", k, "' not found")
  }
  key <- if (length(id_cols) == 1L) df[[id_cols]]
         else interaction(df[id_cols], drop = TRUE)
  if (anyDuplicated(key) > 0) {
    dup <- which(duplicated(key))[seq_len(min(5, sum(duplicated(key))))]
    stop("level-", level, " file has duplicate keys (",
         paste(id_cols, collapse = ","),
         ") at rows: ", paste(dup, collapse = ", "))
  }
  invisible(TRUE)
}

#' Cross-check that every (l3,l2) key in level-1 exists in level-2,
#' and every l3 in level-2 exists in level-3.
hlm_check_keys <- function(l1, l2, l3, l3_id, l2_id) {
  k1 <- paste0(as.character(l1[[l3_id]]), "\u0001",
               as.character(l1[[l2_id]]))
  k2 <- paste0(as.character(l2[[l3_id]]), "\u0001",
               as.character(l2[[l2_id]]))
  miss <- setdiff(unique(k1), k2)
  if (length(miss))
    warning("level-1 has ", length(miss),
            " (l3,l2) key(s) absent from level-2; first: ",
            paste(head(miss, 3), collapse = " | "))
  miss3 <- setdiff(unique(as.character(l2[[l3_id]])), as.character(l3[[l3_id]]))
  if (length(miss3))
    warning("level-2 has ", length(miss3),
            " l3 id(s) absent from level-3; first: ",
            paste(head(miss3, 3), collapse = " | "))
  invisible(TRUE)
}

#' Truncate variable names to HLM's 8-character limit and detect collisions.
#'
#' HLM/Stat-Transfer silently truncates names > 8 chars when reading .sav.
#' We do the truncation explicitly so we can: (a) list the truncated names
#' in the .mdmt where HLM expects them; (b) error loudly on collisions
#' (which would otherwise silently merge two columns into one).
#'
#' @return a named character vector: names = original, values = truncated.
hlm_truncate_names <- function(names) {
  trunc <- substr(names, 1L, 8L)
  dup <- trunc[duplicated(trunc)]
  if (length(dup)) {
    pairs <- vapply(unique(dup), function(d) {
      origs <- names[trunc == d]
      paste0(d, " <- ", paste(origs, collapse = " + "))
    }, character(1))
    stop("variable name collisions after HLM 8-char truncation:\n  ",
         paste(pairs, collapse = "\n  "),
         "\nRename these in your source data before passing to hlmwrap.")
  }
  setNames(trunc, names)
}

#' Polymorphic data loader.  Accepts:
#'   - a data.frame / tibble (returned as-is)
#'   - a path to a .csv file (read with readr::read_csv if available, else read.csv)
#'   - a path to a .tsv / .txt file (auto-detected delimiter)
#'   - a path to a .sav file (read with haven::read_sav)
#'   - a path to a .dta file (read with haven::read_dta)
#'   - a path to a .xlsx file (if readxl available)
#'
#' @return a tibble
hlm_load <- function(x) {
  if (is.data.frame(x)) return(tibble::as_tibble(x))
  if (!is.character(x) || length(x) != 1L)
    stop("hlm_load() expects a data.frame or a single file path")
  if (!file.exists(x)) stop("file not found: ", x)
  ext <- tolower(tools::file_ext(x))
  switch(ext,
    "csv"  = if (requireNamespace("readr",  quietly = TRUE))
               tibble::as_tibble(readr::read_csv(x, show_col_types = FALSE))
             else tibble::as_tibble(read.csv(x, stringsAsFactors = FALSE)),
    "tsv"  = if (requireNamespace("readr",  quietly = TRUE))
               tibble::as_tibble(readr::read_tsv(x, show_col_types = FALSE))
             else tibble::as_tibble(read.delim(x, stringsAsFactors = FALSE)),
    "txt"  = tibble::as_tibble(read.delim(x, stringsAsFactors = FALSE)),
    "sav"  = tibble::as_tibble(haven::read_sav(x)),
    "dta"  = tibble::as_tibble(haven::read_dta(x)),
    "xlsx" = if (requireNamespace("readxl", quietly = TRUE))
               tibble::as_tibble(readxl::read_excel(x))
             else stop("install 'readxl' to read xlsx files"),
    stop("unsupported file extension: .", ext,
         " (supported: csv, tsv, txt, sav, dta, xlsx)")
  )
}

#' Write a tibble to .sav inside the workspace and return the win path.
#'
#' CRITICAL: writes UNCOMPRESSED .sav (`compress = "none"`). HLM 8.2 / its
#' embedded Stat/Transfer reader silently mis-reads byte-compressed (`$FL2`
#' compress=1) SPSS files: it loads only the first decompression block and
#' reports N=1 for every variable. Uncompressed files work correctly.
#' Discovered empirically by comparing a working pyreadstat-written .sav
#' against a haven-defaults file.
hlm_write_sav <- function(df, ws, basename) {
  stopifnot(inherits(ws, "hlm_workspace"))
  if (!grepl("\\.sav$", basename, ignore.case = TRUE))
    basename <- paste0(basename, ".sav")
  mac_path <- file.path(ws$mac, basename)
  haven::write_sav(df, mac_path, compress = "none")
  list(
    mac = mac_path,
    win = paste0(ws$win, "\\", basename),
    rows = nrow(df),
    cols = ncol(df)
  )
}
