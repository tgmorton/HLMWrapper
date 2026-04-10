# =============================================================================
# .mdmt template generation and "Make MDM" (hlm3.exe -w -r) invocation.
# =============================================================================

#' Build the text contents of an HLM3 .mdmt template file.
#'
#' Mirrors the format produced by whlm.exe (verified empirically and via RE).
#'
#' @param l1_win    Win path to the level-1 .sav file
#' @param l2_win    Win path to the level-2 .sav file
#' @param l3_win    Win path to the level-3 .sav file
#' @param l3_id     name of the level-3 ID variable (case-insensitive)
#' @param l2_id     name of the level-2 ID variable
#' @param l1_vars   character vector of level-1 variable names (excluding IDs)
#' @param l2_vars   character vector of level-2 variable names (excluding IDs)
#' @param l3_vars   character vector of level-3 variable names (excluding L3 ID)
#' @param mdm_name  short name for the .mdm file (no extension)
#' @param l1_missing  TRUE/FALSE whether to allow level-1 missing
#' @param time_of_deletion "analysis" or "make_mdm"
#'
#' @return a character scalar containing the .mdmt file body
hlm_build_mdmt_text <- function(l1_win, l2_win, l3_win,
                                l3_id, l2_id,
                                l1_vars, l2_vars, l3_vars,
                                mdm_name,
                                l1_missing = TRUE,
                                time_of_deletion = c("analysis","make_mdm")) {
  time_of_deletion <- match.arg(time_of_deletion)
  lines <- c(
    "#HLM3 MDM CREATION TEMPLATE",
    "mdmtype:0",
    "rawdattype:spss",
    paste0("l1fname:", l1_win),
    paste0("l2fname:", l2_win),
    paste0("l3fname:", l3_win),
    paste0("l1missing:", if (l1_missing) "y" else "n"),
    paste0("timeofdeletion:", time_of_deletion),
    paste0("mdmname:", mdm_name),
    "*begin l1vars",
    paste0("level3id:", l3_id),
    paste0("level2id:", l2_id),
    l1_vars,
    "*end l1vars",
    "*begin l2vars",
    paste0("level3id:", l3_id),
    paste0("level2id:", l2_id),
    l2_vars,
    "*end l2vars",
    "*begin l3vars",
    paste0("level3id:", l3_id),
    l3_vars,
    "*end l3vars"
  )
  paste(lines, collapse = "\n")
}

#' Write a .mdmt file into a workspace and return the paths.
hlm_write_mdmt <- function(text, ws, basename = "model") {
  stopifnot(inherits(ws, "hlm_workspace"))
  if (!grepl("\\.mdmt$", basename, ignore.case = TRUE))
    basename <- paste0(basename, ".mdmt")
  mac_path <- file.path(ws$mac, basename)
  writeLines(text, mac_path, sep = "\n")
  list(mac = mac_path, win = paste0(ws$win, "\\", basename))
}

#' Run hlm3.exe -w -r <mdmt> to build the binary .mdm.
#' Returns the path to the produced .mdm and any messages.
hlm_make_mdm <- function(mdmt, ws, solver = "hlm3.exe") {
  stopifnot(inherits(ws, "hlm_workspace"))
  res <- hlm_run(solver,
                 args = c("-w", "-r", mdmt$win),
                 cwd  = ws$mac,
                 timeout = 300L,
                 need_whlm = TRUE)   # critical: -w hangs cold without whlm alive
  # The .mdm name comes from the mdmname: directive in the .mdmt
  txt <- readLines(mdmt$mac, warn = FALSE)
  mdm_name <- sub("^mdmname:", "",
                  grep("^mdmname:", txt, value = TRUE)[1])
  mdm_path <- file.path(ws$mac, mdm_name)
  sts_pat <- file.path(ws$mac,
    c("HLM2MDM.STS","HLM3MDM.STS","HLM4MDM.STS",
      "HCM2MDM.STS","HCM3MDM.STS","HMLMMDM.STS",
      "HMLM2MDM.STS","HLMHCMMDM.STS"))
  sts_path <- sts_pat[file.exists(sts_pat)][1]
  list(
    mdm        = mdm_path,
    mdm_win    = paste0(ws$win, "\\", basename(mdm_path)),
    sts        = if (length(sts_path)) sts_path else NA_character_,
    exit       = res$status,
    duration   = res$duration,
    stderr     = res$stderr,
    success    = file.exists(mdm_path)
  )
}
