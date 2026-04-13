# =============================================================================
# Top-level orchestration: build_mdm() + fit() in user-friendly wrappers.
# =============================================================================

# HLM solver exit codes — discovered empirically from RE / runs.
# 0 = success
# 100 = file open error (from FUN_0044c9c0(100,...) inside hlm3.exe parser)
# 136 = near-singularity in fixed effects (graceful failure, results still written)
# others = unknown
.hlm_exit_meaning <- function(code) {
  switch(as.character(code),
    "0"    = "success",
    "100"  = "file_error",
    "136"  = "near_singularity",
    paste0("unknown_", code)
  )
}

#' Build a 3-level MDM from data sources of any kind.
#'
#' This is the **agent-facing entry point** for data prep. Accepts
#' data.frames, .csv, .sav, .dta, .xlsx — anything `hlm_load()` understands.
#' Sorts the level-1 file by (l3_id, l2_id) automatically (the bug fix).
#'
#' @param level1,level2,level3  data sources (df or file paths)
#' @param l3_id,l2_id           ID column names (e.g. "PID", "cond")
#' @param vars_l1,vars_l2,vars_l3  character vectors of variable names to
#'                                 INCLUDE (excluding the ID columns).
#'                                 If NULL, all non-ID columns are included.
#' @param workspace             a workspace object from hlm_workspace(), or
#'                              a string name (auto-creates with clean=TRUE)
#' @param mdm_name              short name for the .mdm file (no extension)
#' @param l1_missing            TRUE to allow level-1 missing data
#'
#' @return list of class "hlm_mdm"
hlm_build_mdm3 <- function(level1, level2, level3,
                           l3_id, l2_id,
                           vars_l1 = NULL, vars_l2 = NULL, vars_l3 = NULL,
                           workspace = "default",
                           mdm_name = "model",
                           l1_missing = TRUE,
                           method = c("direct", "wine")) {
  method <- match.arg(method)

  # Workspace
  ws <- if (inherits(workspace, "hlm_workspace")) workspace
        else hlm_workspace(workspace, clean = TRUE)

  # Load + validate
  l1 <- hlm_load(level1)
  l2 <- hlm_load(level2)
  l3 <- hlm_load(level3)

  # Default variable selection: all columns minus IDs
  if (is.null(vars_l1)) vars_l1 <- setdiff(names(l1), c(l3_id, l2_id))
  if (is.null(vars_l2)) vars_l2 <- setdiff(names(l2), c(l3_id, l2_id))
  if (is.null(vars_l3)) vars_l3 <- setdiff(names(l3), c(l3_id))

  # Validate that requested variables exist
  bad <- setdiff(vars_l1, names(l1))
  if (length(bad)) stop("level-1 missing columns: ", paste(bad, collapse=", "))
  bad <- setdiff(vars_l2, names(l2))
  if (length(bad)) stop("level-2 missing columns: ", paste(bad, collapse=", "))
  bad <- setdiff(vars_l3, names(l3))
  if (length(bad)) stop("level-3 missing columns: ", paste(bad, collapse=", "))

  hlm_check_higher(l3, l3_id, level = 3L)
  hlm_check_higher(l2, c(l3_id, l2_id), level = 2L)
  hlm_check_keys(l1, l2, l3, l3_id, l2_id)

  # Apply the sort fix
  l1_sorted <- hlm_sort_l1(l1, l3_id = l3_id, l2_id = l2_id)
  if (!attr(l1_sorted, "was_sorted"))
    message("hlmwrap: level-1 was not sorted by (", l3_id, ",", l2_id,
            "); sorted automatically (bypasses HLM3 silent-drop bug).")

  # IMPORTANT: empirically, HLM mis-reads .sav files when the .mdmt lists
  # only a subset of columns. The MDM build degenerates to N=1. The
  # GUI-generated .mdmt always lists ALL columns of each source file in
  # source order, so we do the same. The vars_l{1,2,3} parameters become
  # an INCLUSIVE filter (intersected with source columns), and the final
  # .sav contains exactly those columns in source order; the .mdmt lists
  # them in the same order.

  # Critical: HLM reads .sav columns POSITIONALLY (not by name), so the
  # .mdmt's *begin l1vars block must list the analysis variables in the
  # SAME order they appear in the .sav file. We therefore preserve the
  # source data.frame's column order in both the .sav we write and the
  # .mdmt list. ID columns are extracted BY NAME via level3id:/level2id:
  # so they don't need to be moved to a particular position.
  pick <- function(df, ids, requested) {
    cols_in_source_order <- setdiff(names(df), ids)
    if (!is.null(requested)) {
      bad <- setdiff(requested, names(df))
      if (length(bad)) stop("requested var(s) not in source: ",
                            paste(bad, collapse = ", "))
      cols_in_source_order <- intersect(cols_in_source_order, requested)
    }
    cols_in_source_order
  }
  v1 <- pick(l1_sorted, c(l3_id, l2_id), vars_l1)
  v2 <- pick(l2,        c(l3_id, l2_id), vars_l2)
  v3 <- pick(l3,        c(l3_id),        vars_l3)

  # HLM 8-char limit + collision detection. Apply on the FULL set of
  # columns we're about to write (analysis vars + IDs).
  l1_full <- c(intersect(names(l1_sorted), c(l3_id, l2_id, v1)))
  l2_full <- c(intersect(names(l2),        c(l3_id, l2_id, v2)))
  l3_full <- c(intersect(names(l3),        c(l3_id, v3)))
  l1_map <- hlm_truncate_names(l1_full)
  l2_map <- hlm_truncate_names(l2_full)
  l3_map <- hlm_truncate_names(l3_full)

  # Subset BUT preserve original column order
  l1_out <- l1_sorted[, l1_full, drop = FALSE]
  l2_out <- l2[,        l2_full, drop = FALSE]
  l3_out <- l3[,        l3_full, drop = FALSE]
  names(l1_out) <- l1_map[names(l1_out)]
  names(l2_out) <- l2_map[names(l2_out)]
  names(l3_out) <- l3_map[names(l3_out)]
  # IDs and var lists in truncated form, in source order
  l3_id_t  <- unname(l1_map[l3_id])
  l2_id_t  <- unname(l1_map[l2_id])
  v1 <- unname(l1_map[v1])
  v2 <- unname(l2_map[v2])
  v3 <- unname(l3_map[v3])

  # Write .sav into the workspace
  l1_p <- hlm_write_sav(l1_out, ws, "level1.sav")
  l2_p <- hlm_write_sav(l2_out, ws, "level2.sav")
  l3_p <- hlm_write_sav(l3_out, ws, "level3.sav")

  # Update vars_l{1,2,3} to the canonical (source-ordered, validated) lists
  vars_l1 <- v1; vars_l2 <- v2; vars_l3 <- v3

  mdm_path <- file.path(ws$mac, mdm_name)
  sts_path <- file.path(ws$mac, "HLM3MDM.STS")

  if (method == "direct") {
    # R-native .mdm writer — no Wine needed, no whlm.exe, no .sav quirks.
    # Byte-identical to hlm3.exe -w output (verified in development).
    hlm_write_mdm3(l1_out, l2_out, l3_out,
                   l3_id = l3_id_t, l2_id = l2_id_t,
                   l1_vars = vars_l1, l2_vars = vars_l2, l3_vars = vars_l3,
                   mdm_path = mdm_path,
                   l1_sav_win = l1_p$win, l2_sav_win = l2_p$win,
                   l3_sav_win = l3_p$win,
                   l1_missing = l1_missing)
    if (!file.exists(mdm_path))
      stop("R-native .mdm writer failed to create ", mdm_path)
  } else {
    # Legacy: write .mdmt + invoke hlm3.exe -w via Wine
    mdmt_text <- hlm_build_mdmt_text(
      l1_win = l1_p$win, l2_win = l2_p$win, l3_win = l3_p$win,
      l3_id = l3_id_t, l2_id = l2_id_t,
      l1_vars = vars_l1, l2_vars = vars_l2, l3_vars = vars_l3,
      mdm_name = mdm_name, l1_missing = l1_missing
    )
    mdmt <- hlm_write_mdmt(mdmt_text, ws, mdm_name)
    build <- hlm_make_mdm(mdmt, ws, solver = "hlm3.exe")
    if (!build$success)
      stop("hlm3.exe -w failed (exit ", build$exit, "; ",
           .hlm_exit_meaning(build$exit), ")\nstderr: ", build$stderr)
    sts_path <- build$sts
  }

  structure(list(
    workspace = ws,
    level     = 3L,
    method    = method,
    mdm       = mdm_path,
    mdm_win   = hlm_to_win_path(mdm_path),
    sts       = if (file.exists(sts_path)) sts_path else NA_character_,
    l1_path = l1_p, l2_path = l2_p, l3_path = l3_p,
    l3_id = l3_id, l2_id = l2_id,
    vars_l1 = vars_l1, vars_l2 = vars_l2, vars_l3 = vars_l3,
    rows = list(l1 = nrow(l1_out), l2 = nrow(l2_out), l3 = nrow(l3_out))
  ), class = c("hlm_mdm", "hlm_mdm3"))
}

#' Build a 2-level MDM. Same as hlm_build_mdm3 but no level-3 file.
hlm_build_mdm2 <- function(level1, level2,
                           l2_id,
                           vars_l1 = NULL, vars_l2 = NULL,
                           workspace = "default",
                           mdm_name = "model",
                           l1_missing = TRUE,
                           method = c("direct", "wine")) {
  method <- match.arg(method)
  ws <- if (inherits(workspace, "hlm_workspace")) workspace
        else hlm_workspace(workspace, clean = TRUE)
  l1 <- hlm_load(level1)
  l2 <- hlm_load(level2)
  pick2 <- function(df, ids, requested) {
    cols <- setdiff(names(df), ids)
    if (!is.null(requested)) {
      bad <- setdiff(requested, names(df))
      if (length(bad)) stop("requested var(s) not in source: ",
                            paste(bad, collapse = ", "))
      cols <- intersect(cols, requested)
    }
    cols
  }
  if (is.null(vars_l1)) vars_l1 <- pick2(l1, l2_id, NULL)
  if (is.null(vars_l2)) vars_l2 <- pick2(l2, l2_id, NULL)
  hlm_check_higher(l2, l2_id, level = 2L)
  l1_sorted <- hlm_sort_l1(l1, l3_id = l2_id, l2_id = NULL)
  if (!attr(l1_sorted, "was_sorted"))
    message("hlmwrap: level-1 was not sorted by ", l2_id, "; sorted automatically.")

  # Preserve source column order + truncate names
  l1_full <- intersect(names(l1_sorted), c(l2_id, vars_l1))
  l2_full <- intersect(names(l2),        c(l2_id, vars_l2))
  l1_map <- hlm_truncate_names(l1_full)
  l2_map <- hlm_truncate_names(l2_full)
  l1_out <- l1_sorted[, l1_full, drop = FALSE]
  l2_out <- l2[,        l2_full, drop = FALSE]
  names(l1_out) <- l1_map[names(l1_out)]
  names(l2_out) <- l2_map[names(l2_out)]
  l2_id_t <- unname(l1_map[l2_id])
  vars_l1 <- unname(l1_map[vars_l1])
  vars_l2 <- unname(l2_map[vars_l2])

  l1_p <- hlm_write_sav(l1_out, ws, "level1.sav")
  l2_p <- hlm_write_sav(l2_out, ws, "level2.sav")

  mdm_path <- file.path(ws$mac, mdm_name)
  sts_path <- file.path(ws$mac, "HLM2MDM.STS")

  if (method == "direct") {
    hlm_write_mdm2(l1_out, l2_out,
                   l2_id = l2_id_t,
                   l1_vars = vars_l1, l2_vars = vars_l2,
                   mdm_path = mdm_path,
                   l1_sav_win = l1_p$win, l2_sav_win = l2_p$win,
                   l1_missing = l1_missing)
    if (!file.exists(mdm_path))
      stop("R-native HLM2 .mdm writer failed")
  } else {
    mdmt_text <- paste(c(
      "#HLM2 MDM CREATION TEMPLATE",
      "mdmtype:0", "rawdattype:spss",
      paste0("l1fname:", l1_p$win), paste0("l2fname:", l2_p$win),
      paste0("l1missing:", if (l1_missing) "y" else "n"),
      "timeofdeletion:analysis", paste0("mdmname:", mdm_name),
      "*begin l1vars", paste0("level2id:", l2_id_t), vars_l1, "*end l1vars",
      "*begin l2vars", paste0("level2id:", l2_id_t), vars_l2, "*end l2vars"
    ), collapse = "\n")
    mdmt <- hlm_write_mdmt(mdmt_text, ws, mdm_name)
    build <- hlm_make_mdm(mdmt, ws, solver = "hlm2.exe")
    if (!build$success)
      stop("hlm2.exe -w failed (exit ", build$exit, ")")
    sts_path <- build$sts
  }

  structure(list(
    workspace = ws, level = 2L, method = method,
    mdm = mdm_path, mdm_win = hlm_to_win_path(mdm_path),
    sts = if (file.exists(sts_path)) sts_path else NA_character_,
    l1_path = l1_p, l2_path = l2_p,
    l2_id = l2_id,
    vars_l1 = vars_l1, vars_l2 = vars_l2,
    rows = list(l1 = nrow(l1_out), l2 = nrow(l2_out))
  ), class = c("hlm_mdm", "hlm_mdm2"))
}

#' Build a 4-level MDM. Extends hlm_build_mdm3 with a level-4 file.
hlm_build_mdm4 <- function(level1, level2, level3, level4,
                           l4_id, l3_id, l2_id,
                           vars_l1 = NULL, vars_l2 = NULL,
                           vars_l3 = NULL, vars_l4 = NULL,
                           workspace = "default",
                           mdm_name = "model",
                           l1_missing = TRUE,
                           method = c("direct", "wine")) {
  method <- match.arg(method)
  ws <- if (inherits(workspace, "hlm_workspace")) workspace
        else hlm_workspace(workspace, clean = TRUE)
  l1 <- hlm_load(level1)
  l2 <- hlm_load(level2)
  l3 <- hlm_load(level3)
  l4 <- hlm_load(level4)

  pick <- function(df, ids, requested) {
    cols <- setdiff(names(df), ids)
    if (!is.null(requested)) {
      bad <- setdiff(requested, names(df))
      if (length(bad)) stop("requested var(s) not in source: ",
                            paste(bad, collapse = ", "))
      cols <- intersect(cols, requested)
    }
    cols
  }
  v1 <- pick(l1, c(l4_id, l3_id, l2_id), vars_l1)
  v2 <- pick(l2, c(l4_id, l3_id, l2_id), vars_l2)
  v3 <- pick(l3, c(l4_id, l3_id),         vars_l3)
  v4 <- pick(l4, c(l4_id),                vars_l4)

  l1_full <- intersect(names(l1), c(l4_id, l3_id, l2_id, v1))
  l2_full <- intersect(names(l2), c(l4_id, l3_id, l2_id, v2))
  l3_full <- intersect(names(l3), c(l4_id, l3_id, v3))
  l4_full <- intersect(names(l4), c(l4_id, v4))
  l1_map <- hlm_truncate_names(l1_full)
  l2_map <- hlm_truncate_names(l2_full)
  l3_map <- hlm_truncate_names(l3_full)
  l4_map <- hlm_truncate_names(l4_full)

  hlm_check_higher(l4, l4_id, level = 4L)
  hlm_check_higher(l3, c(l4_id, l3_id), level = 3L)
  hlm_check_higher(l2, c(l4_id, l3_id, l2_id), level = 2L)

  l1_sorted <- hlm_sort_l1(l1, l3_id = l4_id, l2_id = l3_id)
  if (!attr(l1_sorted, "was_sorted"))
    message("hlmwrap: level-1 sorted automatically.")

  l1_out <- l1_sorted[, l1_full, drop = FALSE]
  l2_out <- l2[,        l2_full, drop = FALSE]
  l3_out <- l3[,        l3_full, drop = FALSE]
  l4_out <- l4[,        l4_full, drop = FALSE]
  names(l1_out) <- l1_map[names(l1_out)]
  names(l2_out) <- l2_map[names(l2_out)]
  names(l3_out) <- l3_map[names(l3_out)]
  names(l4_out) <- l4_map[names(l4_out)]
  l4_id_t <- unname(l1_map[l4_id])
  l3_id_t <- unname(l1_map[l3_id])
  l2_id_t <- unname(l1_map[l2_id])
  v1 <- unname(l1_map[v1])
  v2 <- unname(l2_map[v2])
  v3 <- unname(l3_map[v3])
  v4 <- unname(l4_map[v4])

  l1_p <- hlm_write_sav(l1_out, ws, "level1.sav")
  l2_p <- hlm_write_sav(l2_out, ws, "level2.sav")
  l3_p <- hlm_write_sav(l3_out, ws, "level3.sav")
  l4_p <- hlm_write_sav(l4_out, ws, "level4.sav")

  mdm_path <- file.path(ws$mac, mdm_name)
  sts_path <- file.path(ws$mac, "HLM4MDM.STS")

  if (method == "direct") {
    hlm_write_mdm4(l1_out, l2_out, l3_out, l4_out,
                   l4_id = l4_id_t, l3_id = l3_id_t, l2_id = l2_id_t,
                   l1_vars = v1, l2_vars = v2, l3_vars = v3, l4_vars = v4,
                   mdm_path = mdm_path,
                   l1_sav_win = l1_p$win, l2_sav_win = l2_p$win,
                   l3_sav_win = l3_p$win, l4_sav_win = l4_p$win,
                   l1_missing = l1_missing)
    if (!file.exists(mdm_path))
      stop("R-native HLM4 .mdm writer failed")
  } else {
    mdmt_text <- paste(c(
      "#HLM4 MDM CREATION TEMPLATE", "mdmtype:0", "rawdattype:spss",
      paste0("l1fname:", l1_p$win), paste0("l2fname:", l2_p$win),
      paste0("l3fname:", l3_p$win), paste0("l4fname:", l4_p$win),
      paste0("l1missing:", if (l1_missing) "y" else "n"),
      "timeofdeletion:analysis", paste0("mdmname:", mdm_name),
      "*begin l1vars", paste0("level4id:", l4_id_t),
      paste0("level3id:", l3_id_t), paste0("level2id:", l2_id_t),
      v1, "*end l1vars",
      "*begin l2vars", paste0("level4id:", l4_id_t),
      paste0("level3id:", l3_id_t), paste0("level2id:", l2_id_t),
      v2, "*end l2vars",
      "*begin l3vars", paste0("level4id:", l4_id_t),
      paste0("level3id:", l3_id_t), v3, "*end l3vars",
      "*begin l4vars", paste0("level4id:", l4_id_t), v4, "*end l4vars"
    ), collapse = "\n")
    mdmt <- hlm_write_mdmt(mdmt_text, ws, mdm_name)
    build <- hlm_make_mdm(mdmt, ws, solver = "hlm4.exe")
    if (!build$success) stop("hlm4.exe -w failed (exit ", build$exit, ")")
    sts_path <- build$sts
  }

  structure(list(
    workspace = ws, level = 4L, method = method,
    mdm = mdm_path, mdm_win = hlm_to_win_path(mdm_path),
    sts = if (file.exists(sts_path)) sts_path else NA_character_,
    l1_path = l1_p, l2_path = l2_p, l3_path = l3_p, l4_path = l4_p,
    l4_id = l4_id, l3_id = l3_id, l2_id = l2_id,
    vars_l1 = v1, vars_l2 = v2, vars_l3 = v3, vars_l4 = v4,
    rows = list(l1 = nrow(l1_out), l2 = nrow(l2_out),
                l3 = nrow(l3_out), l4 = nrow(l4_out))
  ), class = c("hlm_mdm", "hlm_mdm4"))
}

#' Fit a model. Dispatches on the spec class.
#'
#' @param mdm   an hlm_mdm built by hlm_build_mdm2/3
#' @param spec  an hlm2_spec or hlm3_spec
#' @param model_name basename for the .hlm and .html files
#' @param timeout    seconds before killing the solver
#'
#' @return an hlm_result list (parsed from HTML)
hlm_fit <- function(mdm, spec, model_name = "fit", timeout = 600L) {
  stopifnot(inherits(mdm, "hlm_mdm"))
  ws <- mdm$workspace
  is4 <- inherits(spec, "hlm4_spec")
  is3 <- inherits(spec, "hlm3_spec")
  is2 <- inherits(spec, "hlm2_spec")
  if (!(is2 || is3 || is4))
    stop("spec must be an hlm2_spec, hlm3_spec, or hlm4_spec")
  if (is4 && mdm$level != 4L) stop("hlm4_spec requires a 4-level MDM")
  if (is3 && mdm$level != 3L) stop("hlm3_spec requires a 3-level MDM")
  if (is2 && mdm$level != 2L) stop("hlm2_spec requires a 2-level MDM")

  graph_win <- paste0(ws$win, "\\", model_name, "_graph.geq")
  out_win   <- paste0(ws$win, "\\", model_name, ".html")
  out_mac   <- file.path(ws$mac, paste0(model_name, ".html"))
  if (file.exists(out_mac)) file.remove(out_mac)

  text <- if (is4) hlm_render_hlm4(spec, basename(mdm$mdm), graph_win, out_win)
          else if (is3) hlm_render_hlm3(spec, basename(mdm$mdm), graph_win, out_win)
          else          hlm_render_hlm2(spec, basename(mdm$mdm), graph_win, out_win)

  hlm_file <- hlm_write_hlm(text, ws, model_name)

  solver <- if (is4) "hlm4.exe" else if (is3) "hlm3.exe" else "hlm2.exe"
  res <- hlm_run(solver,
                 args = c("-nowait", basename(mdm$mdm), basename(hlm_file$mac)),
                 cwd  = ws$mac,
                 timeout = timeout)

  exit_meaning <- .hlm_exit_meaning(res$status)
  produced_html <- file.exists(out_mac)

  parsed <- if (produced_html) hlm_parse_html(out_mac) else list()

  structure(list(
    spec = spec,
    mdm  = mdm,
    workspace = ws,
    files = list(hlm = hlm_file$mac, hlm_text = text, html = out_mac,
                 mdm = mdm$mdm),
    exit          = res$status,
    exit_meaning  = exit_meaning,
    success       = produced_html && (res$status %in% c(0L, 136L)),
    duration      = res$duration,
    fixed_effects        = parsed$fixed_effects,
    fixed_effects_robust = parsed$fixed_effects_robust,
    variance_components  = parsed$variance_components,
    reliability   = parsed$reliability,
    deviance      = parsed$deviance,
    sample_sizes  = parsed$sample_sizes,
    title         = parsed$title,
    messages      = parsed$messages,
    warnings      = parsed$warnings,
    html          = if (produced_html)
                      paste(readLines(out_mac, warn = FALSE), collapse = "\n")
                    else NA_character_
  ), class = "hlm_result")
}

#' Save the raw HLM HTML output to a chosen path.
#'
#' @param result   an hlm_result returned by hlm_fit
#' @param path     destination path for the HTML file
hlm_save_html <- function(result, path) {
  stopifnot(inherits(result, "hlm_result"))
  if (is.na(result$html))
    stop("no HTML output is attached to this result")
  writeLines(result$html, path)
  invisible(path)
}

#' Print method for hlm_result.
print.hlm_result <- function(x, ...) {
  cat("HLM result (",
      sprintf("%s, exit=%d, %s", x$exit_meaning, x$exit,
              if (x$success) "ok" else "FAILED"),
      ")\n", sep = "")
  if (!is.null(x$sample_sizes)) {
    cat("  N: ")
    cat(paste(names(x$sample_sizes), x$sample_sizes, sep="="), sep=", ")
    cat("\n")
  }
  if (!is.null(x$fixed_effects)) {
    cat("  Fixed effects:\n")
    print(x$fixed_effects, n = 20)
  }
  if (!is.null(x$variance_components)) {
    cat("  Variance components:\n")
    if (inherits(x$variance_components, "data.frame")) {
      print(x$variance_components, n = 20)
    } else {
      for (i in seq_along(x$variance_components)) {
        cat("  -- table", i, "--\n")
        print(x$variance_components[[i]], n = 20)
      }
    }
  }
  if (length(x$warnings)) {
    cat("  Warnings:\n")
    for (w in x$warnings) cat("    *", w, "\n")
  }
  cat("  Files:\n")
  cat("    .hlm command:", x$files$hlm, "\n")
  cat("    .html output:", x$files$html, "\n")
  cat("  $html holds the full HTML as a single string;\n")
  cat("  hlm_save_html(result, \"path.html\") to write it elsewhere.\n")
  invisible(x)
}
