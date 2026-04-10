# =============================================================================
# .hlm command file generation.
# =============================================================================
#
# An HLM .hlm command file is plain text that specifies the model to fit
# against an existing .mdm file. We mirror the format we observed in
# example.hlm and newcmd.hlm exactly.
#
# Mental model of an HLM3 model:
#
#   Level 1: Y = INTRCPT1 + p1*X1 + p2*X2 + ...           (i.e. one outcome)
#   Level 2: each level-1 coefficient (INTRCPT1, X1, X2, ...) gets its own
#            equation predicting it from level-2 variables; each is either
#            random or fixed.
#   Level 3: each level-2 coefficient (the intercept + each level-2 predictor
#            of each level-2 equation) gets its own equation predicting it
#            from level-3 variables; each is random or fixed.
#
# In the .hlm file the equations are written depth-first:
#
#   level1: Y = INTRCPT1 + X1 + X2 + RANDOM
#   level2: INTRCPT1 = INTRCPT2 + W1 + random/    <-- L2 eq for L1 INTRCPT
#   level3: INTRCPT2 = INTRCPT3 + Z1 + random/    <-- L3 eq for L2 INTRCPT
#   level3: W1       = INTRCPT3/                  <-- L3 eq for L2 W1
#   level2: X1       = INTRCPT2 + W2/             <-- L2 eq for L1 X1
#   level3: INTRCPT2 = INTRCPT3/                  <-- L3 eq for L2 INTRCPT (of X1)
#   level3: W2       = INTRCPT3/                  <-- L3 eq for L2 W2 (of X1)
#   level2: X2       = INTRCPT2/                  <-- L2 eq for L1 X2
#   level3: INTRCPT2 = INTRCPT3/                  <-- L3 eq for L2 INTRCPT (of X2)
#
# The wrapper builds the equation list automatically from a high-level spec.

# =============================================================================
# Predictor + centering helpers.
# =============================================================================
#
# Each predictor in an L1/L2/L3 equation can be either:
#   - a bare character string         e.g. "D1_OBS"          (uncentered)
#   - a named list                    e.g. list(name="CSFQTOT", center="grand")
#
# Centering codes (per re/notes/03_file_format_spec.md §B.2):
#   "none"  / NULL  ->  no suffix         (e.g. "D1_OBS")
#   "group"         ->  ",1"              (e.g. "D1_OBS,1") group-mean centering
#   "grand"         ->  ",2"              (e.g. "CSFQTOT,2") grand-mean centering
#
# .center_code(): translate semantic name to HLM code
.center_code <- function(x) {
  if (is.null(x) || is.na(x) || identical(x, "none") || identical(x, FALSE) ||
      identical(x, 0) || identical(x, 0L)) return("")
  if (identical(x, "group") || identical(x, 1) || identical(x, 1L)) return(",1")
  if (identical(x, "grand") || identical(x, 2) || identical(x, 2L)) return(",2")
  stop("invalid centering code: ", x,
       " (use 'none', 'group', 'grand', or NULL)")
}

# .render_predictor(): take either a string or a list and emit a single token
.render_predictor <- function(p) {
  if (is.character(p) && length(p) == 1L) {
    # Bare name, no centering
    return(p)
  }
  if (is.list(p) && !is.null(p$name)) {
    paste0(p$name, .center_code(p$center))
  } else {
    stop("predictor must be a string or list(name, center=...); got: ",
         paste(class(p), collapse = "/"))
  }
}

# .normalize_predictors(): coerce a "predictors" arg to a list of canonical
# predictor descriptors. Accepts c("a","b") or list("a","b") or
# list(list(name="a"), list(name="b",center="grand")).
.normalize_predictors <- function(p) {
  if (is.null(p) || (is.character(p) && length(p) == 0L)) return(list())
  if (is.character(p)) return(as.list(p))
  if (is.list(p))      return(p)
  stop("predictors must be character or list, got: ", class(p)[1])
}

# .extract_var_names(): get the variable name from each predictor (no centering)
.extract_var_names <- function(p) {
  vapply(.normalize_predictors(p), function(x) {
    if (is.character(x)) x else x$name
  }, character(1))
}

# .validate_var_name(): enforce HLM's 8-char limit
.validate_var_name <- function(name, where) {
  bad <- name[nchar(name) > 8L]
  if (length(bad))
    stop(where, ": variable name(s) exceed HLM's 8-character limit and would ",
         "be silently truncated: ", paste(bad, collapse = ", "))
  invisible(name)
}

#' Build an HLM3 model spec.
#'
#' This is the **agent-facing API** for specifying a 3-level model. Every
#' field is named and unambiguous; an LLM agent can construct the spec as
#' a JSON-equivalent R list. Defaults are filled in for any equation that
#' isn't explicitly overridden.
#'
#' @param outcome        name of the outcome variable (must be in the level-1 file)
#' @param l1_predictors  character vector of level-1 predictor names. The
#'                       intercept is always implicit and named "INTRCPT" in
#'                       the spec; the underlying file uses INTRCPT1.
#' @param l2             named list of level-2 equation overrides. Names are
#'                       level-1 coefficient names ("INTRCPT", or any name
#'                       in `l1_predictors`). Each value is a list with:
#'                         - predictors: character vector of L2 vars (default NULL)
#'                         - random: logical (default TRUE for INTRCPT, else FALSE)
#'                       Coefficients not listed default to (predictors=NULL,
#'                       random = (name == "INTRCPT")).
#' @param l3             named list of level-3 equation overrides. Names are
#'                       paths "<l1coef>/<l2coef>" (e.g. "INTRCPT/INTRCPT").
#'                       Each value is the same shape as l2 entries.
#'                       Defaults: predictors=NULL, random=FALSE.
#' @param numit          maximum number of iterations
#' @param stopval        convergence criterion
#' @param fishertype     1 or 2 (Fisher scoring variant)
#' @param accel          acceleration parameter
#' @param fixtau2,fixtau3 0..3 — fix variance components at each level
#' @param hypoth         "y"/"n" — multivariate hypothesis tests
#' @param fulloutput     "y"/"n" — verbose output
#' @param title          run title (string)
#' @param extras         named list of additional raw directives written verbatim
#'
#' @return list of class "hlm3_spec"
hlm3_spec <- function(outcome,
                      l1_predictors = character(),
                      l2 = list(),
                      l3 = list(),
                      numit = 100L,
                      stopval = 1e-6,
                      fishertype = 2L,
                      accel = 5L,
                      fixtau2 = 3L,
                      fixtau3 = 3L,
                      hypoth = "n",
                      fulloutput = "n",
                      title = "no title",
                      lvr_beta = "n",
                      constrain = "N",
                      varianceknown = "none",
                      level1_deletion = "none",
                      l1_weight = "none",
                      l2_weight = "none",
                      l3_weight = "none",
                      extras = list()) {
  stopifnot(is.character(outcome), length(outcome) == 1L, nzchar(outcome))
  .validate_var_name(outcome, "outcome")
  l1_pred_names <- .extract_var_names(l1_predictors)
  .validate_var_name(l1_pred_names, "l1_predictors")
  l1_coefs <- c("INTRCPT", l1_pred_names)
  if (anyDuplicated(l1_coefs))
    stop("duplicate level-1 coefficient name in spec")

  # Fill in l2 defaults
  l2_full <- lapply(l1_coefs, function(coef) {
    user <- l2[[coef]]
    list(
      coef       = coef,
      predictors = user$predictors %||% character(),
      random     = user$random     %||% (coef == "INTRCPT")
    )
  })
  names(l2_full) <- l1_coefs

  # Build l3 defaults from the l2 structure
  l3_full <- list()
  for (l1c in l1_coefs) {
    l2_eq <- l2_full[[l1c]]
    l2_coefs <- c("INTRCPT", .extract_var_names(l2_eq$predictors))
    for (l2c in l2_coefs) {
      key <- paste0(l1c, "/", l2c)
      user <- l3[[key]]
      l3_full[[key]] <- list(
        l1c        = l1c,
        l2c        = l2c,
        predictors = user$predictors %||% character(),
        random     = user$random     %||% FALSE
      )
    }
  }

  # Validate user l3 keys reference real l2 coefs
  for (k in names(l3)) {
    if (!k %in% names(l3_full))
      stop("l3 override key '", k, "' does not match any level-2 coefficient. ",
           "Valid keys: ", paste(names(l3_full), collapse = ", "))
  }

  .validate_extras(extras, .HLM3_DIRECTIVE_WHITELIST, "hlm3_spec")
  structure(list(
    outcome = outcome,
    l1_predictors = l1_predictors,
    .l1_coef_names = l1_coefs,
    l2 = l2_full,
    l3 = l3_full,
    options = list(
      numit = as.integer(numit),
      stopval = stopval,
      fishertype = as.integer(fishertype),
      accel = as.integer(accel),
      fixtau2 = as.integer(fixtau2),
      fixtau3 = as.integer(fixtau3),
      hypoth = hypoth,
      fulloutput = fulloutput,
      title = title,
      lvr_beta = lvr_beta,
      constrain = constrain,
      varianceknown = varianceknown,
      level1_deletion = level1_deletion,
      l1_weight = l1_weight,
      l2_weight = l2_weight,
      l3_weight = l3_weight
    ),
    extras = extras
  ), class = "hlm3_spec")
}

# =============================================================================
# Directive whitelist — unknown directives cause hlm3.exe to HANG on getchar()
# (per re/notes/02_directive_reference.md "Unknown directives" warning).
# =============================================================================
.HLM3_DIRECTIVE_WHITELIST <- c(
  "nonlin","numit","stopval","level1","level2","level3",
  "level1weight","level2weight","level3weight",
  "varianceknown","level1deletion","level2deletion","level3deletion",
  "hypoth","resfil1","resfil2","resfil3","fishertype",
  "fixtau2","fixtau3","accel","constrain",
  "graphgammas","lvr-beta","title","output","fulloutput",
  "macroit","microit","stopmicro","stopmacro",
  "laplace","agq","agqderivlevel","autoimpute","autoimputeiter","autoimputekeep",
  "contau2","contau3","debug","deviance","diagonalizetaubeta","diagonalizetaupi",
  "dofisher","esttau2","esttau3","firc","fixsigma2","growthmodel","hetit",
  "nobase","plausvals","popavit","printvariance-covariance",
  "resfil1name","resfil2name","resfil3name","resfiltype",
  "startvals","stopval","taylor","vagueprior"
)
.HLM2_DIRECTIVE_WHITELIST <- c(
  "nonlin","numit","stopval","level1","level2",
  "level1weight","level2weight",
  "varianceknown","level1deletion","level2deletion",
  "hypoth","resfil1","resfil2","fixtau","lev1ols","accel",
  "constrain","graphgammas","lvr","title","output","fulloutput",
  "homvar","heterol1var","mlf",
  "macroit","microit","stopmicro","stopmacro",
  "laplace","agq","autoimpute","autoimputeiter","autoimputekeep",
  "debug","deviance","dofisher","esttau2","fixsigma2","growthmodel","hetit",
  "nobase","plausvals","popavit","startvals","taylor","vagueprior",
  "hasmissing"  # alias for l1missing
)

.validate_extras <- function(extras, whitelist, where) {
  if (!length(extras)) return(invisible(TRUE))
  if (is.null(names(extras)) || any(!nzchar(names(extras))))
    stop(where, ": extras must be a named list")
  bad <- setdiff(tolower(names(extras)), whitelist)
  if (length(bad))
    stop(where, ": unknown directive(s) — these would HANG hlm*.exe on a ",
         "getchar() prompt: ", paste(bad, collapse = ", "),
         "\nValid directives: ", paste(head(whitelist, 20), collapse = ", "),
         ", ...")
  invisible(TRUE)
}

#' Render an hlm3_spec as the body of an .hlm command file.
#'
#' @param spec   an hlm3_spec
#' @param mdm_basename basename of the .mdm (used in the header comment)
#' @param graphgammas_win   Win path for the (unused) gamma graph file
#' @param output_html_win   Win path for the HTML output
hlm_render_hlm3 <- function(spec, mdm_basename,
                            graphgammas_win, output_html_win) {
  stopifnot(inherits(spec, "hlm3_spec"))
  L <- character()
  push <- function(x) L[length(L) + 1L] <<- x

  push(sprintf("#WHLM CMD FILE FOR %s", mdm_basename))
  push("nonlin:n")
  push(sprintf("numit:%d",       spec$options$numit))
  push(sprintf("stopval:%.10f",  spec$options$stopval))

  # Level-1 equation: outcome = INTRCPT1 + p1 + p2 ... + RANDOM
  l1_pred_tokens <- vapply(.normalize_predictors(spec$l1_predictors),
                           .render_predictor, character(1))
  l1_rhs <- c("INTRCPT1", l1_pred_tokens)
  push(sprintf("level1:%s=%s+RANDOM",
               spec$outcome, paste(l1_rhs, collapse = "+")))

  # Walk depth-first: each l1 coef → its l2 eq → for each l2 coef in that
  # eq, the corresponding l3 eq
  for (l1c in spec$.l1_coef_names) {
    l2_eq <- spec$l2[[l1c]]
    l2_lhs <- if (l1c == "INTRCPT") "INTRCPT1" else l1c
    l2_pred_tokens <- vapply(.normalize_predictors(l2_eq$predictors),
                             .render_predictor, character(1))
    l2_rhs <- c("INTRCPT2", l2_pred_tokens)
    push(sprintf("level2:%s=%s%s/",
                 l2_lhs,
                 paste(l2_rhs, collapse = "+"),
                 if (l2_eq$random) "+random" else ""))

    # The L3 equations for this L2 equation, depth-first.
    # l2_coef_names: the literal coef names ("INTRCPT" + bare predictor names)
    l2_coef_names <- c("INTRCPT", .extract_var_names(l2_eq$predictors))
    for (l2c in l2_coef_names) {
      l3_eq <- spec$l3[[paste0(l1c, "/", l2c)]]
      l3_lhs <- if (l2c == "INTRCPT") "INTRCPT2" else l2c
      l3_pred_tokens <- vapply(.normalize_predictors(l3_eq$predictors),
                               .render_predictor, character(1))
      l3_rhs <- c("INTRCPT3", l3_pred_tokens)
      push(sprintf("level3:%s=%s%s/",
                   l3_lhs,
                   paste(l3_rhs, collapse = "+"),
                   if (l3_eq$random) "+random" else ""))
    }
  }

  # Trailing options block
  push(sprintf("fixtau2:%d", spec$options$fixtau2))
  push(sprintf("fixtau3:%d", spec$options$fixtau3))
  push(sprintf("accel:%d",   spec$options$accel))
  push(sprintf("level1weight:%s", spec$options$l1_weight))
  push(sprintf("level2weight:%s", spec$options$l2_weight))
  push(sprintf("level3weight:%s", spec$options$l3_weight))
  push(sprintf("varianceknown:%s", spec$options$varianceknown))
  push(sprintf("level1deletion:%s", spec$options$level1_deletion))
  push(sprintf("hypoth:%s", spec$options$hypoth))
  push("resfil1:n")
  push("resfil2:n")
  push("resfil3:n")
  push(sprintf("constrain:%s", spec$options$constrain))
  push(sprintf("graphgammas:%s", graphgammas_win))
  push(sprintf("lvr-beta:%s", spec$options$lvr_beta))
  push(sprintf("title:%s", spec$options$title))
  push(sprintf("output:%s", output_html_win))
  push(sprintf("fulloutput:%s", spec$options$fulloutput))
  push(sprintf("fishertype:%d", spec$options$fishertype))

  # Extras (raw directives)
  for (k in names(spec$extras)) {
    push(sprintf("%s:%s", k, spec$extras[[k]]))
  }

  paste(L, collapse = "\n")
}

#' Build an HLM2 model spec.
#'
#' Two-level analogue of `hlm3_spec`. Each level-1 coefficient gets exactly
#' one level-2 equation. There is no level-3.
#'
#' @inheritParams hlm3_spec
#' @param fixtau         0..3 — fix variance components
#' @param lev1ols        OLS prep iterations (HLM2 specific)
#' @param heterol1var    "y"/"n" — heterogeneous level-1 variance
#' @param homvar         "y"/"n" — homogeneity of variance test
#' @param mlf            "y"/"n" — full ML (else REML)
hlm2_spec <- function(outcome,
                      l1_predictors = character(),
                      l2 = list(),
                      numit = 100L,
                      stopval = 1e-6,
                      accel = 5L,
                      fixtau = 3L,
                      lev1ols = 10L,
                      hypoth = "n",
                      fulloutput = "n",
                      heterol1var = "n",
                      homvar = "n",
                      mlf = "n",
                      lvr = "n",
                      title = "no title",
                      constrain = "N",
                      varianceknown = "none",
                      level1_deletion = "none",
                      level2_deletion = "none",
                      l1_weight = "none",
                      l2_weight = "none",
                      extras = list()) {
  stopifnot(is.character(outcome), length(outcome) == 1L)
  .validate_var_name(outcome, "outcome")
  l1_pred_names <- .extract_var_names(l1_predictors)
  .validate_var_name(l1_pred_names, "l1_predictors")
  l1_coefs <- c("INTRCPT", l1_pred_names)
  if (anyDuplicated(l1_coefs)) stop("duplicate level-1 coefficient name")
  l2_full <- lapply(l1_coefs, function(coef) {
    user <- l2[[coef]]
    list(
      coef       = coef,
      predictors = user$predictors %||% character(),
      random     = user$random     %||% (coef == "INTRCPT")
    )
  })
  names(l2_full) <- l1_coefs
  .validate_extras(extras, .HLM2_DIRECTIVE_WHITELIST, "hlm2_spec")
  structure(list(
    outcome = outcome,
    l1_predictors = l1_predictors,
    .l1_coef_names = l1_coefs,
    l2 = l2_full,
    options = list(
      numit = as.integer(numit),
      stopval = stopval,
      accel = as.integer(accel),
      fixtau = as.integer(fixtau),
      lev1ols = as.integer(lev1ols),
      hypoth = hypoth,
      fulloutput = fulloutput,
      heterol1var = heterol1var,
      homvar = homvar,
      mlf = mlf,
      lvr = lvr,
      title = title,
      constrain = constrain,
      varianceknown = varianceknown,
      level1_deletion = level1_deletion,
      level2_deletion = level2_deletion,
      l1_weight = l1_weight,
      l2_weight = l2_weight
    ),
    extras = extras
  ), class = "hlm2_spec")
}

#' Render an hlm2_spec to a .hlm command file body.
hlm_render_hlm2 <- function(spec, mdm_basename,
                            graphgammas_win, output_html_win) {
  stopifnot(inherits(spec, "hlm2_spec"))
  L <- character()
  push <- function(x) L[length(L) + 1L] <<- x
  push(sprintf("#WHLM CMD FILE FOR %s", mdm_basename))
  push("nonlin:n")
  push(sprintf("numit:%d",      spec$options$numit))
  push(sprintf("stopval:%.10f", spec$options$stopval))
  l1_pred_tokens <- vapply(.normalize_predictors(spec$l1_predictors),
                           .render_predictor, character(1))
  l1_rhs <- c("INTRCPT1", l1_pred_tokens)
  push(sprintf("level1:%s=%s+RANDOM",
               spec$outcome, paste(l1_rhs, collapse = "+")))
  for (l1c in spec$.l1_coef_names) {
    eq <- spec$l2[[l1c]]
    lhs <- if (l1c == "INTRCPT") "INTRCPT1" else l1c
    pred_tokens <- vapply(.normalize_predictors(eq$predictors),
                          .render_predictor, character(1))
    rhs <- c("INTRCPT2", pred_tokens)
    push(sprintf("level2:%s=%s%s/", lhs,
                 paste(rhs, collapse = "+"),
                 if (eq$random) "+random" else ""))
  }
  push(sprintf("fixtau:%d",  spec$options$fixtau))
  push(sprintf("lev1ols:%d", spec$options$lev1ols))
  push(sprintf("accel:%d",   spec$options$accel))
  push(sprintf("level1weight:%s", spec$options$l1_weight))
  push(sprintf("level2weight:%s", spec$options$l2_weight))
  push(sprintf("varianceknown:%s", spec$options$varianceknown))
  push(sprintf("level1deletion:%s", spec$options$level1_deletion))
  push(sprintf("level2deletion:%s", spec$options$level2_deletion))
  push(sprintf("hypoth:%s", spec$options$hypoth))
  push("resfil1:n")
  push("resfil2:n")
  push(sprintf("homvar:%s",  spec$options$homvar))
  push(sprintf("constrain:%s", spec$options$constrain))
  push(sprintf("heterol1var:%s", spec$options$heterol1var))
  push(sprintf("graphgammas:%s", graphgammas_win))
  push(sprintf("lvr:%s",     spec$options$lvr))
  push(sprintf("title:%s",   spec$options$title))
  push(sprintf("output:%s",  output_html_win))
  push(sprintf("fulloutput:%s", spec$options$fulloutput))
  push(sprintf("mlf:%s",     spec$options$mlf))
  for (k in names(spec$extras)) push(sprintf("%s:%s", k, spec$extras[[k]]))
  paste(L, collapse = "\n")
}

#' Write an .hlm file to disk and return paths.
hlm_write_hlm <- function(text, ws, basename = "model") {
  if (!grepl("\\.hlm$", basename, ignore.case = TRUE))
    basename <- paste0(basename, ".hlm")
  mac_path <- file.path(ws$mac, basename)
  writeLines(text, mac_path, sep = "\n")
  list(mac = mac_path, win = paste0(ws$win, "\\", basename))
}
