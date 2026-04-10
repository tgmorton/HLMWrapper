# =============================================================================
# Parse hlm2.exe / hlm3.exe HTML output into tidy R structures.
# =============================================================================
#
# HLM emits hand-rolled HTML with these consistent landmarks:
#
#   <h2>Specifications for this HLM3 run</h2>
#       Problem Title: ...
#       The maximum number of level-N units = ...
#       Method of estimation: ...
#   <h4>Level-N Model</h4>                  -- model rendering (math)
#   <h4>Mixed Model</h4>
#   <h4> Final estimation of fixed effects:</h4>     -- table
#   <h4> Final estimation of fixed effects (with robust standard errors)</h4>
#   <h4>Final estimation of (level-1 and level-2 | level-3 | ...) variance components</h4>
#   <h4>Statistics for the current model</h4>        -- deviance / parameter count
#
# Implemented with rvest + xml2 instead of regex.

#' Strip HTML entities and tags from a string.
.clean_html <- function(x) {
  if (is.null(x) || all(is.na(x))) return(x)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;",  "&", x, fixed = TRUE)
  x <- gsub("&lt;",   "<", x, fixed = TRUE)
  x <- gsub("&gt;",   ">", x, fixed = TRUE)
  x <- gsub("&copy;", "(c)", x, fixed = TRUE)
  x <- gsub("&beta;", "beta", x, fixed = TRUE)
  x <- gsub("&pi;",   "pi",   x, fixed = TRUE)
  x <- gsub("&gamma;","gamma",x, fixed = TRUE)
  x <- gsub("&chi;",  "chi",  x, fixed = TRUE)
  x <- gsub("&sigma;","sigma",x, fixed = TRUE)
  x <- gsub("&tau;",  "tau",  x, fixed = TRUE)
  x <- gsub("&Pi;",   "Pi",   x, fixed = TRUE)
  x <- gsub("&epsilon;","epsilon",x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Find the <table> immediately following the first <h4> whose text matches.
.h4_then_table <- function(doc, heading_pattern) {
  h4s <- rvest::html_elements(doc, "h4")
  if (length(h4s) == 0) return(NULL)
  texts <- vapply(h4s, function(h) .clean_html(rvest::html_text(h)), character(1))
  hit <- grep(heading_pattern, texts, perl = TRUE, ignore.case = TRUE)
  if (length(hit) == 0) return(NULL)
  result <- list()
  for (i in hit) {
    h4 <- h4s[[i]]
    # find next sibling that's a <table>
    sib <- xml2::xml_find_first(h4, "following-sibling::table[1]")
    if (!inherits(sib, "xml_missing"))
      result[[length(result) + 1L]] <- sib
  }
  if (length(result) == 0) NULL else result
}

# Map a wordy raw header name to a canonical short snake_case name.
# Anything we don't recognize stays as a cleaned-up version of the original.
.canonical_col <- function(raw) {
  k <- tolower(gsub("\\s+", " ", trimws(raw)))
  k <- gsub("[^a-z0-9 ]+", " ", k)
  k <- gsub("\\s+", " ", trimws(k))
  table <- list(
    "fixed effect"           = "term",
    "random effect"          = "term",
    "coefficient"            = "coefficient",
    "standard error"         = "se",
    "standard deviation"     = "sd",
    "variance component"    = "variance",
    "t ratio"                = "t_ratio",
    "approx d f"             = "df",
    "approx df"              = "df",
    "d f"                    = "df",
    "df"                     = "df",
    "p value"                = "p_value",
    "chi 2"                  = "chi_sq",
    "chi sq"                 = "chi_sq",
    "chi"                    = "chi_sq",
    "reliability estimate"   = "reliability"
  )
  if (!is.null(table[[k]])) return(table[[k]])
  # Strip common decoration: numeric superscripts left over, leading "for "
  k <- gsub("^for ", "", k)
  k <- gsub("[^a-z0-9_]+", "_", k)
  k <- gsub("_+", "_", k)
  k <- gsub("^_|_$", "", k)
  if (!nzchar(k)) "v" else k
}

# Coerce p-values that may be written as "<0.001" or ">0.500" to numeric.
# Keeps the threshold value (0.001, 0.500), with the inequality semantically
# folded in (consumers can filter `p_value <= 0.001`, etc.).
.coerce_pvalue <- function(x) {
  if (is.numeric(x)) return(x)
  v <- trimws(as.character(x))
  v <- sub("^[<>]\\s*", "", v)              # strip leading <  or >
  suppressWarnings(as.numeric(v))
}

#' Convert an xml_node <table> to a tibble.
#'
#' Handles HLM's specific HTML quirks:
#'   - Header is the first <tr> only (single row), columns mapped to canonical
#'     short names like `coefficient`, `se`, `t_ratio`, `df`, `p_value`.
#'   - "Grouping" rows that use colspan=N to span the full width (e.g.
#'     `For INTRCPT1, π0`) are NOT data rows. They are recorded as a
#'     `for_term` context column attached to subsequent data rows.
#'   - p-values like `<0.001` parse as 0.001 numerically.
.table_to_tibble <- function(tbl_node) {
  rows <- rvest::html_elements(tbl_node, "tr")
  if (length(rows) == 0) return(NULL)
  # Parse each row into a list of cells with their colspan
  parsed <- lapply(rows, function(r) {
    cells <- rvest::html_elements(r, "td,th")
    if (length(cells) == 0) return(NULL)
    text <- vapply(cells, function(c) .clean_html(rvest::html_text(c)),
                   character(1))
    spans <- vapply(cells, function(c) {
      cs <- rvest::html_attr(c, "colspan")
      if (is.na(cs) || !nzchar(cs)) 1L else as.integer(cs)
    }, integer(1))
    list(text = text, spans = spans)
  })
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) return(NULL)

  # Header is row 1 — assume each header cell is colspan=1 in HLM tables
  hdr_raw <- parsed[[1]]$text
  ncols <- length(hdr_raw)
  hdr <- vapply(hdr_raw, .canonical_col, character(1))
  # Disambiguate dups
  if (anyDuplicated(hdr)) hdr <- make.unique(hdr, sep = "_")

  # Walk the body. Each row is either a data row (cells fill all ncols)
  # or a grouping row (1 cell with colspan = ncols, OR fewer cells than
  # ncols total). Grouping rows update a `current_group` context that
  # gets attached to subsequent data rows in a `for_term` column.
  data_rows <- list()
  current_group <- NA_character_
  for (i in seq_along(parsed)[-1]) {
    p <- parsed[[i]]
    total_span <- sum(p$spans)
    is_data <- (length(p$text) == ncols && all(p$spans == 1L))
    if (!is_data) {
      # Treat anything that's not a clean ncols-cell row as a grouping label
      label <- paste(p$text[nzchar(p$text)], collapse = " | ")
      if (nzchar(label)) current_group <- label
      next
    }
    row <- as.list(p$text)
    names(row) <- hdr
    row$for_term <- current_group
    data_rows[[length(data_rows) + 1L]] <- row
  }
  if (length(data_rows) == 0) return(NULL)

  df <- tibble::as_tibble(do.call(rbind, lapply(data_rows, as.data.frame,
                                                stringsAsFactors = FALSE)))

  # Numeric coercion column-by-column. p_value columns get the special
  # `<X` / `>X` handler.
  for (j in seq_len(ncol(df))) {
    nm <- names(df)[j]
    if (nm == "for_term") next
    v <- df[[j]]
    if (nm == "p_value") {
      df[[j]] <- .coerce_pvalue(v)
    } else {
      nv <- suppressWarnings(as.numeric(v))
      nz <- nzchar(v) & !is.na(v)
      if (sum(!is.na(nv)) >= sum(nz) * 0.7 && sum(nz) > 0)
        df[[j]] <- nv
    }
  }

  # Reorder so for_term comes first if it exists and isn't all NA
  if ("for_term" %in% names(df) && any(!is.na(df$for_term))) {
    df <- df[, c("for_term", setdiff(names(df), "for_term")), drop = FALSE]
  } else if ("for_term" %in% names(df)) {
    df$for_term <- NULL
  }
  df
}

#' Top-level HTML parser. Returns a list of tidy result components.
hlm_parse_html <- function(path) {
  # HLM emits literal `<0.001` and `>0.999` p-value cells (no entity escape)
  # which confuse libxml2's HTML parser into dropping the text. We only want
  # to escape these *as content of a cell*, not anywhere a `<` or `>`
  # happens to precede a digit (which would clobber tag closes like `>9 April`).
  # Pattern: a tag close `>` immediately followed by `<0.NNN` or `>0.NNN`
  # immediately followed by another tag open `<`.
  raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
  raw <- gsub(">\\s*<(0\\.[0-9]+)\\s*<", ">&lt;\\1<", raw, perl = TRUE)
  raw <- gsub(">\\s*>(0\\.[0-9]+)\\s*<", ">&gt;\\1<", raw, perl = TRUE)
  doc <- xml2::read_html(raw)
  body_text <- .clean_html(rvest::html_text(doc))
  out <- list()

  # Title
  m <- regmatches(body_text, regexpr("Problem Title:\\s*\\S[^.\\n]*",
                                     body_text, perl = TRUE))
  if (length(m)) out$title <- trimws(sub("^Problem Title:\\s*", "", m))

  # Sample sizes
  ns <- list()
  for (lvl in 1:4) {
    pat <- sprintf("The maximum number of level-%d units\\s*=\\s*([0-9]+)", lvl)
    m <- regmatches(body_text, regexpr(pat, body_text, perl = TRUE))
    if (length(m)) {
      val <- as.integer(sub(".*=\\s*", "", m))
      ns[[paste0("level", lvl)]] <- val
    }
  }
  if (length(ns)) out$sample_sizes <- ns

  # Method of estimation
  m <- regmatches(body_text, regexpr("Method of estimation:\\s*[^.<\n]+",
                                     body_text, perl = TRUE))
  if (length(m)) out$method <- trimws(sub("^Method of estimation:\\s*", "", m))

  # Deviance
  m <- regmatches(body_text, regexpr("Deviance\\s*=\\s*([\\-0-9.eE+]+)",
                                     body_text, perl = TRUE))
  if (length(m)) out$deviance <- as.numeric(sub(".*=\\s*", "", m))

  # Fixed effects (non-robust)
  fe <- .h4_then_table(doc,
    "Final estimation of fixed effects(?!.*robust)")
  if (length(fe))
    out$fixed_effects <- .table_to_tibble(fe[[1]])

  # Fixed effects (robust SE)
  feR <- .h4_then_table(doc,
    "Final estimation of fixed effects.*robust standard")
  if (length(feR))
    out$fixed_effects_robust <- .table_to_tibble(feR[[1]])

  # Variance components — possibly multiple tables for HLM3+
  vc <- .h4_then_table(doc, "Final estimation of.*variance components")
  if (length(vc)) {
    parsed <- lapply(vc, .table_to_tibble)
    parsed <- parsed[!vapply(parsed, is.null, logical(1))]
    if (length(parsed) == 1L) {
      out$variance_components <- parsed[[1]]
    } else if (length(parsed) > 1L) {
      out$variance_components <- parsed   # named or numbered list
    }
  }

  # Reliability
  rel <- .h4_then_table(doc, "Reliability estimates|Reliability estimate")
  if (length(rel))
    out$reliability <- .table_to_tibble(rel[[1]])

  # Warning / error blurbs in body text
  warn_signals <- c(
    "near singularity",
    "collinearity",
    "Sigma_squared.*is virtually zero",
    "no degrees of freedom",
    "did not converge",
    "Iterations terminated prematurely",
    "There are missing data at level",
    "Group .* not in level"
  )
  warnings <- character()
  for (pat in warn_signals) {
    m <- regmatches(body_text, regexpr(paste0("[^.]*", pat, "[^.]*\\."),
                                       body_text, ignore.case = TRUE,
                                       perl = TRUE))
    if (length(m)) warnings <- c(warnings, trimws(m))
  }
  out$warnings <- unique(warnings)

  out
}
