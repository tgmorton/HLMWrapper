# =============================================================================
# hlmwrap — environment setup and verification
# =============================================================================
#
# Run this on a fresh machine to verify everything is in place. Will install
# missing R packages and run smoke tests.
#
#   Rscript /path/to/hlmwrap/setup.R
#
# Exit code 0 = ready to use. Non-zero = something needs attention.

cat("hlmwrap setup — checking environment\n\n")
ok <- TRUE
fail <- function(msg) { cat("[FAIL] ", msg, "\n", sep = ""); ok <<- FALSE }
warn <- function(msg) { cat("[WARN] ", msg, "\n", sep = "") }
pass <- function(msg) { cat("[ok]   ", msg, "\n", sep = "") }

# 1. macOS / arch sanity
if (Sys.info()[["sysname"]] != "Darwin") {
  fail("hlmwrap requires macOS (HLM is Windows; we run it via Whisky/Wine)")
} else {
  pass(sprintf("macOS %s on %s", Sys.info()[["release"]], Sys.info()[["machine"]]))
}

# 2. Whisky present?
whisky_app <- "/Applications/Whisky.app"
whisky_wine <- "~/Library/Application Support/com.isaacmarovitz.Whisky/Libraries/Wine/bin/wine64"
whisky_bottles <- "~/Library/Containers/com.isaacmarovitz.Whisky/Bottles"
if (!dir.exists(whisky_app)) {
  fail(paste("Whisky.app not found at", whisky_app, "- install from",
             "https://getwhisky.app"))
} else {
  pass("Whisky.app present")
}
if (!file.exists(path.expand(whisky_wine))) {
  fail(paste("wine64 not found at", whisky_wine,
             "- open Whisky once to bootstrap GPTK"))
} else {
  pass("Whisky's bundled wine64 present")
}

# 3. At least one bottle with HLM 8.2 installed
bdir <- path.expand(whisky_bottles)
hlm_bottle <- NULL
if (dir.exists(bdir)) {
  for (c in list.dirs(bdir, recursive = FALSE, full.names = FALSE)) {
    if (file.exists(file.path(bdir, c, "drive_c", "Program Files",
                              "HLM 8.2", "hlm3.exe"))) {
      hlm_bottle <- c; break
    }
  }
}
if (is.null(hlm_bottle)) {
  fail("No Whisky bottle has HLM 8.2 installed.\n",
       "         Create a bottle in Whisky, drag HLMInstaller.exe in, run it.")
} else {
  pass(sprintf("HLM 8.2 found in bottle %s", hlm_bottle))
  # check the solver list
  hlm_dir <- file.path(bdir, hlm_bottle, "drive_c", "Program Files", "HLM 8.2")
  expected <- c("hlm2.exe","hlm3.exe","hlm4.exe","whlm.exe","statrn32.dll")
  miss <- expected[!file.exists(file.path(hlm_dir, expected))]
  if (length(miss)) warn(sprintf("missing solver(s): %s",
                                 paste(miss, collapse = ", ")))
}

# 4. R packages
needed <- c("haven","processx","tibble","rvest","xml2","readr")
miss <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  cat("\nInstalling missing R packages: ", paste(miss, collapse = ", "), "\n")
  install.packages(miss, repos = "https://cloud.r-project.org")
  miss <- miss[!vapply(miss, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    fail(sprintf("could not install: %s", paste(miss, collapse = ", ")))
  } else {
    pass("all R packages installed")
  }
} else {
  pass(paste("R packages OK:", paste(needed, collapse = ", ")))
}

# 5. Source the wrapper itself
`%||%` <- function(a, b) if (is.null(a)) b else a
# When run via `Rscript`, the script path is in commandArgs(); when sourced
# inside an R session it's in sys.frame(). Try both.
.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) return(normalizePath(sub("^--file=", "", fa[1])))
  if (sys.nframe() >= 1L) {
    f <- sys.frame(1)
    if (!is.null(f$ofile)) return(normalizePath(f$ofile))
  }
  normalizePath(".")
}
here <- dirname(.script_path())
load_file <- file.path(here, "R", "99_load.R")
if (!file.exists(load_file)) {
  fail("missing R/99_load.R - is the hlmwrap directory complete?")
} else {
  source(load_file)
  pass("hlmwrap loaded")
}

# 6. End-to-end smoke tests (HLM3 + HLM2). Spawn each as a fresh Rscript so
#    --file= is set correctly inside the child.
example_csv <- file.path(here, "examples", "data")
if (ok && length(list.files(example_csv, "\\.csv$"))) {
  for (test in c("test_n16.R", "test_n16_hlm2.R")) {
    cat("\nRunning", test, "...\n")
    res <- system2("Rscript", file.path(here, "examples", test),
                   stdout = TRUE, stderr = TRUE)
    code <- attr(res, "status") %||% 0L
    has_success <- any(grepl("\\(success", res))
    has_ok <- any(grepl("HLM result.*ok", res))
    if (code == 0L && (has_success || has_ok)) {
      pass(paste(test, "passed"))
    } else {
      fail(paste0(test, " failed (exit=", code, "):\n         ",
                  paste(tail(res, 8), collapse = "\n          ")))
    }
  }
} else {
  cat("\n(smoke tests skipped — no example data in", example_csv, ")\n")
}

cat("\n", if (ok) "READY" else "NOT READY", "\n", sep = "")
quit(status = if (ok) 0L else 1L)
