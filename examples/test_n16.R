# =============================================================================
# End-to-end test: build a 3-level MDM and fit a model on the n16 data.
# =============================================================================
#
# This is the canonical "does the wrapper work" test. It mirrors what the
# user did interactively in the GUI but with one R function call per step,
# starting from CSV files.

# 1. Load the wrapper.
# Resolve the wrapper's R/99_load.R relative to THIS script.
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
HERE <- dirname(.script_path())
source(file.path(HERE, "..", "R", "99_load.R"))

# 2. Point at the source CSVs bundled in examples/data/
data_dir <- file.path(HERE, "data")
l1_csv <- file.path(data_dir, "level1_arousal_n16.csv")        # intentionally unsorted
l2_csv <- file.path(data_dir, "level2_hot_cold_long_n16.csv")
l3_csv <- file.path(data_dir, "level3_hot_cold_wide_n16.csv")

stopifnot(file.exists(l1_csv), file.exists(l2_csv), file.exists(l3_csv))

# 3. Build a 3-level MDM. The wrapper:
#      - reads the three CSVs
#      - sorts level-1 by (PID, cond) — bypassing the silent-drop bug
#      - writes .sav files into a fresh workspace inside the bottle
#      - generates the .mdmt template
#      - runs hlm3.exe -w -r to materialize the binary .mdm
mdm <- hlm_build_mdm3(
  level1 = l1_csv,
  level2 = l2_csv,
  level3 = l3_csv,
  l3_id  = "PID",
  l2_id  = "cond",
  # Use NULL → wrapper includes all source columns (matches GUI behavior;
  # avoids the empirical "subset .sav makes HLM read N=1" issue).
  vars_l1 = NULL, vars_l2 = NULL, vars_l3 = NULL,
  workspace = "n16_test",
  mdm_name  = "n16"
)

cat("MDM built.\n")
cat("  rows: l1=", mdm$rows$l1, " l2=", mdm$rows$l2, " l3=", mdm$rows$l3, "\n", sep="")
cat("  expected: l1=288 l2=48 l3=16  (if you see 150/25/16 the sort fix didn't fire)\n")

# 4. Specify a model. We're keeping it simple to avoid the singularity
#    that the user's earlier example.hlm hit:
#    - Outcome AROUSAL
#    - One level-1 predictor (D1_obs); intercept random, slope fixed
#    - One level-2 predictor of the intercept (HOT); fixed
#    - One level-3 predictor of the intercept of the intercept (HC_C_Z_NAT); fixed
spec <- hlm3_spec(
  outcome       = "arousal",
  l1_predictors = c("D1_obs"),
  l2 = list(
    INTRCPT = list(predictors = "hot", random = TRUE),
    D1_obs  = list(predictors = NULL,  random = FALSE)
  ),
  l3 = list(
    # NB: HLM truncates L3 var names to 8 chars; HC_C_Z_N is the canonical 8-char form
    "INTRCPT/INTRCPT" = list(predictors = "HC_C_Z_N", random = TRUE)
  ),
  numit = 100,
  stopval = 1e-6
)

# 5. Show the generated .hlm text before running
cat("\n--- Generated .hlm command file ---\n")
cat(hlm_render_hlm3(spec, basename(mdm$mdm),
                    paste0(mdm$workspace$win, "\\n16_graph.geq"),
                    paste0(mdm$workspace$win, "\\fit.html")), "\n")
cat("--- end ---\n\n")

# 6. Fit
result <- hlm_fit(mdm, spec, model_name = "fit")

# 7. Inspect
cat("\nResult: exit=", result$exit, " (", result$exit_meaning, ")  duration=",
    round(result$duration, 1), "s\n", sep="")
print(result)
