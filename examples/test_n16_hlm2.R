# =============================================================================
# End-to-end test for HLM2 (2-level model). Same n16 dataset, but treats
# (PID,cond) as the level-2 unit and ignores the L3 grouping. The unit
# count we expect is 48 (PID × cond combinations) at level 2 and 288 at
# level 1.
#
# We construct a synthetic level-2 ID by concatenating PID and cond into
# one numeric column ("uid"), so the wrapper treats each (PID,cond) as a
# distinct level-2 unit.
# =============================================================================

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

# Read the bundled L1 data, derive a numeric uid for the L2 unit
l1 <- hlm_load(file.path(HERE, "data", "level1_arousal_n16.csv"))
l1$uid <- as.integer(as.factor(paste(l1$PID, l1$cond, sep = "_")))

# Build an L2 frame with one row per uid, carrying the cond as an analysis var
l2 <- hlm_load(file.path(HERE, "data", "level2_hot_cold_long_n16.csv"))
l2$uid <- as.integer(as.factor(paste(l2$PID, l2$cond, sep = "_")))
l2 <- l2[!duplicated(l2$uid), ]  # 48 rows expected

mdm <- hlm_build_mdm2(
  level1 = l1,
  level2 = l2,
  l2_id  = "uid",
  workspace = "n16_hlm2_test",
  mdm_name  = "n16h2"
)

cat("HLM2 MDM built.\n")
cat("  rows: l1=", mdm$rows$l1, " l2=", mdm$rows$l2, "\n", sep="")
cat("  expected: l1=288 l2=48\n")

# Simple model: AROUSAL ~ D1_obs (random intercept, fixed slope)
spec <- hlm2_spec(
  outcome       = "arousal",
  l1_predictors = c("D1_obs"),
  l2 = list(
    INTRCPT = list(predictors = "hot", random = TRUE),
    D1_obs  = list(random = FALSE)
  ),
  numit = 100
)

cat("\n--- Generated .hlm command file ---\n")
cat(hlm_render_hlm2(
      spec, basename(mdm$mdm),
      paste0(mdm$workspace$win, "\\n16h2_graph.geq"),
      paste0(mdm$workspace$win, "\\fit.html")
    ), "\n")
cat("--- end ---\n\n")

result <- hlm_fit(mdm, spec, model_name = "fit")
cat("\n")
print(result)
