# =============================================================================
# Sourceable entry point.
# =============================================================================
#
# Use this until we ship as a real R package:
#
#   source("/Users/thomasmorton/Downloads/HLM/re/hlmwrap/R/99_load.R")
#
# It loads all hlmwrap files in the right order.

local({
  here <- dirname(sys.frame(1)$ofile)
  for (f in c("00_wine.R", "01_data.R", "02_mdmt.R", "03_hlm.R",
              "04_fit.R", "05_parse.R", "06_mdm_write.R")) {
    p <- file.path(here, f)
    if (file.exists(p)) source(p) else warning("missing: ", p)
  }
  message("hlmwrap loaded — try `?hlm_build_mdm3` (no help yet) or run examples/test_n16.R")
})
