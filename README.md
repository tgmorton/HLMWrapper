# hlmwrap

An R package that drives **HLM 8.2** (the Windows statistical program by SSI)
headlessly through Apple's Game Porting Toolkit / Whisky on macOS. Reads CSV
or `.sav` data, builds HLM's binary `.mdm` cache, fits 2- or 3-level models,
and returns tidy result tables — all from a single R function call.

## Why this exists

HLM 8.2 is a powerful but Windows-only stat program with a clunky GUI and
several hidden gotchas. Working with it from a Mac normally means clicking
through dialogs in Whisky for every model. This wrapper:

- **Hides Wine/Whisky** completely
- **Bypasses the silent-drop bug** in HLM3/HLM4 where unsorted level-1 data
  causes ~½ the level-2 units to vanish without any error message
  (full details in `re/notes/01_bug_report.md`)
- **Handles HLM's quirks** for you: variable name 8-char limit + collision
  detection, uncompressed `.sav` writing (HLM mis-reads compressed files
  to N=1), unknown-directive whitelist (unknown directives hang the solver
  on `getchar()`), automatic level-1 sort
- **Returns tidy results** as tibbles instead of HTML

## Coverage

| Solver | Wrapper status | Notes |
|---|---|---|
| **HLM2** (2-level linear) | ✅ Full | `hlm_build_mdm2`, `hlm2_spec`, end-to-end smoke test |
| **HLM3** (3-level linear) | ✅ Full | `hlm_build_mdm3`, `hlm3_spec`, end-to-end smoke test |
| HLM4 (4-level linear) | ⏳ Planned | Binary RE'd; directive vocabulary mapped (uses `FIXTAUPI/BETA/GAMMA`); spec function not yet written |
| HCM2 / HCM3 (cross-classified) | ⏳ Planned | Binaries RE'd; uses `ROWCOL:`, `CLUS:`; spec functions not yet written |
| HMLM / HMLM2 (multivariate) | ⏳ Planned | Binaries RE'd; uses `R_E_MODEL:`, `UNRESTRICTED:`; wide-format data |
| HLMHCM (combined) | ⏳ Planned | Binaries RE'd; hybrid `level2:`+`rowcol:`, `FIXTAU/FIXDELTA/FIXOMEGA` |
| Non-Normal outcomes (HGLM) | ⏳ Planned | Parser supports `LAPLACE:`, `AGQ:`, `NONLIN:BERNOULLI`, etc.; needs `nonlin` spec arg |

The wrapper is **production-ready for HLM2 and HLM3 linear models**. Other
solvers can still be invoked via raw `hlm_run()` calls if you craft the
.hlm file yourself.

## Quick start

```r
source("/path/to/hlmwrap/R/99_load.R")

# 1. Build a 3-level MDM from any combination of CSV / .sav / data.frame
mdm <- hlm_build_mdm3(
  level1 = "trials.csv",        # OR a data.frame, OR a .sav path
  level2 = "conditions.csv",
  level3 = "people.csv",
  l3_id  = "PID",                # level-3 ID column name
  l2_id  = "cond",               # level-2 ID column name
  workspace = "my_run"           # short name; files go in the bottle
)

# 2. Specify the model. Defaults: random intercept at every level, all slopes fixed.
spec <- hlm3_spec(
  outcome       = "AROUSAL",
  l1_predictors = c("D1_OBS", "D1_PART"),
  l2 = list(
    INTRCPT = list(predictors = "HOT", random = TRUE),
    D1_OBS  = list(random = FALSE),
    D1_PART = list(random = FALSE)
  ),
  l3 = list(
    "INTRCPT/INTRCPT" = list(predictors = "HC_C_Z_N", random = TRUE)
  )
)

# 3. Fit it.
result <- hlm_fit(mdm, spec)
print(result)

result$fixed_effects        # tibble with γ coefficients, SE, t, df, p
result$variance_components  # list of tibbles (one per level pair for HLM3)
result$sample_sizes         # list(level1=288, level2=48, level3=16)
result$warnings             # any free-text warnings extracted from the HTML
result$files$html           # path to the raw HLM .html output (if you want it)
```

For HLM2 the API mirrors HLM3 but with `hlm_build_mdm2()` and `hlm2_spec()`
(no `l3` argument). See `examples/test_n16.R` and `examples/test_n16_hlm2.R`.

## Centering predictors

```r
hlm3_spec(
  ...,
  l2 = list(
    INTRCPT = list(
      predictors = list(list(name = "HOT", center = "grand")),
      random = TRUE
    )
  )
)
```

`center` accepts `"none"`, `"group"` (group-mean centering, HLM code `,1`),
or `"grand"` (grand-mean, code `,2`). Bare strings (`predictors = "HOT"`)
mean uncentered.

---

# Setup on a new machine

The rest of this file is an instruction set for an autonomous agent
(Claude Code or similar) to bootstrap `hlmwrap` on a fresh macOS machine.
Follow it top to bottom. Anywhere you see `[CHECK]`, verify the result
before proceeding. Anywhere you see `[USER]`, ask the human and stop until
they answer.

---

## Required files in this folder

A complete `hlmwrap/` distribution contains:

```
hlmwrap/
├── README.md             ← this file
├── setup.R               ← environment check + R-package installer
├── R/
│   ├── 00_wine.R         ← Wine/Whisky bottle plumbing
│   ├── 01_data.R         ← data loading, sort fix, .sav writing
│   ├── 02_mdmt.R         ← .mdmt template generation + hlm-w invocation
│   ├── 03_hlm.R          ← .hlm command file generation, model spec API
│   ├── 04_fit.R          ← top-level orchestration (build_mdm, fit)
│   ├── 05_parse.R        ← HTML output parsing (rvest)
│   └── 99_load.R         ← sourceable entry point
└── examples/
    └── test_n16.R        ← end-to-end test
```

You also need (one of):

- `HLMInstaller.exe` somewhere on disk (the official SSI Windows installer);
  the user will tell you where, OR
- A Whisky bottle that already has HLM 8.2 installed.

---

## Step 1 — System sanity

```bash
sw_vers                                # macOS 11+ recommended
uname -m                               # arm64 or x86_64
which Rscript                          # R must be installed
```

`[CHECK]` macOS, R is on PATH. If R is missing, install it via Homebrew:

```bash
brew install --cask r
```

---

## Step 2 — Install Whisky (if absent)

```bash
ls /Applications/Whisky.app 2>/dev/null
```

If absent:

```bash
brew install --cask whisky
```

`[CHECK]` `/Applications/Whisky.app` exists. Whisky is deprecated upstream
but still works for our purposes.

---

## Step 3 — First-run Whisky to bootstrap GPTK

The first time Whisky runs it downloads Apple's Game Porting Toolkit
(several hundred MB). This must be done once before any `wine64` invocation
will work.

```bash
open -a Whisky
```

`[USER]` Tell the user: "Whisky needs to download GPTK on first run. Open
the Whisky window, click through any setup prompts, and tell me when the
main bottle list is visible." Wait for confirmation.

`[CHECK]` After GPTK install:

```bash
ls "$HOME/Library/Application Support/com.isaacmarovitz.Whisky/Libraries/Wine/bin/wine64"
```

Should exist.

---

## Step 4 — Create a bottle (if none has HLM)

Check for an existing bottle with HLM 8.2:

```bash
find "$HOME/Library/Containers/com.isaacmarovitz.Whisky/Bottles" \
  -path '*Program Files/HLM 8.2/hlm3.exe' -type f 2>/dev/null
```

If output is empty, create a new bottle:

`[USER]` Tell the user: "I need a Whisky bottle with HLM installed. Please
do this manually in the Whisky app: click the **+** button → name it (e.g.
`HLM`) → leave Win10 default → wait for setup to finish → drag
`HLMInstaller.exe` into the bottle's window → click Run → step through the
SSI installer (it'll prompt for a license code; use yours)."

After they confirm, re-run the find command above. You should now see one
match. Note the bottle UUID (the directory name above `drive_c/`).

`[CHECK]` `find` returns a path containing a bottle UUID like
`AE1E551C-772E-42D5-B18F-0B9E4D181E91`.

---

## Step 5 — Verify hlmwrap files

```bash
HLMWRAP=/path/to/hlmwrap     # update to actual location
ls "$HLMWRAP"/{README.md,setup.R}
ls "$HLMWRAP"/R/*.R
ls "$HLMWRAP"/examples/test_n16.R
```

`[CHECK]` All present. If any are missing the user must provide the full
folder.

---

## Step 6 — Install R packages and verify

```bash
Rscript "$HLMWRAP/setup.R"
```

`setup.R` will:
1. Verify Whisky and the wine64 binary
2. Auto-detect a bottle that contains HLM 8.2
3. Install missing R packages: `haven processx tibble rvest xml2 readr`
4. Source `hlmwrap`
5. Optionally run an end-to-end smoke test if example data is present

`[CHECK]` `setup.R` ends with `READY` and exits 0.

If it ends with `NOT READY`, read the `[FAIL]` lines and fix each one in
order. Common failures:

- "No Whisky bottle has HLM 8.2 installed" → repeat Step 4
- R package install failure → retry with a fresh R session, check internet
- "wine64 not found" → repeat Step 3 (open Whisky once)

---

## Step 7 — Try the end-to-end test

If the user has the n16 example data (CSV files for level 1, 2, 3) at a
known path, run `examples/test_n16.R`. Otherwise construct a tiny test
yourself:

```r
source("R/99_load.R")
mdm <- hlm_build_mdm3(
  level1 = your_l1_csv_or_dataframe,
  level2 = your_l2_csv_or_dataframe,
  level3 = your_l3_csv_or_dataframe,
  l3_id  = "PID",
  l2_id  = "cond",
  workspace = "smoke",
  mdm_name  = "smoke"
)
spec <- hlm3_spec(
  outcome = "AROUSAL",
  l1_predictors = NULL  # unconditional means model
)
result <- hlm_fit(mdm, spec)
print(result)
```

`[CHECK]` `result$success` is `TRUE` and `result$sample_sizes` matches the
row counts of the input files.

---

## Step 8 — Hand off to user

Print the location of `hlmwrap`, the bottle UUID, and a one-line summary
of what they can now do (e.g. "build MDMs and fit HLM2/HLM3 models from R
without ever opening the GUI").

---

## Things you should know

- **The silent-drop bug**: HLM3 silently drops level-2 units if the
  level-1 file is not sorted by `(level3id, level2id)`. The wrapper sorts
  automatically and bypasses this. See `re/notes/01_bug_report.md`.
- **Variable name length**: HLM truncates names to 8 characters silently.
  The wrapper truncates and detects collisions explicitly.
- **`.sav` compression**: HLM mis-reads byte-compressed `.sav` files
  (reports N=1). The wrapper writes uncompressed (`compress = "none"`).
- **`hlm3.exe -w` cold start**: in a *cold* wine session, hlm3.exe -w
  fails with exit 136. The wrapper auto-spawns whlm.exe in the background
  to keep the wineserver warm. (A future version will write .mdm directly
  in R and skip this entirely.)
- **Unknown directives hang the solver**: hlm3.exe waits for `getchar()`
  on unknown directives. The wrapper enforces a directive whitelist.

---

## Environment overrides

| Env var | Purpose | Default |
|---|---|---|
| `HLMWRAP_BOTTLE` | Force a specific bottle UUID instead of auto-detecting | (auto) |
