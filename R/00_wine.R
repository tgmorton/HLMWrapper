# =============================================================================
# Wine + Whisky bottle paths and process invocation.
# =============================================================================
#
# `hlmwrap` runs HLM 8.2 (a Windows app) via Apple's Game Porting Toolkit /
# Whisky on macOS. All wine-specific awkwardness is contained here.
#
# Conventions:
#   - `mac_path`     : a normal POSIX path the user passes in
#   - `bottle_path`  : a POSIX path inside the bottle's drive_c (still POSIX)
#   - `win_path`     : a `C:\...` style Windows path the wine binary sees

WHISKY_BOTTLES_DIR <- "~/Library/Containers/com.isaacmarovitz.Whisky/Bottles"
WHISKY_WINE_DIR    <- "~/Library/Application Support/com.isaacmarovitz.Whisky/Libraries/Wine"
HLM_INSTALL_WIN    <- "C:\\Program Files\\HLM 8.2"

# Bottle UUID can be set explicitly via env var, otherwise auto-detected as
# the first bottle that contains a working HLM 8.2 install.
.detect_bottle <- function() {
  override <- Sys.getenv("HLMWRAP_BOTTLE", unset = "")
  if (nzchar(override)) return(override)
  bdir <- path.expand(WHISKY_BOTTLES_DIR)
  if (!dir.exists(bdir)) stop("Whisky bottles dir not found: ", bdir,
                              "\nIs Whisky installed?")
  candidates <- list.dirs(bdir, recursive = FALSE, full.names = FALSE)
  for (c in candidates) {
    hlm <- file.path(bdir, c, "drive_c", "Program Files", "HLM 8.2", "hlm3.exe")
    if (file.exists(hlm)) return(c)
  }
  stop("No Whisky bottle contains HLM 8.2.\n",
       "Install HLM 8.2 in a bottle (HLMInstaller.exe), or set the env var ",
       "HLMWRAP_BOTTLE=<uuid> to override.")
}
WHISKY_BOTTLE <- tryCatch(.detect_bottle(), error = function(e) {
  warning(conditionMessage(e), immediate. = TRUE); ""
})

#' Resolve the absolute path to wine64.
hlm_wine_bin <- function() {
  p <- path.expand(file.path(WHISKY_WINE_DIR, "bin", "wine64"))
  if (!file.exists(p))
    stop("wine64 not found at ", p, " — is Whisky installed?")
  p
}

#' Resolve the bottle's drive_c root as a POSIX path.
hlm_bottle_root <- function() {
  p <- path.expand(file.path(WHISKY_BOTTLES_DIR, WHISKY_BOTTLE, "drive_c"))
  if (!dir.exists(p))
    stop("Whisky bottle drive_c not found at ", p)
  p
}

#' Convert a POSIX path inside the bottle (e.g. `<root>/users/Public/.../foo.sav`)
#' to the `C:\...\foo.sav` form that wine programs see.
#' Only works for files actually under the bottle root.
hlm_to_win_path <- function(bottle_path) {
  root <- normalizePath(hlm_bottle_root(), mustWork = TRUE)
  p    <- normalizePath(bottle_path,        mustWork = FALSE)
  if (!startsWith(p, root))
    stop("Path is not inside the bottle: ", p,
         "\n  Bottle root: ", root,
         "\n  Use hlm_workspace() to get a path that lives inside the bottle.")
  rel <- substring(p, nchar(root) + 2L)
  paste0("C:\\", gsub("/", "\\\\", rel))
}

#' Create (or return) a workspace directory inside the bottle for run artifacts.
#' All files used by HLM (input .sav, .mdmt, .hlm, output .mdm, .sts) live here.
#'
#' @param name   short name for the workspace (becomes a subdir of /work/)
#' @param clean  if TRUE, wipe any pre-existing contents
hlm_workspace <- function(name, clean = FALSE) {
  stopifnot(grepl("^[A-Za-z0-9_.-]+$", name))
  ws <- file.path(hlm_bottle_root(), "work", name)
  if (clean && dir.exists(ws)) unlink(ws, recursive = TRUE, force = TRUE)
  if (!dir.exists(ws)) dir.create(ws, recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      mac = ws,
      win = hlm_to_win_path(ws),
      name = name
    ),
    class = "hlm_workspace"
  )
}

#' Ensure whlm.exe is running in the background.
#'
#' Empirically established (re/notes/01_bug_report.md) that hlm3.exe -w mode
#' fails with exit 136 in a cold wine session but succeeds when whlm.exe is
#' alive in the same wineserver. This function launches whlm.exe headless if
#' it isn't already running and returns its PID. Idempotent.
hlm_ensure_whlm_running <- function() {
  pid_file <- file.path(tempdir(), "hlmwrap_whlm.pid")
  if (file.exists(pid_file)) {
    pid <- suppressWarnings(as.integer(readLines(pid_file, n = 1)))
    alive <- tryCatch({
      out <- suppressWarnings(system2("ps", c("-p", pid, "-o", "pid="),
                                      stdout = TRUE, stderr = FALSE))
      length(out) > 0L && !is.na(out[1]) && nchar(out[1]) > 0L
    }, error = function(e) FALSE)
    if (length(pid) && !is.na(pid) && isTRUE(alive)) return(pid)
    # stale pid file — clean it up and fall through to relaunch whlm.exe
    try(file.remove(pid_file), silent = TRUE)
  }
  wine <- hlm_wine_bin()
  prefix <- path.expand(file.path(WHISKY_BOTTLES_DIR, WHISKY_BOTTLE))
  exe <- paste0(HLM_INSTALL_WIN, "\\whlm.exe")
  proc <- processx::process$new(
    command = wine, args = exe,
    env = c("current", WINEDEBUG = "-all", WINEPREFIX = prefix),
    stdin = NULL, stdout = NULL, stderr = NULL
  )
  Sys.sleep(4)  # let it warm up the wineserver
  if (!proc$is_alive())
    warning("whlm.exe failed to launch — -w mode may not work")
  pid <- proc$get_pid()
  writeLines(as.character(pid), pid_file)
  attr(pid, "process") <- proc  # keep R reference alive
  pid
}

#' Stop the background whlm.exe (if any) and clean wineserver.
hlm_stop_whlm <- function() {
  pid_file <- file.path(tempdir(), "hlmwrap_whlm.pid")
  if (file.exists(pid_file)) {
    pid <- as.integer(readLines(pid_file, n = 1))
    if (length(pid) && !is.na(pid))
      suppressWarnings(system2("kill", as.character(pid),
                               stdout = FALSE, stderr = FALSE))
    file.remove(pid_file)
  }
  ws <- file.path(WHISKY_WINE_DIR, "bin", "wineserver")
  ws <- path.expand(ws)
  if (file.exists(ws)) {
    Sys.setenv(WINEPREFIX = path.expand(file.path(WHISKY_BOTTLES_DIR, WHISKY_BOTTLE)))
    suppressWarnings(system2(ws, "-k", stdout = FALSE, stderr = FALSE))
  }
  invisible(NULL)
}

#' Run an HLM solver binary inside the bottle.
#'
#' @param exe    short name of the exe, e.g. "hlm3.exe", "hlm2.exe", "whlm.exe"
#' @param args   character vector of arguments. Win-style paths if needed.
#' @param cwd    POSIX cwd inside the bottle (typically the workspace$mac)
#' @param timeout seconds before killing the process
#' @param need_whlm  if TRUE, ensure whlm.exe is running before invoking (set
#'                   for the -w MDM-build path; not needed for fitting)
#'
#' @return list(stdout, stderr, status, duration)
hlm_run <- function(exe, args = character(), cwd = NULL, timeout = 600L,
                    need_whlm = FALSE) {
  if (need_whlm) hlm_ensure_whlm_running()
  win_exe <- paste0(HLM_INSTALL_WIN, "\\", exe)
  full_args <- c(win_exe, args)
  # processx wants a named character vector AND merges with the parent env
  # only when "current" sentinel is included.
  env <- c("current",
           WINEDEBUG  = "-all",
           WINEPREFIX = path.expand(file.path(WHISKY_BOTTLES_DIR, WHISKY_BOTTLE)))
  if (is.null(cwd)) cwd <- getwd()
  t0 <- Sys.time()
  res <- tryCatch(
    processx::run(
      command = hlm_wine_bin(),
      args    = full_args,
      env     = env,
      wd      = cwd,
      timeout = timeout,
      error_on_status = FALSE
    ),
    error = function(e) list(stdout = "", stderr = conditionMessage(e),
                             status = -1, timeout = TRUE)
  )
  list(
    stdout   = res$stdout %||% "",
    stderr   = res$stderr %||% "",
    status   = res$status %||% -1,
    duration = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

# Tiny null-coalesce for older R
`%||%` <- function(a, b) if (is.null(a)) b else a
