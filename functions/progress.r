# ---------------------------------------------------------------------------
# progress(): timestamped progress line, flushed to the console immediately
#
# Uses message() (unbuffered) so lines appear the instant they run, unlike
# cat() which VS Code can buffer. A ticking timestamp is the tell that work is
# moving rather than frozen. Drop progress("...") calls into any slow function.
# ---------------------------------------------------------------------------
progress <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
}