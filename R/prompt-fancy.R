#' A fancy prompt, showing probably too much information
#'
#' It also uses color, on terminals that support it.
#' Is shows: \itemize{
#'   \item Status of last command.
#'   \item Memory usage of the R process.
#'   \item Load average of the machine.
#'   \item Package being developed using devtools, if any.
#'   \item Git branch and state of the working tree if within a git tree.
#' }
#'
#' @param expr Evaluated expression.
#' @param value Its value.
#' @param ok Whether the evaluation succeeded.
#' @param visible Whether the result is visible.
#' @return `prompt_fancy()` returns the prompt string.
#'
#' @family example prompts
#' @importFrom cli col_green col_red col_blue
#' @export
prompt_fancy <- function(expr, value, ok, visible) {

  status <- if (ok) col_green(symbol$tick) else col_red(symbol$cross)

  mem <- memory_usage()$formatted

  load <- ps::ps_loadavg()
  dot <- if (cli::is_utf8_output()) "\u00b7" else "-"
  loadstr <- paste(round(load, 1), collapse = dot)

  pkg <- paste(devtools_packages(), collapse = "+")
  cat(
    status,
    memuse,
    gitinfo,
    " ",
    sep = ""
  )
}



  emo <- has_emoji()

  paste0(
    "\n",
    status, " ",
    grey()(mem),
    if (emo) " \U1F4BB " else " / ",
    grey()(loadstr),
    if (nchar(pkg)) if (emo) " \U1F4E6 " else " / ",
    col_blue(pkg),
    if (nzchar(git)) if (emo) " \ue0a0 " else " / ",
    grey()(git),
    "\n",
    symbol$pointer,
    " "
  )
}

git_info <- function() {
  if (attr(git("--version"), "status") != 0) return("")
  if (!is_git_dir()) return("")

  paste0(
    git_branch(),
    git_dirty(),
    git_arrows()
  )
}

has_emoji <- function() {
  l10n_info()$`UTF-8`
}
