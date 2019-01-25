#' A fancy prompt, showing probably too much information
#'
#' It also uses color, on terminals that support it.
#' Is shows: \itemize{
#'   \item Status of last command.
#'   \item Memory usage of the R process.
#'   \item Package being developed using devtools, if any.
#'   \item Git branch and state of the working tree if within a git tree.
#' }
#'
#' @param expr Evaluated expression.
#' @param value Its value.
#' @param ok Whether the evaluation succeeded.
#' @param visible Whether the result is visible.
#'
#' @family prompts
#' @importFrom crayon green red
#' @export
prompt_fancy <- function(expr, value, ok, visible) {

  status <- ok_status(ok)

  memuse <- memory_usage()

  gitinfo <- git_info()
  if (gitinfo != "") {
    gitinfo = grey()(paste0(" | ", gitinfo))
  }

  cat(
    status,
    memuse,
    gitinfo,
    " ",
    sep = ""
  )
}


