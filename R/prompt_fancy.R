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
#' @importFrom crayon green red blue cyan
#' @export

prompt_fancy <- function(expr, value, ok, visible) {

  status <- ok_status(ok)

  mem <- memory_usage()

  pkg <- if (using_devtools()) crayon::blue(devtools_package()) else ""

  git <- git_info()

  cat(
    status,
    mem,
    pkg,
    git,
    sep = ""
  )
}


