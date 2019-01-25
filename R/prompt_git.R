#' A git prompt, showing some information about the current git state
#'
#' Is shows: \itemize{
#'   \item Git branch and state of the working tree if within a git tree.
#' }
#'
#'
#' @family prompts
#' @export
prompt_git <- function(...) {

  gitinfo <- git_info()

  cat(gitinfo, " ")

}
