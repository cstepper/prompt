
#' A prompt that shows the status (OK or error) of the last expression
#'
#' @param expr Evaluated expression.
#' @param value Its value.
#' @param ok Whether the evaluation succeeded.
#' @param visible Whether the result is visible.
#' @return `prompt_status()` returns the prompt string.
#'
#' @importFrom cli symbol
#' @family example prompts
#' @export

prompt_status <- function(expr, value, ok, visible) {
  if (ok) {
    paste0(symbol$tick, " ", symbol$pointer, " ")
  } else {
    paste0(symbol$cross, " ", symbol$pointer, " ")
  }
}
