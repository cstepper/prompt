#' A prompt that shows the status (OK or error) of the last expression
#'
#' @param expr Evaluated expression.
#' @param value Its value.
#' @param ok Whether the evaluation succeeded.
#' @param visible Whether the result is visible.
#'
#' @importFrom clisymbols symbol
#' @family example prompts
#' @export

prompt_error <- function(expr, value, ok, visible) {
  if (ok) {
    # paste0(clisymbols::symbol$tick, " ", clisymbols::symbol$pointer, " ")
    cat(crayon::green(clisymbols::symbol$tick))
  } else {
    # paste0(clisymbols::symbol$cross, " ", clisymbols::symbol$pointer, " ")
    cat(crayon::red(clisymbols::symbol$cross))
  }
}
