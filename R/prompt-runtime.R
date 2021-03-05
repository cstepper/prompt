
prompt_runtime_factory <- function() {

  last <- proc.time()
  last[] <- NA_real_

  function(...) {
    diff <- proc.time() - last
    elapsed <- sum(diff) - diff["elapsed"]
    last <<- proc.time()
    if (!is.na(elapsed) && elapsed > 1) {
      paste0(round(elapsed), "s > ")
    } else {
      "> "
    }
  }
}

#' A prompt that shows the CPU time used by the last top level expression
#'
#' @param ... Arguments, ignored.
#' @return The prompt.
#''
#' @family example prompts
#' @export

prompt_runtime <- prompt_runtime_factory()
