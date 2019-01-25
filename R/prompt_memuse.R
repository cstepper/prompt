prompt_memuse_factory <- function() {
  size <- 0
  unit <- "MiB"

  function(...) {
    current <- memuse::Sys.procmem()[[1]]
    size <<- memuse::mu.size(current)
    unit <<- memuse::mu.unit(current)

    cat(
      crayon::cyan(
        paste(round(size, 1), unit)
        )
      )
  }
}

#' Prompt that shows the current memory usage of the R process
#'
#' @param ... Ignored.
#'
#' @family prompts
#' @export

prompt_memuse <- prompt_memuse_factory()
