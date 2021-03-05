
#' Example prompt that shows the current memory usage of the R process
#'
#' @details
#' `prompt_mem()` is a simple example prompt that shows the physical memory
#' allocated by the current process.
#'
#' @param ... Ignored.
#' @return `prompt_mem()` returns the formatted prompt in a string.
#'
#' @family example prompts
#' @export
#' @examplesIf ps::ps_is_supported()
#' cat(prompt_mem())

prompt_mem <- function(...) {
  mem <- memory_usage()
  paste0(mem$formatted, " ", symbol$pointer, " ")
}

#' @details
#' `memory_usage()` is a utility function that shows memory information
#' about the current R process and the system. You can use it to create a
#' custom prompt.
#'
#' @return `memory_usage()` returns a list with entries:
#'   * `bytes`: the number of bytes of memory the current process uses.
#'     This is the 'Resident Set Size', see [ps::ps_memory_info()].
#'   * `formatted`: string that formats `bytes` nicely, with the appropriate
#'     unit.
#'   * `total`: Total physical memory. See [ps::ps_system_memory()].
#'   * `avail`: the memory that can be given instantly to processes
#'      without the system going into swap. See [ps::ps_system_memory()].
#'   * `percent`: Percentage of memory that is taken. See
#'      [ps::ps_system_memory()].
#' @export
#' @rdname prompt_mem
#' @examplesIf ps::ps_is_supported()
#' memory_usage()

memory_usage <- function() {
  bytes <- ps::ps_memory_info()[["rss"]]
  fmt <- format_bytes$pretty_bytes(bytes)
  sys <- ps::ps_system_memory()
  list(
    bytes = bytes,
    formatted = fmt,
    total = sys$total,
    avail = sys$avail,
    percent = sys$percent
  )
}
