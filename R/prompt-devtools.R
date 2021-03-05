
#' Example prompt that shows the package being developed with devtools
#'
#' If git is installed and the current directory is part of a git tree,
#' then also shows all information from \code{\link{prompt_git}}.
#'
#' @param ... Ignored.
#' @return `prompt_devtools()` returns the prompt string.
#'
#' @family example prompts
#' @export

prompt_devtools <- function(...) {
  pdev <- paste(devtools_packages(), collapse = "+")
  pgit <- git_info()
  paste0(
    pdev,
    if (nzchar(pdev) && nzchar(pgit)) " / ",
    pgit,
    " > "
  )
}

#' @export
#' @rdname prompt_devtools
#' @return `devtools_packages()` returns the packages loaded by
#' devtools/pkgload.

devtools_packages <- function() {
  if (!"devtools_shims" %in% search()) return(character())
  packages <- vapply(
    loadedNamespaces(),
    function(x) !is.null(pkgload::dev_meta(x)), logical(1)
  )
  names(packages)[packages]
}
