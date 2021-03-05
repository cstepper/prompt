
#' An example 'git' prompt
#'
#' @details
#' `prompt_git()` is a prompt with information about the git repository
#' in the current working directory. It shows the current branch, whether
#' there are commits to push or pull to the default remote, and whether
#' the working directory is dirty.
#'
#' @param ... Unused.
#' @return `prompt_git()` returns the prompt as a string.
#'
#' @family example prompts
#' @export

prompt_git <- function(...) {

  if (!is_git_dir()) return ("> ")

  paste0(
    git_branch(),
    git_dirty(),
    git_arrows(),
    " > "
  )
}

#' @details
#' `is_git_dir()` checks whether the working directory is in a git tree.
#' If git is not installed, then it always returns `FALSE`.
#'
#' @return
#' `is_git_dir()` returns a logical scalar.
#'
#' @export
#' @rdname prompt_git

is_git_dir <- function() {
  status <- git("status")
  attr(status, "status") == 0
}

## It fails before the first commit, so we just return "main" there

#' @details
#' `git_branch()` returns the name of the current branch.
#'
#' @return
#' `git_branch()` returns a string. If the repository has no commits, then
#' it returns `"main"`. Note that if git is not available, or fails for
#' any reason, it will also return `"main"`, so you might want to call
#' `is_git_dir()` as well.
#' @export
#' @rdname prompt_git

git_branch <- function() {
  status <- git("rev-parse --abbrev-ref HEAD")
  if (attr(status, "status") != 0) "main" else c(status)
}

#' @details
#' `git_arrows()` checks the status of the local tree compared to the
#' configured remote.
#'
#' @return
#' `git_arrows()` returns a string that has a down arrow if the remote
#' has extra commits, and a down arrow if the local tree has extra commits
#' compared to the remote. Or both arrows for diverged branches. If it is
#' not the empty string then it adds a leading space character.
#'
#' @importFrom cli symbol
#' @export
#' @rdname prompt_git

git_arrows <- function() {
  res <- ""
  lr <- git_remote_status()
  if (!is.na(lr[2]) && lr[2] != 0) res <- paste0(res, symbol$arrow_down)
  if (!is.na(lr[1]) && lr[1] != 0) res <- paste0(res, symbol$arrow_up)
  if (res != "") paste0(" ", res) else res
}

#' @details
#' `git_remote_status()` checks the status of the local tree, compared to
#' a configured remote.
#'
#' @return
#' `git_remote_status()` returns a numeric vector of length two. The first
#' number is the number of extra commits in the local tree. The second
#' number is the number of extra commits in the remote. If there is no
#' remote, or git errors, it returns a vector of two `NA`s.
#'
#' @export
#' @rdname prompt_git

git_remote_status <- function() {
  res <- ""

  status <- git("rev-parse --abbrev-ref @'{u}'")
  if (attr(status, "status") != 0) return(c(NA_integer_, NA_integer_))

  status <- git("rev-list --left-right --count HEAD...@'{u}'")
  if (attr(status, "status") != 0) return (c(NA_integer_, NA_integer_)) # nocov
  scan(text = status, quiet = TRUE)
}

#' @details
#' `git_dirty()` checks if the local tree has uncommitted changes.
#' If there are, it returns `"*"`. Note that it also returns `"*"` on a
#' git error, so you might want to use `is_git_dir()` as well.
#'
#' @return
#' `git_dirty()` returns a character string, `"*"` or `""`.
#'
#' @export
#' @rdname prompt_git

git_dirty <- function() {
  status <- git("status --porcelain --ignore-submodules -u")
  if (attr(status, "status") != 0) return("") # nocov
  if (length(status) > 0) "*" else ""
}

git_has_stash <- function() {
  out <- git("stash list")
  if (attr(out, "status") != 0) return("FALSE") # nocov
  length(out) > 0
}

git <- function(args, quiet = TRUE, path = ".") {
  full <- paste0(shQuote(check_git_path()), " ", paste(args, collapse = ""))
  if (!quiet) {
    message(full)
  }

  result <- tryCatch(
    suppressWarnings(
      in_dir(path, system(full, intern = TRUE, ignore.stderr = quiet))
    ),
    error = function(x) x
  )

  if (inherits(result, "error")) {
    result <- structure("", status = 1)
  } else {
    attr(result, "status") <- attr(result, "status") %||% 0
  }

  result
}

git_path <- function(git_binary_name = NULL) {
  # Use user supplied path
  if (!is.null(git_binary_name)) {
    if (!file.exists(git_binary_name)) {
      stop("Path ", git_binary_name, " does not exist", .call = FALSE)
    }
    return(git_binary_name)
  }

  # Look on path
  git_path <- Sys.which("git")[[1]]
  if (git_path != "") return(git_path)

  # On Windows, look in common locations
  if (os_type() == "windows") {
    look_in <- c(
      "C:/Program Files/Git/bin/git.exe",
      "C:/Program Files (x86)/Git/bin/git.exe"
    )
    found <- file.exists(look_in)
    if (any(found)) return(look_in[found][1])
  }

  NULL
}

check_git_path <- function(git_binary_name = NULL) {

  path <- git_path(git_binary_name)

  if (is.null(path)) {
    stop("Git does not seem to be installed on your system.", call. = FALSE)
  }

  path
}
