## prompt error hook ----

prompt_error_hook <- function() {
  update_prompt(expr = NA, value = NA, ok = FALSE, visible = NA)

  orig <- prompt_env$error
  if (!is.null(orig) && is.function(orig)) orig()
  if (!is.null(orig) && is.call(orig)) eval(orig)
}

## error helpers ----

# checks status (OK or error) of the last expression
ok_status <- function(ok) {

  status <- if (ok) {
    paste0(crayon::green(clisymbols::symbol$tick), grey()(" | "))
  } else {
    paste0(crayon::red(clisymbols::symbol$cross), grey()(" | "))
  }

  status
}



## memory usage helpers ----

memory_usage <- function() {
  if (!requireNamespace("memuse", quietly = TRUE)) return("")
  current <- memuse::Sys.procmem()[[1]]
  size <- memuse::mu.size(current)
  unit <- memuse::mu.unit(current)

  grey()(paste0(round(size, 1), " ", unit, " | "))
}


## git helpers ----

# search for git exe on system - return path
git_path <- function(git_binary_name = NULL) {

  # Use user supplied path
  if (!is.null(git_binary_name)) {
    if (!file.exists(git_binary_name)) {
      stop("Path ", git_binary_name, " does not exist", .call = FALSE)
    }
    return(git_binary_name)
  }

  # On Windows, look in common locations
  if (os_type() == "windows") {
    look_in <- c(
      "C:/Program Files/Git/bin/git.exe",
      "C:/Program Files (x86)/Git/bin/git.exe"
    )
    found <- file.exists(look_in)
    if (any(found)) return(look_in[found][1])
  }

  # Look on path
  git_path <- Sys.which("git")[[1]]
  if (git_path != "") return(git_path)

  NULL
}


# check if git path exists
check_git_path <- function(git_binary_name = NULL) {

  path <- git_path(git_binary_name)

  if (is.null(path)) {
    stop("Git does not seem to be installed on your system.", call. = FALSE)
  }

  path
}


# execute git commands
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

  if (methods::is(result, "error")) {
    result <- structure("", status = 1)
  } else {
    attr(result, "status") <- attr(result, "status") %||% 0
  }

  result
}


# check if current directory is version controlled with git
is_git_dir <- function() {
  status <- git("status")
  attr(status, "status") == 0
}


## git prompts ----

# check currrent git branch (fails before first commit --> return "master" here)
git_branch <- function() {
  status <- git("rev-parse --abbrev-ref HEAD")
  if (attr(status, "status") != 0) "master" else status
}

# check if any modifications are not staged (if the repo is dirty)
git_dirty <- function() {
  status <- git("diff --no-ext-diff --quiet --exit-code")
  if (attr(status, "status") != 0) "*" else ""
}

# check whether there are commits to push or pull to the default remote
git_arrows <- function() {
  res <- ""

  status <- git("rev-parse --abbrev-ref @{u}")
  if (attr(status, "status") != 0) return(res)

  status <- git("rev-list --left-right --count HEAD...@{u}")
  if (attr(status, "status") != 0) return(res)
  lr <- scan(text = status, quiet = TRUE)
  if (lr[2] != 0) return(clisymbols::symbol$arrow_down)
  if (lr[1] != 0) return(clisymbols::symbol$arrow_up)

}

# combine all git checks
git_info <- function() {
  if (attr(git("--version"), "status") != 0) return("")
  if (!is_git_dir()) return("")

  grey()(
    paste0(
      git_branch(),
      git_dirty(),
      git_arrows()
    )
  )
}




## devtools utils ----

using_devtools <- function() {
  "devtools_shims" %in% search()
}

devtools_package <- function() {
  tryCatch(
    devtools::as.package(".")$package,
    error = function(e) "<unknown pkg>"
  )
}


## misc utils ----

grey <- function() {
  crayon::make_style("grey70")
}


is.string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

with_something <- function(set, reset = set) {
  function(new, code) {
    old <- set(new)
    on.exit(reset(old))
    force(code)
  }
}

in_dir <- with_something(setwd)

os_type <- function() .Platform$OS.type
