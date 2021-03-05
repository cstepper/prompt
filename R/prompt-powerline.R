
#' This is a Powerline-like prompt
#'
#' It is inspired by the <https://github.com/powerline/powerline>
#' project. This prompt uses some Unicode glyphs that work best
#' with the fonts specifically modified for Powerline:
#' https://github.com/powerline/fonts
#' It also works best on consoles that support ANSI colors.
#'
#' @param parts List of strings and functions. Strings are for the
#' built-in powerline pieces, functions are arbitrary functions with
#' four parameters: `expr`, `value`, `ok` and `visible`, and they should
#' return a character string. The builtin pieces are:
#' * `status`: Status of last command, a red or green box.
#' * `memory`: Memory usage of the R process.
#' * `loadavg`: The load average of the system, see [ps::ps_loadavg()].
#' * `path`: Current working directory.
#' * `devtools`: Package(s) loaded by [pkgload::load_all()] or the same
#'   function of devtools.
#' * `git`: git status, see [prompt_git()].
#' @param colors Colors of the parts. Builtin parts have default colors,
#' but you can change them.
#'
#' @return `make_prompt_powerline()` returns a function that you can
#' use with [set_prompt()].
#'
#' @family example prompts
#' @export

new_prompt_powerline <- function(
  parts = list("status", "memory", "loadavg", "path", "devtools", "git"),
  colors = powerline_colors(parts)) {

  stopifnot(
    is.list(parts),
    is.null(colors) || is.character(colors),
    is.null(colors) || length(colors) == length(parts)
  )

  chr <- vapply(parts, is.character, logical(1))
  parts[chr] <- powerline_funcs[unlist(parts[chr])]

  function(expr, value, ok, visible) {
    out <- lapply(parts, function(p) p(expr, value, ok, visible))

    # Drop empty ones
    colors <- colors[nzchar(out)]
    out <- out[nzchar(out)]

    # Bg colors
    nosep <- vapply(out, function(x) isTRUE(attr(x, "nosep")), logical(1))
    out <- mapply(out, colors, FUN = function(x, c) get_col(bg = c)(x))

    # Separators
    sep <- vapply(seq_along(out), FUN.VALUE = character(1), function(i) {
      if (nosep[i]) return("")
      sep <- "\ue0b0"
      if (!is.na(colors[i])) sep <- get_col(colors[i])(sep)
      if (!is.na(colors[i + 1])) sep <- get_col(bg = colors[i + 1])(sep)
      sep
    })

    paste0(
      "\n",
      paste(rbind(out, sep), collapse = ""), "\n",
      symbol$pointer, " "
    )
  }
}

powerline_colors <- function(parts = NULL) {
  parts <- parts %||% eval(formals(new_prompt_powerline)$parts)
  default_map <- c(
    memory = "#404040",
    loadavg = "#404040",
    path = "#0055d4",
    devtools = "gold4",
    git = "#7d26cd"
  )
  cols <- rep(NA_character_, length(parts))
  for (i in seq_along(parts)) {
    if (is.character(parts[[i]])) cols[[i]] <- default_map[ parts[[i]] ]
  }

  cols
}

#' @importFrom cli combine_ansi_styles make_ansi_style

get_col <- function(fg = NA_character_, bg = NA_character_) {
  key <- paste0(fg, "-", bg)
  if (is.null(prompt_env$colors[[key]])) {
    bgs <- if (!identical(bg, NA_character_)) make_ansi_style(bg, bg = TRUE)
    args <- drop_nulls(list(if (!identical(fg, NA_character_)) fg, bgs))
    prompt_env$colors[[key]] <- do.call("combine_ansi_styles", args)
  }
  prompt_env$colors[[key]]
}

powerline_status <- function(expr, value, ok, visible) {
  col <- if (ok) "green4" else "#b90e0a"
  bg <- get_col(bg = col)
  nx <- get_col(bg = "#404040")
  fg <- get_col(fg = col)
  structure(paste0(bg(" "), nx(fg("\ue0b0"))), nosep = TRUE)
}

powerline_memory <- function(expr, value, ok, visible) {
  structure(paste0(" ", memory_usage()$formatted, " "), nosep = TRUE)
}

powerline_loadavg <- function(expr, value, ok, visible) {
  load <- ps::ps_loadavg()
  paste0("\u2317 ", paste(format(round(load, 1)), collapse = "\u00b7"), " ")
}

powerline_path <- function(expr, value, ok, visible) {
  paste0(" ", gsub("/", " / ", format_wd(), fixed = TRUE), "  ")
}

powerline_devtools <- function(expr, value, ok, visible) {
  pkg <- paste(devtools_packages(), collapse = " / ")
  if (nzchar(pkg)) paste0(" \u229e ", pkg, "  ") else ""
}

powerline_git <- function(expr, value, ok, visible) {
  if (!is_git_dir()) return("")
  remote <- git_remote_status()
  gitx <- paste0(
    if (git_dirty() == "*") "\u002a ",
    if (git_has_stash()) "\u2261 ",
    if (!is.na(remote[2]) && remote[2] > 0) paste0("\u21E3", remote[2], " "),
    if (!is.na(remote[1]) && remote[1] > 0) paste0("\u21E1", remote[1], " "),
    ""
  )
  paste0(" \ue0a0 ", git_branch(), " ", gitx, if (!nzchar(gitx)) " ")
}

powerline_funcs <- list(
  status = powerline_status,
  memory = powerline_memory,
  loadavg = powerline_loadavg,
  path = powerline_path,
  devtools = powerline_devtools,
  git = powerline_git
)


format_wd <- function() {
  wd <- getwd()
  wd <- gsub("\\", "/", wd, fixed = TRUE)
  home <- path.expand("~")
  if (wd == home) {
    "~"
  } else if (substr(wd, 1, nchar(home)) == home) {
    paste0("~", substr(wd, nchar(home) + 1L, nchar(wd)))
  } else {
    wd
  }
}
