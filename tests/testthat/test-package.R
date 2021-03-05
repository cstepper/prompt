
test_that("update_callback", {
  args <- NULL
  mockery::stub(
    update_callback, "update_prompt",
    function(...) args <<- list(...)
  )
  update_callback(quote(expr), "value", TRUE, TRUE)
  expect_equal(args, list(quote(expr), "value", TRUE, TRUE))
})

test_that("update_prompt", {
  withr::local_options(prompt = "> ")
  old <- prompt_env$prompt
  on.exit(prompt_env$prompt <- old, add = TRUE)
  prompt_env$prompt <- "bah"

  update_prompt()
  expect_equal(getOption("prompt"), "bah")

  prompt_env$prompt <- function(...) "bah2"
  update_prompt()
  expect_equal(getOption("prompt"), "bah2")
})

test_that("set_prompt", {
  ret <- callr::r(function() {
    prompt::set_prompt(function(...) "bah> ")
    getOption("prompt")
  }, env = c(callr::rcmd_safe_env(), PROMPT_NO_GLOBAL = "true"))
  expect_equal(ret, "bah> ")
})

test_that("suspend, restore, toggle", {
  ret <- callr::r(function() {
    prompt::set_prompt(function(...) "bah> ")
    p1 <- getOption("prompt")
    prompt::suspend()
    prompt::suspend()
    p2 <- getOption("prompt")
    prompt::restore()
    prompt::restore()
    p3 <- getOption("prompt")
    prompt::toggle()
    p4 <- getOption("prompt")
    prompt::toggle()
    p5 <- getOption("prompt")
    list(p1, p2, p3, p4, p5)
  }, env = c(callr::rcmd_safe_env(), PROMPT_NO_GLOBAL = "true"))

  expect_equal(ret, list("bah> ", "> ", "bah> ", "> ", "bah> "))
})
