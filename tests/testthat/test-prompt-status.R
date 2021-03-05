
test_that("prompt_status", {
  withr::local_options(cli.unicode = FALSE)
  expect_equal(
    prompt_status(NULL, NULL, TRUE, TRUE),
    "v > "
  )
  expect_equal(
    prompt_status(NULL, NULL, FALSE, TRUE),
    "x > "
  )
})
