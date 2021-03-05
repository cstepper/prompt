
test_that("grey", {
  withr::local_options(cli.num_colors = 256)
  expect_true(cli::ansi_has_any(grey()("foobar")))
  withr::local_options(cli.num_colors = 1)
  expect_false(cli::ansi_has_any(grey()("foobar")))
})

test_that("prompt_fancy", {
  withr::local_options(cli.unicode = FALSE)
  withr::local_options(cli.num_colors = 1)
  mockery::stub(prompt_fancy, "devtools_packages", character())
  mockery::stub(prompt_fancy, "memory_usage", list(formatted = "194.29 MB"))
  mockery::stub(prompt_fancy, "ps::ps_loadavg", c(1.31, 1.89, 2.001))
  mockery::stub(prompt_fancy, "git_info", "gitinfo")
  mockery::stub(prompt_fancy, "has_emoji", FALSE)
  expect_snapshot(prompt_fancy(NULL, NULL, TRUE, TRUE))

  mockery::stub(prompt_fancy, "devtools_packages", "mypkg")
  expect_snapshot(prompt_fancy(NULL, NULL, TRUE, TRUE))
})

test_that("git_info", {
  mockery::stub(git_info, "git", structure(1, "status" = 10))
  expect_equal(git_info(), "")

  mockery::stub(git_info, "git", structure(1, "status" = 0))
  mockery::stub(git_info, "is_git_dir", FALSE)
  expect_equal(git_info(), "")

  mockery::stub(git_info, "is_git_dir", TRUE)
  mockery::stub(git_info, "git_branch", "br")
  mockery::stub(git_info, "git_dirty", "*")
  mockery::stub(git_info, "git_arrows", "arr")
  expect_equal(git_info(), "br*arr")
})

test_that("has_emoji", {
  he <- has_emoji()
  expect_true(inherits(he, "logical"))
  expect_equal(length(he), 1)
})
