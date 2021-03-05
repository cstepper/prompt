

test_that("prompt_git", {
  withr::local_options(cli.unicode = FALSE)
  mockery::stub(prompt_git, "is_git_dir", FALSE)
  expect_equal(prompt_git(), "> ")

  mockery::stub(prompt_git, "is_git_dir", TRUE)
  mockery::stub(prompt_git, "git_branch", "main")
  mockery::stub(prompt_git, "git_dirty", "*")
  mockery::stub(prompt_git, "git_arrows", " ^v")
  expect_equal(prompt_git(), "main* ^v > ")
})

test_that("is_git_dir", {
  mockery::stub(is_git_dir, "git", structure(1, status = 0))
  expect_true(is_git_dir())
  mockery::stub(is_git_dir, "git", structure(1, status = 1))
  expect_false(is_git_dir())
})

test_that("git_branch", {
  if (Sys.which("git") == "") skip("no git")
  withr::local_dir(withr::local_tempdir())
  expect_equal(git_branch(), "main")
  gert::git_init()
  expect_equal(git_branch(), "main")
  cat("foo\n", file = "foo")
  gert::git_add("foo")
  gert::git_commit("Initial commit", author = "gcs <gcs@gmail.com>")
  expect_equal(git_branch(), gert::git_info()$shorthand)
})

test_that("git_arrows", {
  withr::local_options(cli.unicode = FALSE)
  mockery::stub(git_arrows, "git_remote_status", c(0, 0))
  expect_snapshot(git_arrows())
  mockery::stub(git_arrows, "git_remote_status", c(0, 1))
  expect_snapshot(git_arrows())
  mockery::stub(git_arrows, "git_remote_status", c(1, 0))
  expect_snapshot(git_arrows())
  mockery::stub(git_arrows, "git_remote_status", c(1, 1))
  expect_snapshot(git_arrows())
  mockery::stub(git_arrows, "git_remote_status", c(NA_integer_, NA_integer_))
  expect_snapshot(git_arrows())
})

test_that("git_remote_status", {
  skip_on_cran()
  skip_on_os("windows") # The file remote does not work...
  if (Sys.which("git") == "") skip("no git")
  withr::local_dir(remote <- withr::local_tempdir())
  git("init --bare")

  withr::local_dir(withr::local_tempdir())
  gert::git_init()
  expect_equal(git_branch(), "main")
  cat("foo\n", file = "foo")
  gert::git_add("foo")
  gert::git_commit("Initial commit", author = "gcs <gcs@gmail.com>")
  expect_equal(git_remote_status(), c(NA_integer_, NA_integer_))

  gert::git_remote_add(remote)
  gert::git_push(set_upstream = TRUE, verbose = FALSE)
  expect_equal(git_remote_status(), c(0, 0))

  cat("foobar\n", append = TRUE, file = "foo")
  gert::git_add("foo")
  gert::git_commit("Second commit", author = "gcs <gcs@gmail.com>")
  expect_equal(git_remote_status(), c(1, 0))

  gert::git_push(verbose = FALSE)
  gert::git_reset_soft("HEAD^")
  expect_equal(git_remote_status(), c(0, 1))

  cat("qwerty\n", append = TRUE, file = "foo")
  gert::git_add("foo")
  gert::git_commit("Third commit", author = "gcs <gcs@gmail.com>")
  expect_equal(git_remote_status(), c(1, 1))
})

test_that("git_dirty", {
  if (Sys.which("git") == "") skip("no git")
  withr::local_dir(withr::local_tempdir())
  gert::git_init()
  cat("foo\n", file = "foo")
  gert::git_add("foo")
  gert::git_commit("Initial commit", author = "gcs <gcs@gmail.com>")
  expect_equal(git_dirty(), "")
  cat("foobar\n", append = TRUE, file = "foo")
  expect_equal(git_dirty(), "*")
  gert::git_add("foo")
  expect_equal(git_dirty(), "*")
  gert::git_commit("second", author = "gcs <gcs@gmail.com>")
  expect_equal(git_dirty(), "")
})

test_that("git", {
  skip_on_cran()
  if (Sys.which("git") == "") skip("no git")
  expect_message(git("status", quiet = FALSE))

  ret <- git("dsdfsdfsdf")
  expect_true(attr(ret, "status") > 0)
})

test_that("git_path", {
  expect_error(git_path(tempfile()))
  expect_equal(git_path(tempdir()), tempdir())
})

test_that("check_git_path", {
  mockery::stub(check_git_path, "git_path", NULL)
  expect_error(check_git_path())
})
