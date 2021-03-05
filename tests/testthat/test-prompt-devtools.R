
test_that("prompt_devtools", {
  mockery::stub(prompt_devtools, "devtools_packages", character())
  mockery::stub(prompt_devtools, "git_info", "gittt")
  expect_equal(prompt_devtools(), "gittt > ")

  mockery::stub(prompt_devtools, "devtools_packages", c("one", "two"))
  expect_equal(prompt_devtools(), "one+two / gittt > ")

  mockery::stub(prompt_devtools, "git_info", "")
  expect_equal(prompt_devtools(), "one+two > ")

  mockery::stub(prompt_devtools, "devtools_packages", character())
  expect_equal(prompt_devtools(), " > ")
})

test_that("devtools_packages", {
  expect_equal(
    callr::r(function() prompt::devtools_packages()),
    character()
  )
  expect_equal(
    callr::r(
      function() { pkgload::load_all(); prompt::devtools_packages() },
      wd = test_path("fixtures/pkg")
    ),
    "foo"
  )
})
