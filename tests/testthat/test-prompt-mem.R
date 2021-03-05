
test_that("prompt_mem", {
  withr::local_options(cli.unicode = FALSE)
  mockery::stub(prompt_mem, "memory_usage", list(formatted = "236.67 MB"))
  expect_equal(prompt_mem(), "236.67 MB > ")
})


test_that("memory_usage", {
  mem <- fixture$get(ps::ps_memory_info())
  mockery::stub(memory_usage, "ps::ps_memory_info", mem)
  sys <- fixture$get(ps::ps_system_memory())
  mockery::stub(memory_usage, "ps::ps_system_memory", sys)
  expect_snapshot(memory_usage())
})
