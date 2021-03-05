library(testthat)
library(prompt)

if (packageVersion("ps") >= "1.6.0") test_check("prompt")
