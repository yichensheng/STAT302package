usethis::use_github_action("test-coverage")library(testthat)
library(STAT302package)

test_check("STAT302package")
