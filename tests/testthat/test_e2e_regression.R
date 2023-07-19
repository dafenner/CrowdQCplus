library(data.table)
library(CrowdQCplus)

test_that("qc result is as expected", {
  # this will load as variable `cqcp_cws_data`
  data(cqcp_cws_data)
  test_data <- cqcp_padding(cqcp_cws_data)
  # make sure the check passes (also cover those lines this way)
  expect_identical(cqcp_check_input(test_data), TRUE)
  data_qc <- cqcp_qcCWS(test_data, m5_keep_isolated = TRUE)
  # this will load as variable `expected`
  load('test_files/cqcp_cws_data_expected.rda')
  setkey(data_qc, NULL)
  setkey(expected, NULL)
  expect_equal(data_qc, expected = expected)
})
