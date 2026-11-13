library(data.table)
library(CrowdQCplus)

make_data <- function() {
  group <- function(prefix, lon0, n) {
    data.table(
      p_id = paste0(prefix, 0:n),
      lon = lon0 + c(0, 0.005 * seq_len(n) / n),
      lat = 50,
      z = 100
    )
  }
  # A0: 6 buddies, 2 missing at t2, outlier at t3
  # C0: 4 buddies (isolated)
  # D0: 5 buddies, one without any data
  stations <- rbind(group("A", 10, 6), group("C", 11, 4), group("D", 12, 5))
  times <- as.POSIXct("2025-11-01", tz = "UTC") + 3600 * 0:2
  d <- CJ(p_id = stations$p_id, time = times)[stations, on = "p_id"]
  d[, ta := 15 + 0.2 * as.integer(substring(p_id, 2))]
  d[p_id == "A0", ta := 15.5]
  d[p_id %in% c("A1", "A2") & time == times[2], ta := NA]
  d[p_id == "A0" & time == times[3], ta := 25]
  d[p_id == "D5", ta := NA]
  d[, m4 := !is.na(ta)]
  return(d)
}

run_m5 <- function(keep_isolated) {
  capture.output(
    res <- cqcp_m5(make_data(),
      keep_isolated = keep_isolated,
      heightCorrection = FALSE,
      quiet = TRUE
    )
  )
  setkey(res, p_id, time)
  return(res)
}

test_that("keep_isolated = TRUE keeps values that cannot be checked", {
  res <- run_m5(keep_isolated = TRUE)
  expect_identical(res[p_id == "A0", m5], c(TRUE, TRUE, FALSE))
  expect_identical(res[p_id == "C0", m5], c(TRUE, TRUE, TRUE))
  expect_identical(res[p_id == "D0", m5], c(TRUE, TRUE, TRUE))
  expect_false(any(res[m4 == FALSE, m5]))
})

test_that("isolated is set per time step when too few buddies have valid data", {
  for (keep in c(TRUE, FALSE)) {
    res <- run_m5(keep_isolated = keep)
    # A0: too few valid buddies only at t2
    expect_identical(res[p_id == "A0", isolated], c(FALSE, TRUE, FALSE))
    # A3: same as A0, since A1 and A2 are also its buddies
    expect_identical(res[p_id == "A3", isolated], c(FALSE, TRUE, FALSE))
    # A1: missing itself at t2, but still has 5 valid buddies (A0, A3-A6)
    expect_identical(res[p_id == "A1", isolated], c(FALSE, FALSE, FALSE))
    # C0: isolated station
    expect_identical(res[p_id == "C0", isolated], c(TRUE, TRUE, TRUE))
    # D0: one buddy without any data, too few valid buddies at all time steps
    expect_identical(res[p_id == "D0", isolated], c(TRUE, TRUE, TRUE))
  }
})

test_that("keep_isolated = FALSE removes values that cannot be checked", {
  res <- run_m5(keep_isolated = FALSE)
  expect_identical(res[p_id == "A0", m5], c(TRUE, FALSE, FALSE))
  expect_identical(res[p_id == "C0", m5], c(FALSE, FALSE, FALSE))
  expect_identical(res[p_id == "D0", m5], c(FALSE, FALSE, FALSE))
})
