library(readr)

test_that("get_longest_dist_obs works", {

  ships <- data.table::fread("data/ships.csv")

  result <- get_longest_dist_obs(data = ships, ship_name = "KAROLI")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = ships, ship_name = "KERLI")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = ships, ship_name = "FINNKRAFT")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = ships, ship_name = "IDUNA")

  expect_equal(nrow(result), 2)
})
