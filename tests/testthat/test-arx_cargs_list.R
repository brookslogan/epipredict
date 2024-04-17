test_that("arx_class_args checks inputs", {
  expect_s3_class(arx_class_args_list(), c("arx_class", "alist"))
  expect_error(arx_class_args_list(ahead = c(0, 4)))
  expect_error(arx_class_args_list(n_training = c(28, 65)))

  expect_error(arx_class_args_list(ahead = -1))
  expect_error(arx_class_args_list(ahead = 1.5))
  expect_error(arx_class_args_list(n_training = -1))
  expect_error(arx_class_args_list(n_training = 1.5))
  expect_error(arx_class_args_list(lags = c(-1, 0)))
  expect_error(arx_class_args_list(lags = list(c(1:5, 6.5), 2:8)))


  expect_error(arx_class_args_list(target_date = "2022-01-01"))
  expect_identical(
    arx_class_args_list(target_date = as.Date("2022-01-01"))$target_date,
    as.Date("2022-01-01")
  )

  expect_error(arx_class_args_list(n_training_min = "de"))
  expect_error(arx_class_args_list(epi_keys = 1))
})

test_that("arx_classifier_args_list provided forecast_date and target_date are respected", {
  horizon <- 7L
  latency <- 5L
  ahead <- horizon + latency
  forecast_date <- as.Date("1234-01-01")
  target_date <- forecast_date + horizon
  epi_workflow <- arx_class_epi_workflow(
    case_death_rate_subset,
    outcome = "case_rate",
    predictors = c("case_rate"),
    args_list = arx_class_args_list(
      ahead = ahead,
      forecast_date = forecast_date,
      target_date = target_date
    )
  )
  expect_identical(
    extract_frosting(epi_workflow)[["layers"]] %>%
      `[[`(which(map_lgl(., inherits, "layer_add_forecast_date"))) %>%
      `[[`("forecast_date"),
    forecast_date
  )
  expect_identical(
    extract_frosting(epi_workflow)[["layers"]] %>%
      `[[`(which(map_lgl(., inherits, "layer_add_target_date"))) %>%
      `[[`("target_date"),
    target_date
  )
})
