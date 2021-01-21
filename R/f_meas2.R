#' Simple wrapper for yardstick::f_meas with beta = 0.5 to use in tuning operations
#'
#' @importFrom yardstick f_meas_vec metric_summarizer new_class_metric

f_meas2_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
  f_meas_vec(
    truth = truth, 
    estimate = estimate, 
    beta = 0.5, 
    estimator = estimator, 
    na_rm = na_rm,
    ...
  )
}


f_meas2 <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
  metric_summarizer(
    "f_meas2",
    f_meas2_vec,
    data = data,
    truth = {{truth}},
    estimate = {{estimate}},
    estimator = estimator,
    na_rm = na_rm,
    ...
  )
}

#' @export
f_meas2 <- new_class_metric(f_meas2, "maximize")