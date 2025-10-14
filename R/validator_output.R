ensure_output_header_names <- function(
    data,
    expected_headers = c("time", "individual", "keypoint", "x", "y", "confidence")
) {
  headers <- names(data)[names(data) %in% expected_headers]
  if (!all(expected_headers %in% headers)) {
    cli::cli_abort(
      "Got header names '{headers}', but expected '{expected_headers}'."
    )
  }
}

ensure_output_header_class <- function(
    data,
    expected_headers = c(
      "time",
      "individual",
      "keypoint",
      "x",
      "y",
      "confidence"
    ),
    expected_header_class = c(
      "numeric",
      "factor",
      "factor",
      "numeric",
      "numeric",
      "numeric"
    )
) {
  data <- data |>
    dplyr::select(dplyr::all_of(expected_headers))
  header_classes <- sapply(data, class)
  header_classes <- sapply(header_classes, function(x) {
    dplyr::if_else(x == "integer", "numeric", x)
  })
  if (!all(expected_header_class == header_classes)) {
    cli::cli_abort(
      "Expected output headers to be {expected_header_class}, got {header_classes}."
    )
  }
}

ensure_output_no_nan <- function(data) {
  if (any(sapply(data, is.nan))) {
    cli::cli_warn(
      "The data frame contains NaN's. It is suggested to convert them to NAs."
    )
  }
}
