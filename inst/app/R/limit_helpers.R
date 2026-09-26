# Estimation uses a masked copy; observed values and temporal positions survive.
limit_estimate <- function(values, excluded = rep(FALSE, length(values)),
                           use_autocorr = FALSE) {
  stopifnot(length(values) == length(excluded), !anyNA(excluded))
  eligible <- is.finite(values) & !excluded
  x <- values
  x[!eligible] <- NA_real_
  if (sum(eligible) < 2L) {
    stop("At least two included observations are required to estimate limits.", call. = FALSE)
  }
  ranges <- abs(diff(x))
  ranges <- ranges[is.finite(ranges)]
  if (!length(ranges)) {
    stop("No adjacent included observations remain. Restore an adjacent pair to estimate limits.", call. = FALSE)
  }
  r <- NA_real_
  a <- head(x, -1L)
  b <- tail(x, -1L)
  pairs <- is.finite(a) & is.finite(b)
  if (sum(pairs) >= 2L && stats::sd(a[pairs]) > 0 && stats::sd(b[pairs]) > 0) {
    r <- stats::cor(a[pairs], b[pairs])
  }
  avg_mr <- mean(ranges)
  adjusted <- isTRUE(use_autocorr) && is.finite(r) && abs(r) < 0.999
  sd_fallback <- isTRUE(use_autocorr) && is.finite(r) && abs(r) >= 0.999
  sigma <- if (adjusted) {
    avg_mr / (1.128 * sqrt(1 - r^2))
  } else if (sd_fallback) {
    stats::sd(x, na.rm = TRUE)
  } else avg_mr / 1.128
  center <- mean(x, na.rm = TRUE)
  list(center = center, sigma = sigma, upper = center + 3 * sigma,
       lower = center - 3 * sigma, average_moving_range = avg_mr,
       r = r, adjusted = adjusted, sd_fallback = sd_fallback, included = sum(eligible))
}

limit_exclusion_caption <- function(data, caption = "") {
  omitted <- data[data$.limit_excluded, , drop = FALSE]
  if (!nrow(omitted)) return(caption)
  axis <- if ("observation" %in% names(data)) "observation" else "date"
  labels <- as.character(omitted[[axis]])
  note <- paste0("Excluded from estimation (cross markers): ", paste(labels, collapse = ", "),
                 ". All observed points remain in signal checks.")
  paste(c(caption[nzchar(caption)], paste(strwrap(note, width = 110), collapse = "\n")), collapse = "\n")
}
