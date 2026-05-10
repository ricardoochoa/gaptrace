#' Plot Waffle Chart of Progress
#'
#' Generates a waffle chart grid representing the proportion of data collected
#' across the four status levels.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object containing the waffle chart.
#' @export
#' @importFrom dplyr count mutate arrange
#' @importFrom waffle geom_waffle
#' @importFrom ggplot2 ggplot aes scale_fill_manual labs coord_equal
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_waffle_progress(tracker)
#' }
plot_waffle_progress <- function(tracker_obj) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }

  # Count occurrences of each status
  status_counts <- tracker_obj |>
    dplyr::count(status) |>
    # Ensure all levels are present even if count is 0
    tidyr::complete(status = factor(c("Not Delivered", "Rough Estimate", "Delivered Unvalidated", "Validated"), 
                                     levels = c("Not Delivered", "Rough Estimate", "Delivered Unvalidated", "Validated"),
                                     ordered = TRUE), 
                    fill = list(n = 0)) |>
    dplyr::arrange(status)

  # Calculate combined percentage
  total_n <- sum(status_counts$n)
  pct_val <- 0
  if (total_n > 0) {
    pct_val <- sum(status_counts$n[status_counts$status %in% c("Delivered Unvalidated", "Validated")]) / total_n * 100
  }
  subtitle_text <- sprintf("%.1f%% of data points have been delivered or validated", pct_val)

  # Define colors for each status
  status_colors <- c(
    "Not Delivered" = "#D9D9D9",        # Light Gray
    "Rough Estimate" = "#969696",       # Medium Gray
    "Delivered Unvalidated" = "#525252",# Dark Gray
    "Validated" = "#000000"             # Solid Black
  )

  # Create waffle chart
  p <- ggplot2::ggplot(status_counts, ggplot2::aes(fill = status, values = n)) +
    waffle::geom_waffle(color = "white", size = 0.5, n_rows = 10, make_proportional = TRUE, flip = TRUE) +
    ggplot2::scale_fill_manual(
      name = "Status",
      values = status_colors,
      drop = FALSE
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "DATA COLLECTION MATURITY",
      subtitle = subtitle_text
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      legend.position = "bottom"
    )

  return(p)
}
