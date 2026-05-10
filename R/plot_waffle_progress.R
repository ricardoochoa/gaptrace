#' Plot Waffle Chart of Progress
#'
#' Generates a waffle chart grid representing the proportion of data collected
#' across the four status levels.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object containing the waffle chart.
#' @export
#' @importFrom dplyr count mutate
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
                    fill = list(n = 0))

  # Define colors for each status
  status_colors <- c(
    "Not Delivered" = "#D3D3D3",        # Grey
    "Rough Estimate" = "#FF9999",       # Light Red
    "Delivered Unvalidated" = "#FFD700",# Gold/Yellow
    "Validated" = "#90EE90"             # Light Green
  )

  # Create waffle chart
  # Since waffle::geom_waffle uses the values directly or uncounted data,
  # it's usually best to use the counts directly mapped to fill.
  p <- ggplot2::ggplot(status_counts, ggplot2::aes(fill = status, values = n)) +
    waffle::geom_waffle(color = "white", size = 0.5, n_rows = 5, flip = TRUE) +
    ggplot2::scale_fill_manual(
      name = "Status",
      values = status_colors,
      drop = FALSE
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Sustainability Data Collection Progress",
      subtitle = "Proportion of data points by maturity level"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      legend.position = "bottom"
    )

  return(p)
}
