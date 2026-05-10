#' Plot Collection Trend
#'
#' Generates a cumulative Burn-up chart of "Validated" items over time.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_area geom_point labs theme_minimal theme element_text
#' @importFrom dplyr filter count arrange mutate
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_collection_trend(tracker)
#' }
plot_collection_trend <- function(tracker_obj) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }
  
  if (!"date_captured" %in% names(tracker_obj)) {
    rlang::abort("The tracker object must contain a `date_captured` column.")
  }

  status_colors <- c(
    "Not Delivered" = "#D9D9D9",
    "Rough Estimate" = "#969696",
    "Delivered Unvalidated" = "#525252",
    "Validated" = "#000000"
  )

  # Calculate cumulative sum of validated items over time
  trend_data <- tracker_obj |>
    dplyr::filter(status == "Validated") |>
    dplyr::filter(!is.na(date_captured)) |>
    dplyr::count(date_captured) |>
    dplyr::arrange(date_captured) |>
    dplyr::mutate(cumulative_validated = cumsum(n))

  if (nrow(trend_data) == 0) {
    rlang::abort("No 'Validated' items with valid 'date_captured' available to plot.")
  }

  p <- ggplot2::ggplot(trend_data, ggplot2::aes(x = date_captured, y = cumulative_validated)) +
    ggplot2::geom_area(fill = status_colors["Validated"], alpha = 0.2) +
    ggplot2::geom_line(color = status_colors["Validated"], linewidth = 1.2) +
    ggplot2::geom_point(color = status_colors["Validated"], size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "COLLECTION VELOCITY",
      subtitle = "Cumulative volume of Validated items over time",
      x = "Date",
      y = "Validated Items"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16)
    )

  return(p)
}
