#' Plot Quadrant Chart
#'
#' Generates a scatter plot mapping priority against status.
#' Highlights data points that are "High" priority and "Not Delivered".
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_jitter scale_color_manual scale_size_manual labs theme_minimal theme element_text
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_quadrant(tracker)
#' }
plot_quadrant <- function(tracker_obj) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }
  
  if (!"priority" %in% names(tracker_obj)) {
    rlang::abort("The tracker object must contain a `priority` column.")
  }

  status_colors <- c(
    "Not Delivered" = "#D9D9D9",
    "Rough Estimate" = "#969696",
    "Delivered Unvalidated" = "#525252",
    "Validated" = "#000000"
  )

  # Create a highlight flag
  plot_data <- tracker_obj |>
    dplyr::mutate(
      is_critical = (priority == "High" & status == "Not Delivered"),
      color_group = ifelse(is_critical, "Critical", as.character(status))
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = status, y = priority)) +
    ggplot2::geom_jitter(
      ggplot2::aes(
        color = color_group,
        size = is_critical
      ),
      width = 0.2, height = 0.2, alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(status_colors, "Critical" = "#000000"),
      guide = "none"
    ) +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 3, "TRUE" = 6),
      guide = "none"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "PRIORITY VS STATUS QUADRANT",
      subtitle = "Identifying critical pending items",
      x = "Status",
      y = "Priority"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16)
    )

  return(p)
}
