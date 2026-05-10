#' Plot Thematic Heatmap
#'
#' Generates a faceted tile plot representing individual data points within themes,
#' forming a "Data Grid" of maturity.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap scale_fill_manual coord_equal labs theme_void theme element_text margin
#' @importFrom dplyr select filter arrange group_by mutate row_number ungroup n
#' @importFrom tidyr separate_rows
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_theme_heatmap(tracker)
#' }
plot_theme_heatmap <- function(tracker_obj) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }

  status_colors <- c(
    "Not Delivered" = "#D9D9D9",
    "Rough Estimate" = "#969696",
    "Delivered Unvalidated" = "#525252",
    "Validated" = "#000000"
  )

  plot_data <- tracker_obj |>
    dplyr::select(data_id, themes, status) |>
    tidyr::separate_rows(themes, sep = ",\\s*") |>
    dplyr::filter(themes != "", !is.na(themes)) |>
    dplyr::arrange(themes, status) |> # Arrange by status to form solid blocks within themes
    dplyr::group_by(themes) |>
    dplyr::mutate(
      idx = dplyr::row_number() - 1,
      cols = ceiling(sqrt(dplyr::n())),
      x = idx %% cols,
      y = idx %/% cols
    ) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, fill = status)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::facet_wrap(~ themes, scales = "free") +
    ggplot2::scale_fill_manual(
      name = "Status",
      values = status_colors
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "THEMATIC MATURITY HEATMAP",
      subtitle = "Density of completion by theme"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 20)),
      strip.text = ggplot2::element_text(face = "bold", size = 10, margin = ggplot2::margin(b = 5, t = 5)),
      legend.position = "bottom"
    )

  return(p)
}
