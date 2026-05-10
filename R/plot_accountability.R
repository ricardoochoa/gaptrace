#' Plot Accountability Bars
#'
#' Generates a horizontal stacked bar chart showing the proportion of status levels
#' per owner, ranked by the percentage of "Validated" data.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual labs theme_minimal theme element_text scale_x_continuous
#' @importFrom dplyr count group_by mutate arrange ungroup filter pull
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_accountability(tracker)
#' }
plot_accountability <- function(tracker_obj) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }
  
  if (!"owner" %in% names(tracker_obj)) {
    rlang::abort("The tracker object must contain an `owner` column.")
  }

  status_colors <- c(
    "Not Delivered" = "#D9D9D9",
    "Rough Estimate" = "#969696",
    "Delivered Unvalidated" = "#525252",
    "Validated" = "#000000"
  )

  # Calculate proportions and order owners
  owner_stats <- tracker_obj |>
    dplyr::count(owner, status) |>
    dplyr::group_by(owner) |>
    dplyr::mutate(total = sum(n), prop = n / total) |>
    dplyr::ungroup()
    
  owner_order <- owner_stats |>
    dplyr::filter(status == "Validated") |>
    dplyr::arrange(prop) |> # Ascending because y-axis plots bottom-to-top
    dplyr::pull(owner)
    
  # Add owners with 0 validated to the bottom
  all_owners <- unique(owner_stats$owner)
  missing_owners <- setdiff(all_owners, owner_order)
  final_order <- c(missing_owners, owner_order)
  
  plot_data <- owner_stats |>
    dplyr::mutate(owner = factor(owner, levels = final_order))
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = prop, y = owner, fill = status)) +
    ggplot2::geom_col(position = "fill", width = 0.7) +
    ggplot2::scale_fill_manual(
      name = "Status",
      values = status_colors
    ) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "ACCOUNTABILITY LEADERBOARD",
      subtitle = "Owner efficiency and maturity ranking",
      x = "Proportion",
      y = "Owner"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      legend.position = "bottom"
    )

  return(p)
}
