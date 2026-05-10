#' Plot Interdependency Network Graph
#'
#' Generates a network graph linking data nodes to their central theme nodes.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @param interactive Logical. If `TRUE`, returns a `visNetwork` object. If `FALSE` (default), returns a `ggraph` object.
#' @return A `ggraph` or `visNetwork` object representing the data-to-theme dependencies.
#' @export
#' @importFrom dplyr select mutate rename filter left_join bind_rows distinct count
#' @importFrom tidyr separate_rows
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text theme_graph scale_edge_width
#' @importFrom ggplot2 aes scale_color_manual scale_size_manual guides guide_legend theme element_rect
#' @importFrom visNetwork visNetwork visNodes visEdges visOptions visInteraction
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' plot_theme_network(tracker, interactive = FALSE)
#' }
plot_theme_network <- function(tracker_obj, interactive = FALSE) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }

  # Split themes by comma into tidy format
  edges <- tracker_obj |>
    dplyr::select(data_id, themes) |>
    tidyr::separate_rows(themes, sep = ",\\s*") |>
    dplyr::filter(themes != "", !is.na(themes)) |>
    # Data -> Theme
    dplyr::rename(from = data_id, to = themes)

  if (nrow(edges) == 0) {
    rlang::abort("No valid themes found to build the network edges.")
  }

  # Calculate degree (how many themes each data_id is linked to)
  node_degrees <- edges |>
    dplyr::count(from, name = "degree")

  # Build node definitions
  theme_nodes <- data.frame(
    id = unique(edges$to),
    label = unique(edges$to),
    type = "Theme",
    status = NA_character_,
    degree = max(node_degrees$degree, na.rm = TRUE) + 2,
    stringsAsFactors = FALSE
  )

  data_nodes <- tracker_obj |>
    dplyr::select(id = data_id, status) |>
    # Only keep data nodes that actually appear in edges
    dplyr::filter(id %in% edges$from) |>
    dplyr::left_join(node_degrees, by = c("id" = "from")) |>
    dplyr::mutate(
      label = id,
      type = "Data",
      status = as.character(status)
    )

  nodes <- dplyr::bind_rows(theme_nodes, data_nodes) |>
    dplyr::distinct(id, .keep_all = TRUE)

  # Colors for Data Nodes based on status
  status_colors <- c(
    "Not Delivered" = "#D9D9D9",
    "Rough Estimate" = "#969696",
    "Delivered Unvalidated" = "#525252",
    "Validated" = "#000000",
    "Theme" = "#000000"  # Black for theme nodes
  )

  if (interactive) {
    # Prepare visNetwork nodes
    v_nodes <- nodes |>
      dplyr::mutate(
        shape = ifelse(type == "Theme", "diamond", "dot"),
        value = degree,
        color = ifelse(type == "Theme", status_colors["Theme"], status_colors[status]),
        font.size = ifelse(type == "Theme", 20, 14),
        title = ifelse(type == "Theme", paste("Theme:", label), paste("Data:", label, "<br>Status:", status, "<br>Connectivity:", degree))
      )

    # Prepare visNetwork edges
    v_edges <- edges |>
      dplyr::mutate(arrows = "to")

    p <- visNetwork::visNetwork(v_nodes, v_edges, width = "100%", height = "600px") |>
      visNetwork::visNodes(shadow = TRUE, scaling = list(min = 10, max = 30)) |>
      visNetwork::visEdges(color = list(color = "#BDBDBD", highlight = "#000000")) |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
      visNetwork::visInteraction(navigationButtons = TRUE)
    
    return(p)

  } else {
    # Static ggraph
    ig <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    
    p <- ggraph::ggraph(ig, layout = "fr") +
      ggraph::geom_edge_link(
        color = "#BDBDBD",
        arrow = grid::arrow(length = grid::unit(3, 'mm'), type = "closed"),
        end_cap = ggraph::circle(4, 'mm')
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(
          color = ifelse(type == "Theme", "Theme", status),
          size = degree,
          shape = type
        )
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(
          label = label,
          filter = type == "Theme"
        ),
        vjust = 2.5,
        fontface = "bold",
        size = 5
      ) +
      ggplot2::scale_color_manual(
        name = "Status / Type",
        values = status_colors,
        breaks = names(status_colors)
      ) +
      ggplot2::scale_size_continuous(
        range = c(4, 10),
        guide = "none"
      ) +
      ggplot2::scale_shape_manual(
        values = c("Theme" = 18, "Data" = 16),
        guide = "none"
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 5, shape = 16))
      ) +
      ggraph::theme_graph(background = "white") +
      ggplot2::theme(
        legend.position = "right",
        plot.title = ggplot2::element_text(face = "bold", size = 16),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      ) +
      ggplot2::labs(
        title = "BOTTLENECK MAP",
        subtitle = "Data to Theme traceability"
      )
    
    return(p)
  }
}
