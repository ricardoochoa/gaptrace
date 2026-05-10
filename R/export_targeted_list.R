#' Export Targeted Action List
#'
#' Filters the sustainability tracker and exports a prioritized list to an Excel file.
#'
#' @param tracker_obj An object of class `sustain_tracker`.
#' @param filter_status Optional character vector of statuses to include.
#' @param filter_owner Optional character vector of responsibles to include.
#' @param file_path Character string for the destination `.xlsx` file path.
#' @return Invisibly returns the filtered tibble.
#' @export
#' @importFrom dplyr filter select arrange
#' @importFrom writexl write_xlsx
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' export_targeted_list(
#'   tracker,
#'   filter_status = "Not Delivered",
#'   file_path = "action_list.xlsx"
#' )
#' }
export_targeted_list <- function(tracker_obj, filter_status = NULL, filter_owner = NULL, file_path) {
  if (!inherits(tracker_obj, "sustain_tracker")) {
    rlang::abort("`tracker_obj` must be of class `sustain_tracker`.")
  }

  if (missing(file_path)) {
    rlang::abort("`file_path` must be provided.")
  }

  df <- tracker_obj

  if (!is.null(filter_status)) {
    df <- df |> dplyr::filter(status %in% filter_status)
  }

  if (!is.null(filter_owner)) {
    df <- df |> dplyr::filter(owner %in% filter_owner)
  }

  # Select and reorder columns
  out_cols <- c("data_id", "description", "owner", "reference", "units", "status")
  
  # Ensure only existing columns are selected in case the schema changed
  out_cols <- intersect(out_cols, names(df))

  out_df <- df |>
    dplyr::select(dplyr::all_of(out_cols)) |>
    dplyr::arrange(owner, status)

  # Write to excel
  writexl::write_xlsx(out_df, path = file_path)

  invisible(out_df)
}
