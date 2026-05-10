#' Ingest Sustainability Tracker Data
#'
#' Validates and processes raw sustainability tracker data into a standardized format.
#'
#' @param data A data frame containing the raw sustainability tracker.
#'   Must include columns: `data_id`, `description`, `themes`, `status`,
#'   `owner`, `reference`, `units`.
#' @param lang A character string specifying the input data language schema. Defaults to `"es"`. If `"es"`, it will translate Spanish column names to English.
#' @return An object of class `sustain_tracker` (a validated tibble).
#' @export
#' @importFrom dplyr as_tibble mutate rename
#' @importFrom rlang abort
#'
#' @examples
#' \dontrun{
#' tracker <- sustain_ingest(sustain_dummy_data)
#' }
sustain_ingest <- function(data, lang = "es") {
  # 1. Convert to tibble
  df <- dplyr::as_tibble(data)

  if (lang == "es") {
    es_to_en <- c(
      data_id = "id_dato",
      description = "descripcion",
      themes = "tematicas",
      status = "estatus",
      owner = "responsable",
      reference = "referencia",
      units = "unidades",
      tags = "etiquetas"
    )
    
    es_to_en_valid <- es_to_en[es_to_en %in% names(df)]
    if (length(es_to_en_valid) > 0) {
      df <- dplyr::rename(df, dplyr::all_of(es_to_en_valid))
    }
  }

  # 2. Check required columns
  req_cols <- c("data_id", "description", "themes", "status", 
                "owner", "reference", "units")
  missing_cols <- setdiff(req_cols, names(df))
  
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # 3. Check for duplicate data_id
  if (any(duplicated(df$data_id))) {
    rlang::abort("Duplicate values found in `data_id` column.")
  }

  # 4. Convert status to ordered factor
  valid_levels <- c("Not Delivered", "Rough Estimate", "Delivered Unvalidated", "Validated")
  
  df <- df |>
    dplyr::mutate(
      status = factor(
        status, 
        levels = valid_levels, 
        ordered = TRUE
      )
    )

  # Validate that status didn't become all NAs due to bad inputs
  if (any(is.na(df$status))) {
    warning("Some `status` values did not match the expected levels and were converted to NA.")
  }

  # 5. Add custom S3 class
  class(df) <- c("sustain_tracker", class(df))
  
  return(df)
}
