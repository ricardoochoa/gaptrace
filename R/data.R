#' Synthetic Sustainability Data
#'
#' A dummy dataset representing a sustainability data collection tracker.
#' Used to demonstrate the functionalities of the gaptrace package.
#' Note: The raw data contains Spanish column names to simulate real-world input, 
#' but `sustain_ingest()` will translate these to the internal English schema.
#'
#' @format A data frame with 50 rows and 8 variables:
#' \describe{
#'   \item{id_dato}{Unique identifier for the data point}
#'   \item{descripcion}{Description of the data required}
#'   \item{tematicas}{Comma-separated list of themes}
#'   \item{estatus}{Collection status: Not Delivered, Rough Estimate, Delivered Unvalidated, Validated}
#'   \item{responsable}{Person responsible for the data}
#'   \item{referencia}{Reference document}
#'   \item{unidades}{Units of measurement}
#'   \item{etiquetas}{Comma-separated tags}
#' }
#' @source Simulated for testing and demonstration purposes.
"sustain_dummy_data"
