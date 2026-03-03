#' Save COBI meta
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param categories character vector indicating financing source(s) or expenditure category(ies) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_meta(2025)
#' save_cobi_meta(years = 2025, appr_units = 'KPBAC', categories = '9750')
#' save_cobi_meta(years = c(2024, 2025), line_items = 'CBAA')
#' save_cobi_meta(years = 2025, line_items = '2105', categories = '9800')
#' save_cobi_meta(years = 2025, agencies = '060', categories = '1000')
#' save_cobi_meta(years = 2025, categories = 'AA')

save_cobi_meta <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  categories = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    appr_units,
    categories,
    is1x
  )

  # write file
  save_file(cobi_meta, "COBI Meta Data")

  # notify user
  message("Queried `COBI Meta Data` saved in Downloads folder.")
}
