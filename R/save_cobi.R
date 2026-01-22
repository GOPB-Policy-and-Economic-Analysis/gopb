#' Save COBI orgs
#'
#' @param year integer indicating year of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_orgs()
#' save_cobi_orgs(agencies = "250")
#' save_cobi_orgs(line_items = c("CBAA", "KPBA"))
#' save_cobi_orgs(2023)

save_cobi_orgs <- function(year = NULL, agencies = NULL, line_items = NULL) {
  # retrieve data
  orgs <- get_cobi_orgs(year, agencies, line_items)

  # write file
  save_file(orgs, "Current Budget Organization")

  # notify user
  message("Current Budget Organization saved in Downloads folder.")
}


#' Save COBI appropriations
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_approps(2025)
#' save_cobi_approps(years = 2025, appr_units = 'KPBAC', financing_sources = '9750')
#' save_cobi_approps(years = c(2024, 2025), line_items = 'CBAA')
#' save_cobi_approps(years = 2025, line_items = '2105', financing_sources = '9800')
#' save_cobi_approps(years = c(2024, 2025), agencies = '060', financing_sources = '1000')

save_cobi_approps <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  financing_sources = NULL
) {
  # retrieve data
  approps <- get_cobi_approps(
    years,
    agencies,
    line_items,
    appr_units,
    financing_sources
  )

  # write file
  save_file(approps, "COBI Appropriations")

  # notify user
  message("Queried COBI Appropriations saved in Downloads folder.")
}
