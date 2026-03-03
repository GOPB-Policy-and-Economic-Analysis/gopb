#' Save COBI orgs
#'
#' @param year integer indicating fiscal year time stamp at which to snapshot the budget organization hierarchy
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
  save_file(orgs, "Budget Organization")

  # notify user
  message("Queried COBI `Budget Organization` saved in Downloads folder.")
}


#' Save COBI financing sources
#'
#' @param year integer indicating fiscal year time stamp at which to snapshot active financing sources
#' @param restricted_only boolean value indicating whether to pull only restricted funds/accounts
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_financing_sources()
#' save_cobi_financing_sources(2024, TRUE)

save_cobi_financing_sources <- function(
  year = NULL,
  restricted_only = NULL
) {
  # retrieve data
  financing_sources <- get_cobi_financing_sources(year, restricted_only)

  # write file
  save_file(financing_sources, "Financing Sources")

  # notify user
  message("Queried COBI `Financing Sources` saved in Downloads folder.")
}


#' Save COBI expenditure categories
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_expenditure_categories()

save_cobi_expenditure_categories <- function() {
  # retrieve data
  expenditure_categories <- get_cobi_expenditure_categories()

  # write file
  save_file(expenditure_categories, "Expenditure Categories")

  # notify user
  message("Queried COBI `Expenditure Categories` saved in Downloads folder.")
}
