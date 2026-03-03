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


#' Save COBI State Budget
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#' @param bla boolean value indicating whether to include Business-like Activities as part of the state budget
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_state_budget(c(2025, 2026), is1x = FALSE)
#' save_cobi_state_budget(years = 2025, appr_units = 'KPBAC', financing_sources = '9750')
#' save_cobi_state_budget(years = 2025, line_items = 'CBAA', bla = TRUE)
#' save_cobi_state_budget(years = 2025, agencies = '060', financing_sources = '1000')

save_cobi_state_budget <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  financing_sources = NULL,
  is1x = NULL,
  bla = FALSE
) {
  # retrieve data
  state_budget <- get_cobi_state_budget(
    years,
    agencies,
    line_items,
    appr_units,
    financing_sources,
    is1x,
    bla
  )

  # write file
  save_file(state_budget, "State Budget")

  # notify user
  message("Queried `State Budget` saved in Downloads folder.")
}


#' Save COBI Business-like Activities
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_bla(2025, agencies = '063', financing_sources = '9750')

save_cobi_bla <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  business_like_activities <- get_cobi_bla(
    years,
    agencies,
    line_items,
    financing_sources,
    is1x
  )

  # write file
  save_file(business_like_activities, "Business-like Activities")

  # notify user
  message("Queried `Business-like Activities` saved in Downloads folder.")
}


#' Save COBI Restricted Fund and Account Transfers
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_restricted_transfers(c(2025, 2026))
#' save_cobi_restricted_transfers(years = 2025, agencies = '060', financing_sources = '2480')
#' save_cobi_restricted_transfers(years = 2025, line_items = '2252')

save_cobi_restricted_transfers <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  restricted_transfers <- get_cobi_restricted_transfers(
    years,
    agencies,
    line_items,
    financing_sources,
    is1x
  )

  # write file
  save_file(restricted_transfers, "Restricted Fund and Account Transfers")

  # notify user
  message(
    "Queried `Restricted Fund and Account Transfers` saved in Downloads folder."
  )
}


#' Save COBI Fiduciary Funds
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_fiduciary_funds(2025, line_items = '7208')

save_cobi_fiduciary_funds <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  fiduciary_funds <- get_cobi_fiduciary_funds(
    years,
    agencies,
    line_items,
    financing_sources,
    is1x
  )

  # write file
  save_file(fiduciary_funds, "Fiduciary Funds")

  # notify user
  message(
    "Queried `Fiduciary Funds` saved in Downloads folder."
  )
}


#' Save COBI Capital Project Funds
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_cpf(c(2025, 2026))
#' save_cobi_cpf(years = 2025, agencies = '102', financing_sources = '2480')
#' save_cobi_cpf(years = 2025, line_items = '2915')

save_cobi_cpf <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  capital_project_funds <- get_cobi_cpf(
    years,
    agencies,
    line_items,
    financing_sources,
    is1x
  )

  # write file
  save_file(capital_project_funds, "Capital Project Funds")

  # notify user
  message(
    "Queried `Capital Project Funds` saved in Downloads folder."
  )
}


#' Save COBI State Funds Withdrawal
#'
#' This saves all appropriations made from unrestricted "State Funds" including General Fund (1000), Income Tax Fund (2480), and Uniform School Fund (2400) into an Excel file in the user's Downloads folder.
#'
#' @param years vector of integers indicating year(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_state_funds_withdrawal(2025)

save_cobi_state_funds_withdrawal <- function(years, is1x = NULL) {
  # retrieve data
  state_funds_withdrawal <- get_cobi_state_funds_withdrawal(years, is1x)

  # write file
  save_file(state_funds_withdrawal, "State Funds Withdrawal")

  # notify user
  message(
    "Queried `State Funds Withdrawal` saved in Downloads folder."
  )
}


#' Get COBI proposed expenditures
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param expenditure_categories character vector indicating expenditure category(ies) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns N/A; writes xlsx file to Downloads folder
#' @export
#'
#' @examples
#' save_cobi_proposed_expenditures(c(2025, 2026), agencies = '060')
#' save_cobi_proposed_expenditures(years = 2025, expenditure_categories = 'AA')
#' save_cobi_proposed_expenditures(years = 2025, line_items = 'KPBA', is1x = TRUE)

save_cobi_proposed_expenditures <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  expenditure_categories = NULL,
  is1x = NULL
) {
  # retrieve data
  expenditures <- get_cobi_proposed_expenditures(
    years,
    agencies,
    line_items,
    appr_units,
    expenditure_categories,
    is1x
  )

  # write file
  save_file(expenditures, "COBI Proposed Expenditures")

  # notify user
  message(
    "Queried `COBI Proposed Expenditures` saved in Downloads folder."
  )
}
