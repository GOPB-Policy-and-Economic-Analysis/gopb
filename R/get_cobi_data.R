#' Pull COBI meta
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param categories character vector indicating financing source(s) or expenditure category(ies) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' cobi_meta <- get_cobi_meta(2025)
#' cobi_meta <- get_cobi_meta(years = 2025, appr_units = 'KPBAC', categories = '9750')
#' cobi_meta <- get_cobi_meta(years = c(2024, 2025), line_items = 'CBAA')
#' cobi_meta <- get_cobi_meta(years = 2025, line_items = '2105', categories = '9800')
#' cobi_meta <- get_cobi_meta(years = 2025, agencies = '060', categories = '1000')
#' cobi_meta <- get_cobi_meta(years = 2025, categories = 'AA')

get_cobi_meta <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  categories = NULL,
  is1x = NULL
) {
  # ARGUMENT TYPE CHECKS

  # years
  if (!is.numeric(years) || length(years) < 1 || any(years %% 1 != 0)) {
    rlang::abort(
      "`years` must be a non-empty vector of integers."
    )
  }

  # character parameters
  char_vector_check(agencies, "agencies")
  char_vector_check(line_items, "line_items")
  char_vector_check(appr_units, "appropriation_units")
  char_vector_check(categories, "financing_sources or expenditure_categories")

  # is1x
  if (
    !is.null(is1x) && !(is.logical(is1x) && length(is1x) == 1 && !is.na(is1x))
  ) {
    rlang::abort("`is1x` must be a boolean value.")
  }

  # drop back to when first year of interest was the out-year to capture initial appropriations
  url_years <- unique(c(min(years) - 1, years))

  # create unique urls and pull data with them
  cobi_meta <- purrr::map(url_years, appr_url) %>%
    purrr::map(cobi_data_extraction) %>%
    dplyr::bind_rows()

  # filter to specified appropriations view
  cobi_meta %<>%
    dplyr::filter(.data$FY %in% years)

  if (!is.null(agencies)) {
    cobi_meta %<>% dplyr::filter(.data$Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    cobi_meta %<>% dplyr::filter(.data$Line_Item %in% line_items)
  }
  if (!is.null(appr_units)) {
    cobi_meta %<>% dplyr::filter(.data$Appr_Unit %in% appr_units)
  }
  if (!is.null(categories)) {
    cobi_meta %<>% dplyr::filter(.data$Category %in% categories)
  }
  if (!is.null(is1x)) {
    cobi_meta %<>% dplyr::filter(.data$is1x == is1x)
  }

  return(cobi_meta)
}


#' Get COBI State Budget
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#' @param bla boolean value indicating whether to include Business-like Activities as part of the state budget
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' ongoing_state_budget <- get_cobi_state_budget(c(2025, 2026), is1x = FALSE)
#' aco_federal <- get_cobi_state_budget(years = 2025, appr_units = 'KPBAC', financing_sources = '9750')
#' gopb_budget_with_bla <- get_cobi_state_budget(years = 2025, line_items = 'CBAA', bla = TRUE)
#' gov_from_gf <- get_cobi_state_budget(years = 2025, agencies = '060', financing_sources = '1000')

get_cobi_state_budget <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  financing_sources = NULL,
  is1x = NULL,
  bla = FALSE
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    appr_units,
    financing_sources,
    is1x
  )

  # filter to State Budget view
  state_budget <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Line_Item_Cat_Desc %in%
        c(
          'Operating and Capital Budgets',
          'Expendable Funds and Accounts',
          'Business-like Activities'
        )
    )

  if (bla) {
    return(state_budget)
  } else {
    return(
      state_budget |>
        dplyr::filter(.data$Line_Item_Cat_Desc != 'Business-like Activities')
    )
  }
}


#' Get COBI Business-like Activities
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' goeo_federal_bla <- get_cobi_bla(2025, agencies = '063', financing_sources = '9750')

get_cobi_bla <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    categories = financing_sources,
    is1x = is1x
  )

  # filter to Business-like Activities view
  business_like_activities <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Line_Item_Cat_Desc == 'Business-like Activities'
    )

  return(business_like_activities)
}


#' Get COBI Restricted Fund and Account Transfers
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' restricted_fund_and_account_transfers <- get_cobi_restricted_transfers(c(2025, 2026))
#' gov_itf <- get_cobi_restricted_transfers(years = 2025, agencies = '060', financing_sources = '2480')
#' medicaid_aca_transfers <- get_cobi_restricted_transfers(years = 2025, line_items = '2252')

get_cobi_restricted_transfers <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    categories = financing_sources,
    is1x = is1x
  )

  # filter to Restricted Transfers view
  restricted_transfers <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Line_Item_Cat_Desc == 'Restricted Fund and Account Transfers'
    )

  return(restricted_transfers)
}


#' Get COBI Fiduciary Funds
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' navajo_trust <- get_cobi_fiduciary_funds(2025, line_items = '7208')

get_cobi_fiduciary_funds <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    categories = financing_sources,
    is1x = is1x
  )

  # filter to Fiduciary Funds view
  fiduciary_funds <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Line_Item_Cat_Desc == 'Fiduciary Funds'
    )

  return(fiduciary_funds)
}


#' Get COBI Capital Project Funds
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' capital_project_funds <- get_cobi_cpf(c(2025, 2026))
#' capital_budget_itf <- get_cobi_cpf(years = 2025, agencies = '102', financing_sources = '2480')
#' transit_fund <- get_cobi_cpf(years = 2025, line_items = '2915')

get_cobi_cpf <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  financing_sources = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    categories = financing_sources,
    is1x = is1x
  )

  # filter to Capital Project Funds view
  capital_project_funds <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Line_Item_Cat_Desc == 'Capital Project Funds'
    )

  return(capital_project_funds)
}


#' Get COBI State Funds Withdrawal
#'
#' This returns all appropriations made from unrestricted "State Funds" including General Fund (1000), Income Tax Fund (2480), and Uniform School Fund (2400).
#'
#' @param years vector of integers indicating year(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' state_funds_withdrawal <- get_cobi_state_funds_withdrawal(2025)

get_cobi_state_funds_withdrawal <- function(years, is1x = NULL) {
  # retrieve data
  cobi_meta <- get_cobi_meta(years, is1x = is1x)

  # filter to State Funds Withdrawal view
  state_funds_withdrawal <- cobi_meta |>
    dplyr::filter(
      .data$CatType == 1,
      .data$Category %in%
        c('1000', '2480', '2400')
    )

  return(state_funds_withdrawal)
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
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' gov_expenditures <- get_cobi_proposed_expenditures(c(2025, 2026), agencies = '060')
#' personnel <- get_cobi_proposed_expenditures(years = 2025, expenditure_categories = 'AA')
#' medicaid_one_time <- get_cobi_proposed_expenditures(years = 2025, line_items = 'KPBA', is1x = TRUE)

get_cobi_proposed_expenditures <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  expenditure_categories = NULL,
  is1x = NULL
) {
  # retrieve data
  cobi_meta <- get_cobi_meta(
    years,
    agencies,
    line_items,
    appr_units,
    expenditure_categories,
    is1x
  )

  # filter to proposed expenditure view
  expenditures <- cobi_meta |>
    dplyr::filter(.data$CatType == 2)

  return(expenditures)
}
