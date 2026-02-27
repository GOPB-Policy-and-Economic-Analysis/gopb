# IF CatType 2 ENDS UP BEING PULLED, SEE WHAT SECTIONS OF CODE CAN BE GROUPED INTO HELPERS

#' Pull COBI orgs
#'
#' @param year integer indicating fiscal year time stamp at which to snapshot the budget organization hierarchy
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' orgs <- get_cobi_orgs()
#' dhhs_orgs <- get_cobi_orgs(agencies = "250")
#' medicaid_gopb_orgs <- get_cobi_orgs(line_items = c("CBAA", "KPBA"))
#' orgs_2023 <- get_cobi_orgs(2023)

get_cobi_orgs <- function(year = NULL, agencies = NULL, line_items = NULL) {
  # ARGUMENT TYPE CHECKS

  # character parameters
  char_vector_check(agencies, "agencies")
  char_vector_check(line_items, "line_items")

  if (!is.null(year)) {
    # year
    if (!is.numeric(year) || length(year) != 1 || any(year %% 1 != 0)) {
      rlang::abort(
        "`year` must be an integer."
      )
    }

    # if year is provided as the desired timestamp (this should be less common), pull orgs from indicated fiscal year
    orgs <- cobi_data_extraction(org_url(year))

    orgs %<>% # configure
      configure_org_view(agencies, line_items) %>% # order
      dplyr::arrange(
        .data$Agency_Name,
        .data$Line_Item,
        .data$Appr_Unit
      )

    return(orgs)
  }

  # if no year provided, return recent orgs by defult
  org_FYs <- get_FYs()

  orgs <- purrr::map(org_FYs, org_url) %>%
    purrr::map(cobi_data_extraction) %>%
    rev() %>% # this reversed binding order is very important... this preserves the most recent year when later removing duplicates across both years, and is used to determine currency of the utilization
    dplyr::bind_rows()

  orgs %<>% # configure
    configure_org_view(agencies, line_items) %>% # deduplicate
    dplyr::distinct(
      dplyr::across(
        -c(
          .data$Agency_Name,
          .data$Line_Item_Desc,
          .data$Appr_Unit_Desc,
          .data$Last_Utilization_FY
        )
      ),
      .keep_all = TRUE
    ) %>% # order
    dplyr::arrange(
      .data$Agency_Name,
      .data$Line_Item,
      .data$Appr_Unit
    )

  return(orgs)
}


#' Pull COBI meta appropriations
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' meta_approp <- get_cobi_meta_approps(2025)
#' meta_approp <- get_cobi_meta_approps(years = 2025, appr_units = 'KPBAC', financing_sources = '9750')
#' meta_approp <- get_cobi_meta_approps(years = c(2024, 2025), line_items = 'CBAA')
#' meta_approp <- get_cobi_meta_approps(years = 2025, line_items = '2105', financing_sources = '9800')
#' meta_approp <- get_cobi_meta_approps(years = 2025, agencies = '060', financing_sources = '1000')

get_cobi_meta_approps <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  financing_sources = NULL,
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
  char_vector_check(financing_sources, "financing_sources")

  # is1x
  if (
    !is.null(is1x) && !(is.logical(is1x) && length(is1x) == 1 && !is.na(is1x))
  ) {
    rlang::abort("`is1x` must be a boolean value.")
  }

  # drop back to when first year of interest was the out-year to capture initial appropriations
  url_years <- unique(c(min(years) - 1, years))

  # create unique urls and pull data with them
  approps <- purrr::map(url_years, appr_url) %>%
    purrr::map(cobi_data_extraction) %>%
    dplyr::bind_rows()

  # filter to specified appropriations view
  approps %<>%
    dplyr::filter(.data$FY %in% years, .data$CatType == 1)

  if (!is.null(agencies)) {
    approps %<>% dplyr::filter(.data$Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    approps %<>% dplyr::filter(.data$Line_Item %in% line_items)
  }
  if (!is.null(appr_units)) {
    approps %<>% dplyr::filter(.data$Appr_Unit %in% appr_units)
  }
  if (!is.null(financing_sources)) {
    approps %<>% dplyr::filter(.data$Category %in% financing_sources)
  }
  if (!is.null(is1x)) {
    approps %<>% dplyr::filter(.data$is1x == is1x)
  }

  return(approps)
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
  approps <- get_cobi_meta_approps(
    years,
    agencies,
    line_items,
    appr_units,
    financing_sources,
    is1x
  )

  # filter to State Budget view
  state_budget <- approps |>
    dplyr::filter(
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


#' Get COBI State Fund Withdrawal
#'
#' @param years vector of integers indicating year(s) of interest
#' @param is1x boolean value indicating whether amount is one-time
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' state_fund_withdrawal <- get_cobi_state_fund_withdrawal(2025)

get_cobi_state_fund_withdrawal <- function(years, is1x = NULL) {
  # retrieve data
  approps <- get_cobi_meta_approps(years, is1x = is1x)

  # filter to State Fund Expense view
  state_fund_withdrawal <- approps |>
    dplyr::filter(
      .data$Category %in%
        c('1000', '2480', '2400')
    )

  return(state_fund_withdrawal)
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
  approps <- get_cobi_meta_approps(
    years,
    agencies,
    line_items,
    financing_sources = financing_sources,
    is1x = is1x
  )

  # filter to Restricted Transfers view
  restricted_transfers <- approps |>
    dplyr::filter(
      .data$Line_Item_Cat_Desc == 'Restricted Fund and Account Transfers'
    )

  return(restricted_transfers)
}


cobi_expected_expenditure <- function(years) {
  # drop back to when first year of interest was the out-year to capture initial appropriations
  init_url <- appr_url(min(years) - 1)

  # pull out-year data
  data <- cobi_data_extraction(init_url)

  # pull normal range
  for (year in years) {
    url <- appr_url(year)

    # add to data
    data <- dplyr::bind_rows(
      data,
      cobi_data_extraction(url)
    )
  }

  # filter out preceding year and to anticipated expenditures view
  data %<>%
    dplyr::filter(.data$FY %in% years) |>
    dplyr::filter(.data$CatType == 2)
}
