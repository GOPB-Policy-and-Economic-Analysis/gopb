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
#' gopb_medicaid_orgs <- get_cobi_orgs(line_items = c("CBAA", "KPBA"))
#' orgs_2023 <- get_cobi_orgs(2023)

get_cobi_orgs <- function(year = NULL, agencies = NULL, line_items = NULL) {
  # if year is provided as the desired timestamp (this should be less common), pull orgs from indicated fiscal year
  if (!is.null(year)) {
    # check year type
    if (!is.numeric(year) || length(year) != 1 || any(year %% 1 != 0)) {
      rlang::abort(
        "`year` must be an integer."
      )
    }
    orgs <- cobi_data_extraction(org_url(year))

    orgs %<>% # configure
      configure_org_view(agencies, line_items) %>% # order
      dplyr::arrange(
        Agency_Name,
        Line_Item,
        Appr_Unit
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
          Agency_Name,
          Line_Item_Desc,
          Appr_Unit_Desc,
          Last_Utilization_FY
        )
      ),
      .keep_all = TRUE
    ) %>% # order
    dplyr::arrange(
      Agency_Name,
      Line_Item,
      Appr_Unit
    )

  return(orgs)
}


#' Pull COBI appropriations
#'
#' @param years vector of integers indicating year(s) of interest
#' @param agencies character vector indicating agency(ies) of interest
#' @param line_items character vector indicating line item(s) of interest
#' @param appr_units character vector indicating appropriation unit(s) of interest
#' @param financing_sources character vector indicating financing source(s) of interest
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' cobi_data <- get_cobi_approps(2025)
#' cobi_data <- get_cobi_approps(years = 2025, appr_units = 'KPBAC', financing_sources = '9750')
#' cobi_data <- get_cobi_approps(years = c(2024, 2025), line_items = 'CBAA')
#' cobi_data <- get_cobi_approps(years = 2025, line_items = '2105', financing_sources = '9800')
#' cobi_data <- get_cobi_approps(years = c(2024, 2025), agencies = '060', financing_sources = '1000')

get_cobi_approps <- function(
  years,
  agencies = NULL,
  line_items = NULL,
  appr_units = NULL,
  financing_sources = NULL
) {
  # ARGUMENT TYPE CHECKS

  # check years
  if (!is.numeric(years) || length(years) < 1 || any(years %% 1 != 0)) {
    rlang::abort(
      "`years` must be a non-empty vector of integers."
    )
  }

  # character parameter type checks
  char_vector_check(agencies, "agencies")
  char_vector_check(line_items, "line_items")
  char_vector_check(appr_units, "appropriation_units")
  char_vector_check(financing_sources, "financing_sources")

  # drop back to when first year of interest was the out-year to capture initial appropriations
  url_years <- unique(c(min(years) - 1, years))

  # create unique urls and pull data with them
  approps <- purrr::map(url_years, appr_url) %>%
    purrr::map(cobi_data_extraction) %>%
    dplyr::bind_rows()

  # filter to specified appropriations view
  approps %<>%
    dplyr::filter(FY %in% years, CatType == 1)

  if (!is.null(agencies)) {
    approps %<>% dplyr::filter(Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    approps %<>% dplyr::filter(Line_Item %in% line_items)
  }
  if (!is.null(appr_units)) {
    approps %<>% dplyr::filter(Appr_Unit %in% appr_units)
  }
  if (!is.null(financing_sources)) {
    approps %<>% dplyr::filter(Category %in% financing_sources)
  }

  return(approps)
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
    dplyr::filter(FY %in% years) |>
    dplyr::filter(CatType == 2)
}
