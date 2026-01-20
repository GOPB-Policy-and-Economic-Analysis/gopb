# ALL OF THESE PULL THE appropriations_data JSON ENDPOINT specifically
# NOT YET PUTTING A LOT OF THIS REPETITIVE CODE INTO ANOTHER FUNCTION LIKE cobi_query() OR SOMETHING UNTIL I CAN SEE WHAT ALL IS TRULY REPRODUCIBLE WITH THE INCLUSION OF THE VARIOUS USES/FILTERS DOWN THE ROAD

#' Pull COBI appropriations
#'
#' @param years A vector of integers indicating year(s) of interest
#' @param agencies A character vector indicating agency(ies) of interest
#' @param line_items A character vector indicating line item(s) of interest
#' @param appr_units A character vector indicating appropriation unit(s) of interest
#' @param financing_sources A character vector indicating financing source(s) of interest
#'
#' @returns A tibble (modern data frame)
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
  data <- purrr::map(url_years, appr_url) %>%
    purrr::map(cobi_data_extraction) %>%
    dplyr::bind_rows()

  # filter to specified appropriations view
  data %<>%
    dplyr::filter(FY %in% years, CatType == 1)

  if (!is.null(agencies)) {
    data %<>% dplyr::filter(Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    data %<>% dplyr::filter(Line_Item %in% line_items)
  }
  if (!is.null(appr_units)) {
    data %<>% dplyr::filter(Appr_Unit %in% appr_units)
  }
  if (!is.null(financing_sources)) {
    data %<>% dplyr::filter(Category %in% financing_sources)
  }

  data
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


cobi_orgs <- function(year = NULL) {
  # if year is provided (this should be less common), pull orgs from indicated year
  if (!is.null(year)) {
    cobi_data_extraction(org_url(year))
  }

  # if no year provided, return recent orgs by defult
  FYs <- get_FYs()
  last_year <- cobi_data_extraction(org_url(FYs$previous))
  this_year <- cobi_data_extraction(org_url(FYs$current))
  # this binding order is very important... the preserved year after removing duplicates across both years is the information used to determine current utilization
  recent_orgs <- dplyr::bind_rows(this_year, last_year)

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
