# ALL OF THESE PULL THE appropriations_data JSON ENDPOINT specifically
# NOT YET PUTTING A LOT OF THIS REPETITIVE CODE INTO ANOTHER FUNCTION LIKE cobi_query() OR SOMETHING UNTIL I CAN SEE WHAT ALL IS TRULY REPRODUCIBLE WITH THE INCLUSION OF THE VARIOUS USES/FILTERS DOWN THE ROAD

#' Pull COBI appropriations
#'
#' @param years A vector of integers indicating year(s) of interest
#'
#' @returns A tibble (modern data frame)
#' @export
#'
#' @examples
#' cobi_data <- cobi_approps(c(2024, 2025))
cobi_approps <- function(years) {
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

  # filter out preceding year and to appropriations view
  data %<>%
    dplyr::filter(FY %in% years) |>
    dplyr::filter(CatType == 1)
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
  FYs <- getFYs()
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
