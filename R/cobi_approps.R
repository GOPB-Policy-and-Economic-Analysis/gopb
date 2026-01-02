view <- 'appropriations' # null, appropriations, expected_expenditures

years <- c(2024, 2025) # year(s) of interest - numeric list

# NOT YET PUTTING A LOT OF THIS REPETITIVE CODE INTO ANOTHER FUNCTION LIKE cobi_query() OR SOMETHING UNTIL I CAN SEE WHAT ALL IS TRULY REPRODUCIBLE WITH THE INCLUSION OF THE VARIOUS USES/FILTERS DOWN THE ROAD
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


cobi_expected_expenditures <- function(years) {
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
