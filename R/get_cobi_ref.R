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


#' Pull COBI financing sources
#'
#' @param year integer indicating fiscal year time stamp at which to snapshot active financing sources
#' @param restricted_only boolean value indicating whether to pull only restricted funds/accounts
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' financing_sources <- get_cobi_financing_sources()
#' restricted_2024 <- get_cobi_financing_sources(2024, TRUE)

get_cobi_financing_sources <- function(
  year = NULL,
  restricted_only = NULL
) {
  # ARGUMENT TYPE CHECKS

  # restricted_only
  if (
    !is.null(restricted_only) &&
      !(is.logical(restricted_only) &&
        length(restricted_only) == 1 &&
        !is.na(restricted_only))
  ) {
    rlang::abort("`restricted_only` must be a boolean value.")
  }

  if (!is.null(year)) {
    # year
    if (!is.numeric(year) || length(year) != 1 || any(year %% 1 != 0)) {
      rlang::abort(
        "`year` must be an integer."
      )
    }

    # if year is provided as the desired timestamp (this should be less common), pull financing sources from indicated fiscal year
    financing_sources <- cobi_data_extraction(categories_url(year))

    financing_sources %<>%
      configure_financing_sources(restricted_only) %>%
      dplyr::arrange(.data$Source_Code)

    return(financing_sources)
  }

  # if no year provided, return recent financing sources by defult
  source_FYs <- get_FYs()

  financing_sources <- purrr::map(source_FYs, categories_url) %>%
    purrr::map(cobi_data_extraction) %>%
    rev() %>% # this reversed binding order is very important... this preserves the most recent year when later removing duplicates across both years, and is used to determine currency of the utilization
    dplyr::bind_rows()

  financing_sources %<>% # configure
    configure_financing_sources(restricted_only) %>% # deduplicate
    dplyr::distinct(.data$Source_Code, .keep_all = TRUE) %>% # order
    dplyr::arrange(.data$Source_Code)

  return(financing_sources)
}


#' Pull COBI expenditure categories
#'
#' @returns tibble (modern data frame)
#' @export
#'
#' @examples
#' expenditure_categories <- get_cobi_expenditure_categories()

get_cobi_expenditure_categories <- function() {
  category_FYs <- get_FYs()

  expenditure_categories <- purrr::map(category_FYs, categories_url) %>%
    purrr::map(cobi_data_extraction) %>%
    rev() %>% # this reversed binding order is very important... this preserves the most recent year when later removing duplicates across both years, and is used to determine currency of the utilization
    dplyr::bind_rows()

  expenditure_categories %<>% # filter to only expenditure categories
    dplyr::filter(.data$CatType == 2) %>% # rename and select
    dplyr::transmute(
      Expenditure_Code = .data$category,
      Expenditure_Description = .data$Category_Desc,
      Last_Utilization_FY = .data$sessionFY
    ) %>% # deduplicate
    dplyr::distinct(.data$Expenditure_Code, .keep_all = TRUE) %>% # order
    dplyr::arrange(.data$Expenditure_Code)

  return(expenditure_categories)
}
