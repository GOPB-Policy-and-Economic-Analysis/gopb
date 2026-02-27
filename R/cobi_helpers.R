# generates url for designated year orgs table
org_url <- function(year) {
  return(paste0("https://le.utah.gov/data/cobi/", year, "_orgs.json"))
}

# captures current and previous FY
get_FYs <- function() {
  if (lubridate::month(Sys.Date()) >= 7) {
    previousFY <- lubridate::year(Sys.Date())
    currentFY <- lubridate::year(Sys.Date()) + 1
  } else {
    previousFY <- lubridate::year(Sys.Date()) - 1
    currentFY <- lubridate::year(Sys.Date())
  }
  return(list(previous = previousFY, current = currentFY))
}

# configures COBI orgs view
configure_org_view <- function(orgs, agencies, line_items) {
  orgs %<>% # filter relevant appropriation categories
    dplyr::filter(
      .data$Line_Item_Cat %in%
        c(
          "RAT",
          "ERF",
          "CPF",
          "PRP",
          "FID",
          "GEN",
          "FRT"
        )
    ) %>% # rename
    dplyr::rename(
      Agency_Name = .data$Agency_Desc,
      Last_Utilization_FY = .data$sessionFY
    ) %>% # select
    dplyr::select(
      .data$Agency_Name,
      .data$Agency,
      .data$Line_Item_Desc,
      .data$Line_Item,
      .data$Appr_Unit_Desc,
      .data$Appr_Unit,
      .data$Last_Utilization_FY
    )
  # more filtering
  if (!is.null(agencies)) {
    orgs %<>% dplyr::filter(.data$Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    orgs %<>% dplyr::filter(.data$Line_Item %in% line_items)
  }

  return(orgs)
}

# configures COBI financing soures view
configure_financing_sources <- function(financing_sources, restricted_only) {
  # observe restricted_only directive
  if (identical(restricted_only, TRUE)) {
    financing_sources %<>% dplyr::filter(.data$fundHL == "Restricted Funds")
  }

  financing_sources %<>% # filter to only appropriations financing sources
    dplyr::filter(.data$CatType == 1) %>% # rename and select --(go through all of these native names and define them with Brian Fay, determining which ones to keep for the office)
    dplyr::transmute(
      Source_Code = .data$category,
      Source_Description = .data$Category_Desc,
      Is_State_Fund = .data$StateFund,
      SF_All = .data$SFAll,
      Source_Rollup = .data$SourceRollupName,
      Category_Rollup = .data$cat_rollup_desc,
      Fund_High_Level = .data$fundHL,
      Last_Utilization_FY = .data$sessionFY
    )

  return(financing_sources)
}

# generates url for designated year categories table
categories_url <- function(year) {
  return(paste0("https://le.utah.gov/data/cobi/", year, "_categories.json"))
}

# generates url for designated year appropriations table
appr_url <- function(year) {
  return(paste0(
    "https://le.utah.gov/data/cobi/",
    year,
    "_appropriations_data.json"
  ))
}


# pulls data from designated url
cobi_data_extraction <- function(url) {
  return(jsonlite::fromJSON(url)$data |> tibble::as_tibble())
}

# checks character parameter types
char_vector_check <- function(arg, arg_name) {
  # handle unquoted/invalid argument object(s)
  verified_arg <- tryCatch(
    {
      arg
    },
    error = function(e) {
      rlang::abort(paste0(
        "Invalid argument provided for `",
        arg_name,
        "`. Be sure to wrap character-type arguments in quotes."
      ))
    }
  )

  # ensure correct type
  if (
    !is.null(verified_arg) &&
      (!is.character(verified_arg) || length(verified_arg) < 1)
  ) {
    rlang::abort(paste0(
      "`",
      arg_name,
      "` must be a non-empty character vector or left NULL."
    ))
  }
}

save_file <- function(data, filename) {
  # establish file path
  download_location <- fs::path(
    fs::path_home("Downloads"),
    filename,
    ext = "xlsx"
  )

  # write file
  writexl::write_xlsx(
    data,
    path = download_location
  )
}
