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
      Line_Item_Cat %in%
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
      Agency_Name = Agency_Desc,
      Last_Utilization_FY = sessionFY
    ) %>% # select
    dplyr::select(
      Agency_Name,
      Agency,
      Line_Item_Desc,
      Line_Item,
      Appr_Unit_Desc,
      Appr_Unit,
      Last_Utilization_FY
    )
  # more filtering
  if (!is.null(agencies)) {
    orgs %<>% dplyr::filter(Agency %in% agencies)
  }
  if (!is.null(line_items)) {
    orgs %<>% dplyr::filter(Line_Item %in% line_items)
  }

  return(orgs)
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
  # evaluate argument
  expr <- rlang::enexpr(arg)

  if (!is.null(arg) && (!is.character(arg) || length(arg) < 1)) {
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
