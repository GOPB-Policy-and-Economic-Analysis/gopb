# generates url for designated year appropriations table
appr_url <- function(year) {
  paste0("https://le.utah.gov/data/cobi/", year, "_appropriations_data.json")
}

# generates url for designated year orgs table
org_url <- function(year) {
  paste0("https://le.utah.gov/data/cobi/", year, "_orgs.json")
}

# pulls data from designated url
cobi_data_extraction <- function(url) {
  jsonlite::fromJSON(url)$data |> tibble::as_tibble()
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
  list(previous = previousFY, current = currentFY)
}
