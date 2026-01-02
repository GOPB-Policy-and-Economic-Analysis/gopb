# generates url for designated year appropriations table
appr_url <- function(year) {
  paste0("https://le.utah.gov/data/cobi/", year, "_appropriations_data.json")
}

# pulls data from designated url
cobi_data_extraction <- function(url) {
  jsonlite::fromJSON(url)$data |> tibble::as_tibble()
}

