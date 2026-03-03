#' COBI Appropriations Data Dictionary
#'
#' Reference table documenting COBI appropriations extracts returned by *_cobi_approps functions
#'
#' @format ## `cobi_approps_dictionary`
#' A data frame with 27 rows and 2 columns:
#' \describe{
#'   \item{Field}{field name in COBI appropriations extract}
#'   \item{Definition}{definition of field name}
#' }
#' @source Developed by Jackson Bolos (GOPB) and Brian Fay (LFA)
"cobi_approps_dictionary"


#' COBI 'Line_Item_Cat_Desc' Dictionary
#'
#' Reference table documenting the 'Line_Item_Cat_Desc' field in COBI appropriations extracts returned by corresponding get_cobi_* functions, as defined on COBI
#'
#' @format ## `cobi_approp_line_categories`
#' A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{Line_Item_Cat_Desc}{value of 'Line_Item_Cat_Desc' field in COBI appropriations extract}
#'   \item{COBI Org Alias}{alias ID used by LFA for joining with other tables}
#'   \item{Definition}{definition of field value}
#' }
#' @source <https://cobi.utah.gov/2025/1/financials>
"cobi_approp_line_categories"
