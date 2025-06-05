# cobi <- function(year){
#   box::use(
#     json = jsonlite,
#     tibble,
#     mag = magrittr[...],
#     d = dplyr,
#     scales,
#     jan = janitor
#   )
  
  
#   year <- 2025 # year of interest, assuming completed its GS budget
  
#   # ACTUALLY DETERMINE WHAT SESSION TO BASE THIS OFF OF (SEE ADDITION SESSIONS LIKE "2024GS")
#   # mark as true if GS of the year of interest has a completed/updated budget
#   budget_finalized = FALSE
  
  
#   # json data from more recent URL
#   url <- paste0("https://le.utah.gov/data/cobi/", year, "_appropriations_data.json")
#   recent_data <- json$fromJSON(url)$data %>% tibble$as_tibble()
  
  
#   if(budget_finalized){
  
#     prev_year <- year - 1
  
#     # json data from year-old URL
#     url <- paste0("https://le.utah.gov/data/cobi/", prev_year, "_appropriations_data.json")
#     prev_data <- json$fromJSON(url)$data %>% tibble$as_tibble()
    
#     updated <- rbind(prev_data, recent_data)
  
#   }else(updated <- recent_data)
  
  
#   mcos <- updated %>%
#     d$filter(
#       FY == year,
#       Line_Item_Desc == "Integrated Health",
#       Appr_Unit_Desc == "Medicaid Accountable Care Organizations",
#       CatType == 1,
#       FY == "2025") %>%
#     d$transmute(
#       'Appropriation' = ItemKey,
#       ItemName,
#       is1x,
#       Cat_Sequence,
#       Category,
#       Category_Desc,
#       Amount,
#       )
  
#   mcos_by_source <- mcos %>% d$group_by(Category_Desc, is1x) %>%
#     d$summarize(Appropriated = sum(Amount)) |> 
#     d$ungroup() |> 
#     d$transmute(
#       Fund = paste0(
#         Category_Desc,
#         ifelse(
#           is1x == TRUE,
#           ", One-Time",
#           "")
#         ),
#       Appropriated = scales$dollar(Appropriated)
#     ) |> 
#     d$arrange(Fund) # |> 
#     # jan$adorn_totals("row")
    
  
#   mcos %>% d$filter(Category_Desc == "Expendable Receipts")


#' Print a sentence
#'
#' @param sentence A character vector with one element.
#' 
#' @return A character vector.
#' @export
#' 
#' @examples
#' sentence <- "My sentence."
#' cobi(sentence)
cobi <- function(sentence) {
  sentence
}