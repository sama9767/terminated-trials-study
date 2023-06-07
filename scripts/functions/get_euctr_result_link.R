#' @title euctr_result_link
#' 
#' @title euctr_result_link_table
#' 
#' @description  Retrieves result link if result were posted for EUCTR entry, otherwise returns 'No result found'
#' @param euctr_id Trial registration number for EUCTR trial
#'
#' @return Table identifiers list with EUCTR entry and links to their result section available webpage
#'
#' @example  euctr_result_link_table("2007-002223-32") ## result ##'ctr-search/trial/2007-002223-32/results'
#' @example  euctr_result_link_table("2005-003854-80") ## no result found ## NA



# TODO: add "https://www.clinicaltrialsregister.eu/" at start of result section link

library(stringr)
library(rvest)

# Define a function to extract the result link for a given euctr_id
euctr_result_link <- function(euctr_id) {
  
  # Check for well-formed input
  assertthat::assert_that(
    is.character(euctr_id),
    grepl(
      "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
      euctr_id
    )
  )
  
  url <- paste0(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
    euctr_id )
  
  # Read in the HTML document from the given URL
  html_doc <- read_html(url)
  
  # Find the "View Results" link in the HTML document
  results_link <- html_doc %>% html_nodes("span.label:contains('Trial results:') + a") %>% html_attr("href") 
  
  # Return the results link, if it exists
  if(length(results_link) > 0){
    return(list(result = "result found", link = results_link))
  } else {
    return(list(result = "no result found", link = ""))
  }
}

# Define a function to create a table of euctr_ids with their result links
euctr_result_link_table <- function(euctr_ids) {
  
  # Check for well-formed input
  assertthat::assert_that(
    is.character(euctr_ids),
    all(grepl(
      "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
      euctr_ids
    ))
  )
  
  # Initialize an empty data frame to store the results
  table_identifiers <- data.frame(
    euctr_id = character(),
    result = character(),
    link = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each euctr_id and extract the results link
  for (id in euctr_ids) {
    result_link <- euctr_result_link(id)
    table_identifiers <- rbind(table_identifiers, 
                               data.frame(euctr_id = id,
                                          result = result_link$result,
                                          link = result_link$link,
                                          stringsAsFactors = FALSE))
  }
  
  # Return the final data frame
  return(table_identifiers)
}
