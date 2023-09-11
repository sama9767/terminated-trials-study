#' @title adverse_events
#'
#' @description Retrieves adverse events section for a EUCTR entry
#'     with result section
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number with result section 
#'
#' @return Adverse event table of the trial. Note will not fetch if
#'     result section is an attachment or summary report link
#'
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#'
#' adverse_events("2008-006649-18")

adverse_events <- function(trn) {
    
    ## Throw error if input is not well-formed
    assertthat::assert_that(
        grepl("^[0-9]{4}-[0-9]{6}-[0-9]{2}$", trn),
        msg = "TRN is not well-formed"
    )
    
    ## Construct the URL
    url <- paste0(
        "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
        trn,
        "/results#adverseEventsSection"
    )
    
    ## Read the HTML content from the URL
    html <- rvest::read_html(url)
  
    ## Extract the Adverse events section table using its ID
    adverse_events_table <- html %>%
        rvest::html_nodes("#adverseEventsSection table") %>%
        rvest::html_table(fill = TRUE) # Parses tables into data frames
  
    ## If the Adverse events section table is found, return the table
    if(length(adverse_events_table) > 0) {
        return(adverse_events_table)
    } else {
        return("No result found")
    }
}
