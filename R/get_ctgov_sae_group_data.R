#' @title get_ctgov_sae_groups
#'
#' @description This function retrieves the serious adverse event (SAE) group data from ClinicalTrials.gov for a given clinical trial ID.
#'
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return A tibble with the trial registration number, group ID, title, description, serious number affected, and serious number at risk for each SAE group.
#'
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples get_ctgov_sae_groups("NCT02453282")
#'
#' 
#

get_ctgov_sae_groups <- function (nctid) {

    groups <- tribble(
        ~nctid,
        ~group_id,
        ~title,
        ~description,
        ~seriousNumAffected,
        ~seriousNumAtRisk
    )
    
    url <- paste0(
        "https://clinicaltrials.gov/api/int/studies/download?format=json&term=",
        nctid
    )
    tmp <- tempfile()
    download.file(
        url,
        tmp
    )
    trialdata <- read_json(tmp)
    unlink(tmp)

    for (group in trialdata[[1]]$resultsSection$adverseEventsModule$eventGroups) {
        newrow <- tribble(
            ~nctid,
            ~group_id,
            ~title,
            ~description,
            ~seriousNumAffected,
            ~seriousNumAtRisk,
            nctid,
            group$id,
            group$title,
            ifelse (! is_null(group$description), group$description, as.character(NA)),
            group$seriousNumAffected,
            group$seriousNumAtRisk
        )

        groups <- groups |>
            bind_rows(newrow)
            
    }

    return(groups)
}

