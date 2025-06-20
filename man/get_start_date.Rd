% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_start_date.R
\name{get_start_date}
\alias{get_start_date}
\title{get_start_date}
\usage{
get_start_date(nctid, historical_version = FALSE, data_frame_name = NULL)
}
\arguments{
\item{nctid}{Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.}

\item{historical_version}{Logical. If TRUE, skips downloading historical versions using cthist and directly uses the provided data.}

\item{data_frame_name}{Character. The name of the data frame to use when historical_version is TRUE.}
}
\value{
Data frame containing the start date for the specified clinical trial(s).
}
\description{
This function retrieves the study start date of a clinical trial from ClinicalTrials.gov. The start date is defined as the study start date when the overall status of the trial was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.
}
\examples{

get_start_date("NCT00480077")

}
