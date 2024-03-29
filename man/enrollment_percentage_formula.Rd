% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrollment_percentage_formula.R
\name{enrollment_percentage_formula}
\alias{enrollment_percentage_formula}
\title{enrollment_percentage_formula}
\usage{
enrollment_percentage_formula(actual_enrollment, anticipated_enrollment)
}
\arguments{
\item{actual_enrollment}{An integer value for the actual enrollment}

\item{anticipated_enrollment}{An integer value for the anticipated
enrollment}
}
\value{
The ratio of actual to anticipated enrollment, multiplied
by 100
}
\description{
This function calculates the percentage enrolled based on
'anticipated_enrolment' and 'actual_enrolment' arguments
}
\examples{

enrollment_percentage_formula(5, 500)
}
