# terminated-trials-study  [in development]

# Overview
This package provides functions to analyze terminated clinical trials from ClinicalTrials.gov.

## Input
The dataset used for processing should include trial ID (nctid) of ClinicalTrials.gov registered trials with recruitment status ('recruitment_status') specified as "Terminated" in the registry. ClinicalTrials.gov defines 'terminated' as trials where the recruitment or enrollment of participants has halted prematurely and will not resume, and participants are no longer being examined or receiving intervention. 

## How to install
```{r}
install.packages("devtools")
library(devtools)
install_github("sama9767/terminated-trials-study")
library(terminatedtrialsstudy)
```
## Functions
`get_anticipated_enrollment `: This function retrieves the estimated enrollment of a clinical trial as reported on ClinicalTrials.gov.

`get_actual_enrollment`: This function retrieves the final recorded enrollment of a clinical trial as reported on ClinicalTrials.gov. 

`degree_of_enrollment`: This function checks the enrolment status in a dataframe by calculating the enrolment percentage based on the values in the 'anticipated_enrolment' and 'actual_enrolment' columns. 

`get_why_stopped`:  This function retrieves the reason for trial termination, or returns `no result posted` if the reason is not available.

`has_summary_result`: This function checks whether a ClinicalTrials.gov trial has posted summary results and returns a logical vector TRUE if results were posted, otherwise FALSE

`start_date`: This function creates the start date of a clinical trial, which is defined as the study start date when the overall status of the trial was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.

`stop_date`: This function creates stop_date of a clinical trial, which is defined as the actual primary completion date reported on the clinical trial record when its overall status was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.

`duration_of_trial`: This function calculates the duration of the trial in days for each row of a data frame, based on the provided start and stop date columns.

`get_euctr_results`: This function retrieves the result link if results were posted for EUCTR entry, otherwise returns 'No result found'

`adverse_events`: This function retrieves the adverse events section for an EUCTR entry with the result section


 




