# terminated-trials-study

# Overview
This package provides functions to analyze terminated clinical trials from ClinicalTrials.gov. Some functions can also be used for trials with other recruitment statuses.

 ## How to install
```{r}
install.packages("devtools")
library(devtools)
install_github("sama9767/terminated-trials-study")
library(terminatedtrialsstudy)
```
|function|description|
|----|----|
|`get_anticipated_enrollment `|This function retrieves the estimated enrollment of a clinical trial as reported on ClinicalTrials.gov.|
|`get_actual_enrollment` |This function retrieves the final recorded enrollment of a clinical trial as reported on ClinicalTrials.gov. |
|`degree_of_enrollment`| This function checks the enrollment status in a dataframe by calculating the enrollment percentage based on the values in the 'anticipated_enrollment' and 'actual_enrollment' columns.  It also computes trial and dataset summaries and additional statistics related to the anticipated and actual enrollments. |
|`get_why_stopped`| This function retrieves the reason for trial termination, or returns `no result posted` if the reason is unavailable.|
|`has_summary_result`|This function checks whether a ClinicalTrials.gov trial has posted summary results and returns a logical vector TRUE if results were posted, otherwise FALSE|
|`start_date`|This function creates the start date of a clinical trial, which is defined as the study start date when the overall status of the trial was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.|
|`stop_date`|This function creates stop_date of a clinical trial, which is defined as the actual primary completion date reported on the clinical trial record when its overall status was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.|
|`duration_of_trial`|This function calculates the duration of the trial in days (trial days) for each row of a data frame, based on the provided start and stop date columns.|
|`get_euctr_results`|This function retrieves the result link if results were posted for EUCTR entry, otherwise returns 'No result found'|
|`aggregrate_arms`|This function aggregates the serious adverse event (SAE) data by trial and arm, calculating the total number of participants affected and the total number at risk for each group within a clinical trial.|
|`get_ctgov_sae_groups`|This function retrieves the serious adverse event (SAE) group data from ClinicalTrials.gov for a given clinical trial ID.|


 Thank you for your attention. If you find any bug, please open an issue in the tracker.

TTS team out ✌️


