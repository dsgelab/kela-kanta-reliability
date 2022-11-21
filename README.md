# KELA AND KANTA RELIABILITY

## OBJECTIVE

the project's objective is to asses if kela purchases, kanta prescription and kanta delivery datasets are reliable, for doing so we want to:
1. check quality of datasets
2. understand problems contained in the data 
3. join the datasets together 

## DATA

the data used in the project have been filtered for a specific group of medicines: statins (ATC code starting with C10AA) 
Moreover, years have been restricted from 2014 to 2020 for quality and necessity reason

## SCRIPTS

**PRE-PROCESSING**
- *filter_kela_purchase.R*, *filter_kanta_prescription.R* and *filter_kanta_delivery.R* are used to filter for statins the original yearly datasets 
- *data_joiner.R* is responsible for joining single datasets from year 2014 to 2020, as output it produce the 3 completely filtered datasets: *joined_statins_purchases.rds*,   *joined_statins_prescriptions.rds* and *joined_statins_deliveries.rds*
- *data_joiner.R* is also responsible for joinig the 3 filtered datasets in a unique one: *final_full_df.rds* (FULL JOIN) and *final_inner_df.rds* (INNER JOIN)

**ANALYSIS**
- Quality assessment analysis is performed with the *MAIN_quality_check.R* file
- Analysis over the joined data is instead performed with the *MAIN_analysis.R*
- Analysis over the single filtered datasets (not joined) is performed in order to create: 
1. distribution plot of the lag in days between purchases/prescriptions/deliveris, this is created with *preprocess_plot_distribution.R*+*PLOT_distributions.R*
2. trajectory plots of a group of patients in each dataset to asses the quality of information. for this plot use the file *PLOT_trajectories.R*

