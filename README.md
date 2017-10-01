# trend-analysis
R scripts to monitor the emerging trends in software engineering by analysis data from: Scopus, Stackoverflow, and Twitter. This project is part of the **Emerging Trends in Software Engineering** course at Oulu University. The scripts allow for gathering and analysing data obtained from the three aforementioned sources in order to monitor the emerging trends regarding in a certain topic/theme (for this course it is going to be the emerging trends in software engineering which explains the choice of sources). This scripts in this repository are an adapted and modified version of the scripts provided in the course.

## Data Gathering:
The `trend-analysis/GetData.R` file contains the main script that allows for gathering the data from the three sources.
steps to run the `GetData.R` script:
- Update the `workingDir` variable with the path of your local project. 
- Update the `libPath` variable with the path of your local R lib.
> If the `libPath` is not updated, make sure to create an empty library **folder**: `trend-analysis/Library/`
- Choose a name for the output files. The output location is set to:  `trend-analysis/data/`
- Update the `query_string` variable with your search string.

### To be able to get the data from Scopus:
- Get your Scopus API key (https://dev.elsevier.com/)
- Update API key in `trend-analysis/Helpers/Set_MyScopus_APIKey.R`
> set_api_key("your_scopus_api_key")
- Make sure you are connected to a network that allows data retrival from Scopus.
