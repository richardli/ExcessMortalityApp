# Excess Mortality Calculator

This is the Shiny Application for excess mortality calculation, developed by the [openVA team](https://openva.net), supported by Vital Strategies. 

# Get started

The App can be installed in `R` by running the following command
```
library(devtools)
install_github("richardli/ExcessMortalityApp")
```

The App can be launched by running the following command in `R`. 
```
library(ExcessMortalityApp)
launchApp()
```

The App will then open in the default browser.

# Input data format

The input data file should be a CSV file containing death counts over time, and possibly by subpopulation. The required data input is in the "long format", where each row correspond to the death count of a given population at a given time. The dataset should include at least the following required columns (with the exact column names):

+ `year`: numerical value, e.g., 2015, 2016, ...
+ `month` or `week`: numerical value. For monthly data input, the `month` field is required to be from 1 to 12. For weekly data input, the `week` field is required to be from 1 to 53.
+ `deaths`: numerical value. This is the death count for the corresponding year and month/week. 


There are additional columns that can be included in the data. The column names of these fields can be user-defined.

+ **Sex**: if death count of a given row is for a specific sex.
+ **Age**: if death count of a given row is for a specific age group. Please note the age needs to be grouped into bins.
+ **Population**: the total population corresponding to the row. For example, for a row corresponding to male and age group of 65 or above in a certain month. The `death` field specifies the number of male deaths above age 65 in that month, and the `population` field specifies the total population of male above age 65 in that month.

The App adds up all deaths within each month/week to produce the total deaths and total population at each time point. When both sex and age are specified, the App adds up all deaths within each age/sex to produce the marginal total deaths and total population by age and sex. 

The variables corresponding to age, sex, and population need to be specified manually in the App. If the variables are named "sex", "age", and "population" already, the variables will be automatically picked up by the App. When left unspecified, the App will ignore the age/sex breakdown.


# Version history

## version 0.1.0
This version replicates the calculator by [Vital Strategies](https://preventepidemics.org/covid19/resources/excess-mortality/).




