
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R Youth Homelessness Analyzer *(RYHA)*

<!-- badges: start -->
<!-- badges: end -->

The goal of `{ryha}` is to provide a web-based application for in-depth,
user-friendly consumption of Youth Homelessness data across the State of
Ohio.

## Installation

This Shiny application is built using the [{golem}
framework](https://thinkr-open.github.io/golem/), which means that this
app is actually an *R package*.

You can install the development version of `{ryha}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ketchbrookanalytics/ryha")
```

## Running the App

After installing the package, you can launch the app using the package’s
`run_app()` function.

``` r
ryha::run_app()
```

## Data

### Definitions

-   *“Youth”* is defined as any person within the program who is age 24
    or younger.

### Source

The data within this application are collected by each grantee in a
format compliant with the *HMIS Data Standards*.

The data dictionary for each element across each file can be found here:
<https://files.hudexchange.info/resources/documents/FY-2022-HMIS-Data-Dictionary.pdf>

### Methodology for Analysis

This app gives users the ability to slice & filter the data across many
different demographic variables and attributes about each individual in
the program. These attributes are grouped into the following categories
within the app:

| Attribute                                    | Source File               |
|:---------------------------------------------|:--------------------------|
| Age                                          | *Client.csv*              |
| Ethnicity                                    | *Client.csv*              |
| Gender                                       | *Client.csv*              |
| Military History                             | *Client.csv*              |
| Disabilities                                 | *Disabilities.csv*        |
| Education                                    | *EmploymentEducation.csv* |
| Employment                                   | *EmploymentEducation.csv* |
| Living Situation                             | *Enrollment.csv*          |
| Welfare History                              | *Enrollment.csv*          |
| Program Exit Status                          | *Exit.csv*                |
| Trafficking                                  | *Exit.csv*                |
| Domestic Violence History                    | *HealthAndDV.csv*         |
| Overall Health (General, Dental, and Mental) | *HealthAndDV.csv*         |
| Pregnancy Status                             | *HealthAndDV.csv*         |
| Income                                       | *IncomeBenefits.csv*      |
| Unemployment & Other Benefits Received       | *IncomeBenefits.csv*      |
| Insurance                                    | *IncomeBenefits.csv*      |

### Historical Database

Privileged users have access to historical data within the application,
which gives the ability to view trends over time across metrics.

### Individual Program File Upload

Non-privileged users have the ability to upload their own (individual
program) data into the app for analysis.
