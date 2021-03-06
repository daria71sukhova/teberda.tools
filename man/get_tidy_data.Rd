% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tidy_data.R
\name{get_tidy_data}
\alias{get_tidy_data}
\title{Takes one ore two .csv files
Returns tidy wide table with selected types of shoots:
   columns are species' sums of shoot numbers per a year for all samples.
   The first column is named "year", containes years.
   Rows contain shoot numbers of species for a year.}
\usage{
get_tidy_data(
  csv_file,
  csv_file_2 = NULL,
  need_abbr = FALSE,
  state = NULL,
  first_year = NULL,
  last_year = NULL
)
}
\arguments{
\item{csv_file}{Name of .csv file containes raw data. No default.}

\item{csv_file_2}{Name of second .csv file to be united with the first one.
It must be checked, that the data are from the SAME period.
Default to NULL}

\item{need_abbr}{Boolean, if it is needed to abbtriviate species names. Default to FALSE.}

\item{state}{State of shoots. Can be NULL for all states of shoots,
"g" for generative,
"v" for vegetative,
"v+j" for vegetative and juvenile.
Default to NULL.}

\item{first_year}{The first year of the period. Default to NULL.}

\item{last_year}{The last year of the period. Default to NULL.}
}
\description{
Takes one ore two .csv files
Returns tidy wide table with selected types of shoots:
   columns are species' sums of shoot numbers per a year for all samples.
   The first column is named "year", containes years.
   Rows contain shoot numbers of species for a year.
}
