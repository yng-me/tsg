% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_as_list.R
\name{generate_as_list}
\alias{generate_as_list}
\title{Generate summary tables as a list based on defined grouping or aggregation}
\usage{
generate_as_list(
  .data,
  list_group,
  indicator,
  ...,
  fn = "generate_crosstab",
  list_name_overall = "ALL",
  exclude_overall = FALSE,
  collapse_overall = TRUE,
  save_as_excel = FALSE,
  title = NULL,
  description = NULL,
  footnote = NULL,
  source_note = NULL,
  formatted = TRUE,
  filename = NULL,
  y_group_separator = ">",
  distinct_stub_head = F
)
}
\arguments{
\item{.data}{\strong{Required}. A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.}

\item{list_group}{\strong{Required}. A factor or categorical variable from \code{.data} to be used grouping for the list generated.}

\item{indicator}{\strong{Required}. column name of the variable to be used as categories.}

\item{...}{Accepts valid arguments of the selected function in \code{fn}.}

\item{fn}{Accepts \code{generate_frequency} | \code{generate_crosstab}. The default value is \code{generate_frequency}.}

\item{list_name_overall}{Accepts a string that will be used as name/label for the first list. The default value is \code{All}.}

\item{exclude_overall}{Whether to exclude the overall (aggregate) table (first table) in the list.}

\item{collapse_overall}{Whether to conform the structure of the first table with the rest in the list.}

\item{save_as_excel}{\code{Bolean}. Whether to save the output in Excel. Default is \code{FALSE}.}

\item{title}{Title of table to be applied for each sheet.}

\item{description}{Table description.}

\item{footnote}{Table footnote.}

\item{source_note}{Table footnote.}

\item{formatted}{Whether to apply formatting for the Excel output. Default is \code{FALSE}.}

\item{filename}{Valid filename with \code{.xlsx} extension. If not specified, it will use \code{tsg_list.xlsx} as a filename and will be saved in the current working directory.}

\item{y_group_separator}{Column separator that defines the table hierarchy.}

\item{distinct_stub_head}{Whether to use \code{indicator} variable as x (if \code{FALSE}) or y (if \code{TRUE})}
}
\value{
Returns a list of tables aggregated based on values defined in \code{list_group}.
}
\description{
This function allows you to generate summary tables based on a defined grouping or aggregation.
You can optionally return table values in either frequency or proportion/percentage or both.
It uses any \code{tsg_*} valid functions as calculator to generate desired tabulation structure.
}
\examples{
mtcars_by_cyl_freq <- mtcars |>
  generate_as_list(list_group = cyl, indicator = am)

mtcars_by_cyl_freq

mtcars_by_cyl_prop <- mtcars |>
  generate_as_list(list_group = cyl, indicator = am, gear, fn = 'generate_crosstab')

mtcars_by_cyl_prop
}
