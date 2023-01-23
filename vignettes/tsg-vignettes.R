## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup, eval=F------------------------------------------------------------
#  # Install devtools if not yet installed in your machine
#  if(!('devtools' %in% installed.packages()[,'Package'])){
#     install.packages('devtools')
#  }
#  
#  # Install the package from GitHub
#  devtools::install_github('yng-me/tsg')
#  
#  # Install via R-CRAN
#  install.packages('tsg')
#  

## -----------------------------------------------------------------------------
library(tsg)

## ---- eval=F------------------------------------------------------------------
#  generate_frequency(
#    .data,
#    x,
#    x_group = NULL,
#    x_label = get_config('x_label'),
#    sort_frequency = FALSE,
#    x_as_group = FALSE,
#    include_total = TRUE,
#    include_cumulative = TRUE,
#    exclude_zero_value = FALSE
#  )

## ---- warning=F---------------------------------------------------------------
library(palmerpenguins)
library(gt)

generate_frequency(penguins, species) |> gtx()


## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    x_label = 'Sex'
  ) |> gtx()

## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    x_as_group = TRUE, 
    include_total = FALSE
  ) |> gtx()

## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = species, 
    x_label = 'Species', 
    sort_frequency = TRUE, 
    include_cumulative = FALSE
  ) |> gtx()

## ---- warning=F---------------------------------------------------------------
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = c('skin_color', 'gender'), 
    x_label = 'Sex', 
    include_cumulative = FALSE
  ) |> gtx()

## ---- eval=F------------------------------------------------------------------
#  generate_crosstab(
#    .data,
#    x,
#    y = NULL,
#    x_group = NULL,
#    y_group = NULL,
#    x_label = get_config('x_label'),
#    y_group_separator = '>',
#    x_as_group = FALSE,
#    total_by = 'row',
#    group_values_by = 'statistics',
#    include_frequency = TRUE,
#    include_proportion = TRUE,
#    include_column_total = TRUE,
#    convert_to_percent = TRUE,
#    format_precision = 2,
#    total_label = NULL,
#    ...
#  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex
  ) |> gtx()

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    total_by = 'column'
  ) |> gtx()

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    include_frequency = F
  ) |> gtx()

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    include_proportion = F
  ) |> gtx()

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex, 
    x_group = 'island',
    include_proportion = F
  ) |> gtx()

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex, 
    y_group = 'island',
    convert_to_percent = F,
    include_frequency = F
  ) |> gtx()

## ---- eval=F------------------------------------------------------------------
#  generate_multiple_response(
#    .data,
#    x,
#    pattern = NULL,
#    y = NULL,
#    x_group = NULL,
#    x_label = get_config('x_label'),
#    x_as_group = FALSE,
#    y_group_separator = '>',
#    group_values_by = 'statistics',
#    value_to_count = 1,
#    include_frequency = TRUE,
#    include_proportion = TRUE,
#    format_precision = 2,
#    convert_to_percent = TRUE
#  )

## ---- eval=F------------------------------------------------------------------
#  
#  df <- data.frame(
#    category = c("G1", "G1", "G2", "G1", "G2", "G1"),
#    response = c("AB", "AC", "B", "ABC", "AB", "C"),
#    A = c(1, 1, 0, 1, 1, 0),
#    B = c(1, 0, 1, 1, 1, 0),
#    C = c(0, 1, 0, 1, 0, 1)
#  )
#  
#  df |> generate_multiple_response(category, y = response) |> gtx()

## ---- eval=F------------------------------------------------------------------
#  df |> generate_multiple_response(category, pattern = '^[A:C]$') |> gtx()

## ---- eval=F------------------------------------------------------------------
#  generate_as_list(
#    .data,
#    list_group,
#    x,
#    ...,
#    fn = 'generate_crosstab',
#    list_name_overall = 'ALL',
#    exclude_overall = FALSE,
#    collapse_overall = TRUE,
#    save_as_excel = FALSE,
#    formatted = TRUE,
#    filename = NULL
#  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_as_list(
    list_group = island,
    x = species, 
    sex
  )

