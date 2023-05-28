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
#    label_stub = get_config('label_stub'),
#    sort_frequency = FALSE,
#    x_as_group = FALSE,
#    include_total = TRUE,
#    include_cumulative = TRUE,
#    exclude_zero_value = FALSE
#  )

## ---- warning=F---------------------------------------------------------------
library(palmerpenguins)
library(gt)

## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    label_stub = 'Sex'
  )

## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    x_as_group = TRUE, 
    include_total = FALSE
  )

## ---- warning=F---------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = species, 
    label_stub = 'Species', 
    sort_frequency = TRUE, 
    include_cumulative = FALSE
  )

## ---- warning=F---------------------------------------------------------------
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = c('skin_color', 'gender'), 
    label_stub = 'Sex', 
    include_cumulative = FALSE
  )

## ---- eval=F------------------------------------------------------------------
#  generate_crosstab(
#    .data,
#    x,
#    y = NULL,
#    x_group = NULL,
#    y_group = NULL,
#    label_stub = get_config('label_stub'),
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

