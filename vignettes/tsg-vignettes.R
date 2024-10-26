## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup, eval=F------------------------------------------------------------
#  # Install devtools if not yet installed in your machine
#  if(!('devtools' %in% installed.packages()[,'Package'])){
#     install.packages('devtools')
#  }
#  
#  # Install the package from GitHub
#  devtools::install_github('yng-me/tsgx')
#  
#  # Install via R-CRAN
#  install.packages('tsgx')
#  

## -----------------------------------------------------------------------------
library(tsgx)

## ----eval=F-------------------------------------------------------------------
#  generate_frequency(
#    .data,
#    x,
#    label_stub = get_config('label_stub'),
#    sort_frequency = FALSE,
#    include_total = TRUE,
#    include_cumulative = TRUE,
#    exclude_zero_value = FALSE
#  )

## ----warning=F----------------------------------------------------------------
library(palmerpenguins)
library(dplyr)
library(gt)

## ----warning=F----------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = sex, 
    label_stub = 'Sex'
  )

## ----warning=F----------------------------------------------------------------
penguins |> 
  group_by(species) |>
  generate_frequency(
    x = sex, 
    include_total = FALSE
  )

## ----warning=F----------------------------------------------------------------
penguins |> 
  generate_frequency(
    x = species, 
    label_stub = 'Species', 
    sort_frequency = TRUE, 
    include_cumulative = FALSE
  )

## ----warning=F----------------------------------------------------------------
dplyr::starwars |> 
  group_by(skin_color, gender) |> 
  generate_frequency(
    x = sex, 
    label_stub = 'Sex', 
    include_cumulative = FALSE
  )

## ----eval=F-------------------------------------------------------------------
#  generate_crosstab(
#    .data,
#    x,
#    ...,
#    total_by_col = FALSE,
#    total_option = 'default',
#    include_frequency = TRUE,
#    include_proportion = TRUE,
#    include_column_total = TRUE,
#    include_row_total = TRUE,
#    include_subtotal = FALSE,
#    include_zero_value = FALSE,
#    convert_to_percent = TRUE,
#    decimal_precision = NULL,
#    label_stub = get_config('label_stub'),
#    label_total = 'Total',
#    label_subtotal = NULL,
#    weights = NULL,
#    names_separator = '>'
#  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    species, 
    sex
  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    species, 
    sex,
    total_by_col = TRUE
  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    species, 
    sex,
    include_frequency = F
  )

## -----------------------------------------------------------------------------
penguins |> 
  generate_crosstab(
    species, 
    sex,
    include_proportion = F
  )

## -----------------------------------------------------------------------------
penguins |> 
  group_by(island) |>
  generate_crosstab(
    species, 
    sex, 
    include_proportion = F
  )

## -----------------------------------------------------------------------------
penguins |> 
  group_by(island) |>
  generate_crosstab(
    species, 
    sex, 
    convert_to_percent = F,
    include_frequency = F
  )

## ----eval=F-------------------------------------------------------------------
#  generate_multiple_response(
#    .data,
#    x,
#    ...,
#    value_to_count = 1,
#    include_frequency = TRUE,
#    include_proportion = TRUE,
#    convert_to_percent = TRUE,
#    format_precision = 2,
#    label_stub = get_config('label_stub'),
#    label_total = NULL,
#    names_separator = '>',
#    recode = TRUE,
#    recode_variable_position = 5,
#    clean_name = TRUE
#  )

## ----eval=F-------------------------------------------------------------------
#  
#  df <- data.frame(
#    group = c("A1", "A2", "A1", "A1", "A2", "A2"),
#    category = c("G1", "G1", "G2", "G1", "G2", "G1"),
#    response = c("AB", "AC", "B", "ABC", "AB", "C"),
#    A = c(1, 1, 0, 1, 1, 0),
#    B = c(1, 0, 1, 1, 1, 0),
#    C = c(0, 1, 0, 1, 0, 1)
#  )
#  
#  df |>
#    group_by(group) |>
#    generate_multiple_response(category, response)

## ----eval=F-------------------------------------------------------------------
#  df |> generate_multiple_response(category, matches('^[A-C]$'), recode_variable_position = 1)

