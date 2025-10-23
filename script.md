```r
ibrary(rcdf)
library(tsg)
library(dplyr)

# Read the environment variables from the .env file
# This file should contain the PRIVATE_KEY_PW variable
env <- read_env()

# Extract the password from `env` object
password <- env$PRIVATE_KEY_PW

# Specify the decryption key (private key in PEM format)
decryption_key <- "data/sample-rcdf/RSA Keys/1001319-private-key.pem"

data <- read_rcdf(
  path = "data/sample-rcdf/1001319 Sumilao.rcdf",
  decryption_key = decryption_key,
  password = password,
  return_meta = TRUE # include metadata
)


# Get the area names from the RCDF metadata
area_names <- get_rcdf_metadata(
  path = "data/sample-rcdf/1001319 Sumilao.rcdf",
  key = "area_names"
)

data_dictionary <- get_rcdf_metadata(
  path = "data/sample-rcdf/1001319 Sumilao.rcdf",
  key = "dictionary"
)

create_area_code <- function(.data){
  .data |>
    mutate(
      area_code = paste0(
        region_code,
        province_code,
        city_mun_code,
        barangay_code
      )
    )
}


person_record <- data$cbms_person_record |>
  collect() |>
  create_area_code() |>
  inner_join(area_names, by = 'area_code')


household <- data$cbms_household_record |>
  collect() |>
  create_area_code() |>
  inner_join(area_names, by = 'area_code')


ts <- list()

# Generate the frequency table for Marital Status
ts$marital_status <- person_record |>
  group_by(
    area_code,
    area_name
    ) |>
  generate_frequency(
    a07_marital_status,
    # add_total = FALSE
    )


# Generate the frequency table for Highest Grade Completed
ts$hgc <- person_record |>
  group_by(
    area_code,
    area_name
    ) |>
  generate_frequency(a11_hgc)

# Generate the frequency table for Highest Grade Completed (Grouped)
ts$hgc_group <- person_record |>
  group_by(area_code, area_name) |>
  generate_frequency(a11_hgc_group) |>
  add_table_title('HGC Group')


ts$employment_status <- person_record |>
  filter(e01_labor_force_participation == 1, a05_age >= 15) |>
  group_by(
    area_code,
    area_name
  ) |>
  generate_frequency(
    e01_employment_status,
    add_total = FALSE,
    # group_as_list = TRUE
  ) |>
  add_table_title('Employment Status')


# ts$employment_status <- person_record |>
#   filter(e01_labor_force_participation == 1, a05_age >= 15) |>
#   group_by(
#     area_code,
#     area_name
#   ) |>
#   generate_frequency(
#     e01_employment_status,
#     add_total = FALSE,
#     # group_as_list = TRUE
#   ) |> tidyr::pivot_wider(
#   names_from = category,
#   values_from = c(frequency, percent)
# )

ts$building_type <- household |>
  group_by(area_code, area_name) |>
  generate_frequency(
    o01_building_type,
    add_total = FALSE,
    # collapse_list = TRUE,
    group_as_list = TRUE
  ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('Building Type')


# Generate the frequency table for the Food Security Section
ts$household_conviences <- household |>
  group_by(area_code, area_name) |>
  generate_frequency(
    starts_with('o14_'),
    add_total = FALSE,
    collapse_list = TRUE,
    # group_as_list = TRUE
  ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name')


# Generate the frequency table for the Formal Financial Account Section
ts$financial_acc <- household |>
  group_by(area_code, area_name) |>
  generate_frequency(
    starts_with('i0'),
    collapse_list = TRUE,
    add_total = TRUE
  ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('Formal Financial Account')




#Generate a Cross-Tabulation tables

#Generate a cross-tabulation of age group by sex
ts$age_group_by_sex <- person_record |>
  group_by(area_code, area_name) |>
  generate_crosstab(
    a05_age_group_five_years,
    a03_sex) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('Age group by Sex')


ts$single_age_by_marital_status <- person_record |>
  group_by(area_code, area_name) |>
  generate_crosstab(
    a05_age,
    a07_marital_status
  ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('Single-age group by Marital Status')


ts$hgc_group_by_hgc <- person_record |>
  filter(e01_labor_force_participation == 1, a05_age >= 15) |>
  group_by(area_code, area_name) |>
  tsg::generate_crosstab(
   a11_hgc_group,
   a11_hgc
    ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('HGC group by HGC')



ts$building_type_by_main_source_water <- household |>
  group_by(area_code, area_name) |>
  tsg::generate_crosstab(
    o01_building_type,
    n01_main_water,
    add_total_row = FALSE
  ) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('Building Type by Main source of water')



#Some SDG indicators
ts$sdg_employment_status_by_sex <- person_record |>
  group_by(area_code, area_name) |>
  filter(
    e01_labor_force_participation == 1,
    a05_age >= 15) |>
  generate_crosstab(
    a03_sex,
    e01_employment_status) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name'
    ) |>
  add_table_title('Employment status by sex')


ts$neet_rate_sex <- person_record |>
  group_by(area_code, area_name) |>
  filter(a05_age >= 15 & a05_age <= 24) |>
  mutate(
    neet = if_else(
        d01_currently_attending_school == 2 &
        e01_employment_status == 2 &
        d08_tvet_currently_attending == 2,
          'Not in education, employment or training',
          "In education, employment or training")) |>
  generate_crosstab(a03_sex, neet) |>
  add_table_title('Youth engagement by sex')


ts$sdg_managerial_position_by_sex <- person_record |>
  filter(a05_age >= 15,
         e01_employment_status == 1,
         e05_occupation_group == 1) |>
  generate_crosstab(
    e05_occupation_group,
    a03_sex) |>
  rename_label(
    area_code = 'Area code',
    area_name = 'Area name') |>
  add_table_title('In Managerial Position by sex')




ts$sdg_access_to_electricity <- household|>
  filter(o01_building_type %in% c(1:7, 9)) |>
  generate_crosstab(area_name, o11_electricity) |>
  rename_label(area_name = 'Area name') |>
  add_table_title('Distribution of Households with Electricity')



ts$sdg_drinking_water <- household |>
  mutate(
    n02_improved_drinking_water = if_else(
      n02_drinking_water %in% c(11:14, 21, 31, 41, 51, 61, 71) |
        (n02_drinking_water %in% c(72, 91, 92) & n03_water_for_other_purposes %in% c(11:14, 21, 31, 41, 51, 61, 71, 72, 91, 92)),
      1L,
      2L
    ),
    n05_time_to_obtain_drinking_water = case_when(
      n04_water_source_location %in% c(1, 2) |
        n03_water_for_other_purposes %in% c(11, 12, 61, 71) |
        n02_drinking_water %in% c(11, 12, 61, 71) |
        is.na(as.integer(n05_time_to_collect_water)) ~ 1L,  # Water on premises,
      n04_water_source_location == 3 & as.integer(n05_time_to_collect_water) == 998 ~ 8L, # Not reported,
      n04_water_source_location == 3 & as.integer(n05_time_to_collect_water) >= 30 ~ 4L, # 30 minutes or more,
      n04_water_source_location == 3 & as.integer(n05_time_to_collect_water) > 0 ~ 3L, # Up to 30 minutes,
      n04_water_source_location == 3 & as.integer(n05_time_to_collect_water) == 0 ~ 2L # Non-member of the household who collected water,
    ),
    n03_service_level_drinking_water = case_when(
      n02_drinking_water %in% c(81, 99) ~ 4L, #Surface Water
      n02_drinking_water %in% c(32, 42) | n02_improved_drinking_water == 2 ~ 3L, # Unimproved
      n02_improved_drinking_water == 1 & n05_time_to_obtain_drinking_water %in% 1:3 ~ 1L, #'Basic'
      n02_improved_drinking_water == 1 & n05_time_to_obtain_drinking_water == 4 ~ 2L # Limited
    )
  ) |>
  generate_crosstab(area_code, n03_service_level_drinking_water)



ts$sdg_drinking_water <- household |>
  generate_crosstab(area_name, n03_service_level_drinking_water) |>
  add_table_title('Distribution of households by service level of drinking water')




ts$mpi_overcrowding_status <- household |>
  mutate(
    floor_area_per_capita = o07_floor_area / hh_size) |>
  mutate(
    overcrowding_status = dplyr::case_when(
      floor_area_per_capita < 4 ~ 1L, #"Overcrowded", # Floor area per capita less than 4 indicates overcrowding.
      floor_area_per_capita >= 4 ~ 2L  #"Not overcrowded" # Floor area per capita greater than or equal to 4 indicates no overcrowding.
    )
  ) |>
  generate_crosstab(area_name, overcrowding_status) |>
  add_table_title('Distribution of households by overcrowding status')



ts$mpi_fies_households <- household |>
  mutate(across(matches('^g0[1-8]'), ~ if_else(. == 1, 1L, 0L))) |>
  mutate(
    # Classify as 'not_secured' if they experienced 4 or more.
    food_insecure = if_else((
        g01_worried +
        g02_not_eaten_healthy +
        g03_ate_few_food +
        g04_skipped_meal +
        g05_ate_less +
        g06_ran_out_of_food +
        g07_hungry +
        g08_not_eaten_whole_day
    ) >= 4,
    1L, # If the raw score is greater than or equal to 4, they experience food insecurity
    2L, # else they do not experience food insecurity
    ))|>
  generate_crosstab(area_name, food_insecure) |>
  add_table_title('Food Insecurity')




ts$fies_individuals <- household |>
  mutate(across(matches('^g0[1-8]'), ~ if_else(. == 1, 1L, 0L))) |>
  mutate(
    # Classify as 'not_secured' if they experienced 4 or more.
    food_insecure = if_else((
      g01_worried +
        g02_not_eaten_healthy +
        g03_ate_few_food +
        g04_skipped_meal +
        g05_ate_less +
        g06_ran_out_of_food +
        g07_hungry +
        g08_not_eaten_whole_day
    ) >= 4,
    1L,#'Food Insecure' # If the raw score is greater than or equal to 4, they experience food insecurity
    2L, # 'Food Secure' # else they do not experience food insecurity
    ))|>
  select(uuid, area_code, area_name, food_insecure) |>
  inner_join(select(person_record, uuid), by = 'uuid' ) |>
  generate_crosstab(area_name, food_insecure) |>
  add_table_title('Food Insecurity')


ts$internet_access_individuals <- household |>
  select(uuid, area_code, area_name, k01_internet_access) |>
  inner_join(select(person_record, uuid), by = 'uuid' ) |>
  generate_crosstab(area_name, k01_internet_access) |>
  add_table_title('Total Individuals by Internet Access')

```
