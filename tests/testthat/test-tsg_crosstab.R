test_that("cross tabulation returns correct rows", {

  # set.seed(1)
  # df <- data.frame(
  #   category = sample(rep(paste0('Group ', c(1:5)), 10), 50),
  #   with_feature = sample(rep(c(1, 2), 100), 50),
  #   with_extra_feature = sample(rep(c(1, 2), 100), 50)
  # )

  df <- dplyr::starwars

  crosstab <- df |>
    tsg_crosstab(species, sex)

  crosstab_names <- names(crosstab)

  expect_equal(nrow(crosstab), 39)
  expect_equal(ncol(crosstab), 12)

})
