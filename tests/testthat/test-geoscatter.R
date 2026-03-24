test_that("geoscatter is not silently duplicated", {
  library(ggplot2)
  library(nswgeo)

  plot <-
    covid_cases_nsw |>
    dplyr::filter(lga == "Walgett") |>
    ggplot(aes(location = lga)) +
    # geom_boundaries(feature_type = "nswgeo.lga") +
    geom_geoscatter(aes(colour = type), sample_type = "random", size = 0.5) +
    coord_automap(
      feature_type = "nswgeo.lga",
      xlim = c(147, 153),
      ylim = c(-33.7, -29)
    ) +
    guides(colour = guide_legend(override.aes = list(size = 1))) +
    theme_void()

  # The dataset contains 5 points in Walgett. We expect 2 plot layers -
  # one for the base containing a points grob with 5 points, and one
  # for the inset which is a nullGrob since the inset was not configured.
  expect_length(plot@layers, 2)
  expect_length(ggplot2::get_layer_grob(plot, 1)[[1]]$x, 5)
  expect_s3_class(ggplot2::get_layer_grob(plot, 2)[[1]], "null")
})
