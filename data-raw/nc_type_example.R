library(ggautomap)

set.seed(1234)
nc_type_example <- data.frame(
  location = sample(feature_names("sf.nc"), size = 5) |>
    sample(size = 50, replace = TRUE, prob = c(2, 5, 1, 1, 1)),
  type = sample(c("A", "B"), size = 50, replace = TRUE, prob = c(3, 7))
)

usethis::use_data(nc_type_example, overwrite = TRUE)
