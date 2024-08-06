## code to prepare `classification_data_b` dataset goes here

classification_data_b <- class_data_b(
  list(
    c(
      "newc5q63b", "newc5q62b",
      "newc5q52b", "newc5q64b",
      "newc5q61b", "newc5q55b",
      "newc5q47b", "newc5q48b",
      "newc5q50b", "newc5q60b",
      "newc5q57b", "newc5q58b",
      "newc5q49b", "newc5q65b",
      "newc5q59b", "newc5q56b",
      "newc5q44b", "newc5q51b",
      "newc5q43b"),
    wtype = "weights_static"
  )
)

classification_data_b <- classification_data_b %>%
  dplyr::mutate(Question = gsub("new", "", Question),
                category = "Yes",
                y = Total)

usethis::use_data(classification_data_b, overwrite = TRUE)
