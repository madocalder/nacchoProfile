## code to prepare `classification_data` dataset goes here

classification_data <- class_data(c("c5q63a", "c5q62a", "c5q52a", "c5q64a",
             "c5q61a", "c5q55a", "c5q47a", "c5q48a",
             "c5q50a", "c5q60a", "c5q57a", "c5q58a",
             "c5q49a", "c5q65a", "c5q59a", "c5q56a",
             "c5q44a", "c5q51a", "c5q43a"))

usethis::use_data(classification_data, overwrite = TRUE)
