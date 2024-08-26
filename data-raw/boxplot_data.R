## code to prepare `boxplot_data` dataset goes here

devtools::load_all()
start_time <- Sys.time() # get start time


boxplot_args <- list(
  args = c("c3q15", "totexp_percap", "non_weighted_revenue_total", "non_weighted_revenue_percap"),
  filters = c("urbanrural2022", "c0govcat", "c0popcat3", "censusregion")
)

c(boxplot_args[[1]][[2]],boxplot_args[[2]][[1]])

boxplot_list <- list() # create an empty list

# loop used to shorten code and create the dataframes with each filter
for (i in 1:length(boxplot_args[[1]])) {
  for (j in 1:length(boxplot_args[[2]])) {

    arg1 <- boxplot_args[[1]][[i]]
    arg2 <- boxplot_args[[2]][[j]]

    boxplot_list[[paste0("boxplot_data_", boxplot_args[[1]][[i]], "_", boxplot_args[[2]][[j]])]] <- boxplot_data(arg1, arg2)
    boxplot_list[[paste0("boxplot_data_", boxplot_args[[1]][[i]], "_all")]] <- boxplot_data(arg1)

  }
}

# remove names
for (i in seq_along(boxplot_list)) {
  names(boxplot_list[[i]]) <- NULL
}


# print elapsed time
end_time <- Sys.time() - start_time # calculate difference
print(end_time) # 40 seconds



usethis::use_data(boxplot_list, overwrite = TRUE)


