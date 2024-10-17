View(lines_data)


data <- lines_data %>%
  dplyr::filter(topic == "lboh"),
                filter == "All LHDs")

View(data)

naccho.viz::create_single_line(
      data = data,
      x_variable = data$year,
      y_variable = data$y,
      select_theme = naccho.viz::line,
      tooltip = ""
)

View(lboh_data)

lboh_data <- return_survey_weighted_counts("lboh") %>%
  dplyr::select(Question, y, category,

  )

