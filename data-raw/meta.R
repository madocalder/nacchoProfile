## code to prepare `landscape_meta` dataset goes here
meta <- data.frame(
  field = c("totpop", "geography", "c0govcatn", "c2q501", "lboh", "age_cat",
            "race", "tenure", "c4q34", "field", "c4q29"),
  title = c("Percent of U.S. Population Served by LHDs",
            "Geographic Jurisdictions Served by LHDs,",
            "Governance of LHDs",
            "Structure of LHDs",
            "LHDs with a Local Board of Health",
            "Age Category of LHD Top Executive",
            "Racial Identity of LHD Top Executive",
            "Average Length of Tenure of LHD Top Executive",
            "Highest Degree Obtained by LHD Top Executive",
            "Field of Educational Study of LHD Top Executive",
            "Gender of LHD Top Executive"),
  alt = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  caption = "<b> Source: </b> 2022 National Profile of Local Health
          Departments. National Association of County and City Health Officials.
          Washington, DC. (2024).",
  y_label = c("% of Population",
              "% of LHDs",
              "% of LHDs",
              "% of LHDs",
              "% of LHDs",
              "% of Top Executives",
              "% of Top Executives",
              "Number of Years",
              "% of Top Executives",
              "% of Top Executives",
              "% of Top Executives"),
  y_max = c(100, 100, 100, 100, 100, 100, 100, 10, 100, 100, 100),
  y_format = c("{value}%", "{value}%", "{value}%", "{value}%", "{value}%",
               "{value}%", "{value}%", "{value}", "{value}%", "{value}%", "{value}%"),
  data_label = c("{point.y:,.1f}%", "{point.y:,.1f}%", "{point.y:,.1f}%", "{point.y:,.1f}%", "{point.y:,.1f}%",
                 "{point.y:,.1f}%", "{point.y:,.1f}%", "{point.y:,.1f} Years", "{point.y:,.1f}%", "{point.y:,.1f}%", "{point.y:,.1f}%"),
  tooltip = c("<span style=\"color:{point.color}\">\u25CF</span> {point.y}% of the U.S. population is served by {point.category} LHDs",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHDs are {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHDs are {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHDs are {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHDs have the following Local Board of Health: {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHD Top Executives are {point.category} ",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHD Top Executives identify as {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  LHD Top Executives at {point.category}  agencies have been in their position an average of {point.y} years",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHD Top Executives have the following highest degree: {point.category} ",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHD Top Executives have a degree in {point.category}",
              "<span style=\"color:{point.color}\">\u25CF</span>  {point.y}% of LHD Top Executives identify as {point.category} ")

)

usethis::use_data(meta, overwrite = TRUE)
devtools::document()
