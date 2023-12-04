
library(shiny)


choices <- read_csv('choices.csv') %>%
  distinct(Lake_name)

fluidPage(

    titlePanel("Lake Area Prediction"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput('lake', 'Lake', 
                           choices = c(sort(choices$Lake_name)),
                           selected = "Great Salt"),
            dateInput('pred_date', 
                      'Predict for this month:', 
                      min = as.Date("2019-01-01"),
                      max = as.Date("2048-12-01"),
                      startview = "year"
            )
        ),

        mainPanel(
            leafletOutput('lake_map'),
            plotOutput('forecast_plot')
        )
    )
)
