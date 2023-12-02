
library(shiny)


choices <- read_csv('choices.csv') %>%
  distinct(Lake_name)

fluidPage(

    titlePanel("Lake Area Prediction"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput('lake', 'Lake', choices = c(sort(choices$Lake_name))),
            dateInput2('pred_date', 
                        'Predict for this month:', 
                        minview = "months",
                        startview = "months",
                        maxview = "year")
        ),

        mainPanel(
            leafletOutput('lake_map'),
            plotOutput('forecast_plot')
        )
    )
)
