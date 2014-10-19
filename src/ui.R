# <<<<--------------------------------------------<>-------------------------------------------->>>>
#
#       ui.R
#       By: Fredrick Stakem
#       Date: 10.18.14
#
#       Purpose: the shiny UI for the data analysis done on the Kaggle data for the Titanic.    
#
# <<<<--------------------------------------------<>-------------------------------------------->>>>


# Libararies
library(shiny)


# <<<<-----------------------------------------< UI >----------------------------------------->>>>
shinyUI(fluidPage(

    # Title
    headerPanel('Titanic Survival Analysis'),
    
    sidebarLayout(
        # Sidebar
        sidebarPanel(
            h4('Parameters'),
            selectInput('dataset', "Survival factor:", 
                        choices = c("gender", "class", "age", 'mulitple')),
            helpText("This data is not done yet!"),
            conditionalPanel(
                condition = "input.dataset == 'mulitple'",
                selectInput("smoothMethod", "Method",
                            list("lm", "glm", "gam", "loess", "rlm"))
            )
        ),
    
        # Main
        mainPanel(
            h4('Analysis'),
            tabsetPanel(
                tabPanel("Summary", verbatimTextOutput("summary")), 
                tabPanel("Table", tableOutput("table")),
                tabPanel("Plot", plotOutput("plot"))
            )
        )
    )
    
))

