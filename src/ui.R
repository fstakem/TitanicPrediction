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
            h5('Survival factor'),
            selectInput('dataset', label='',
                        choices = c("gender", "class", "age", 'multiple')),
            conditionalPanel(
                condition = "input.dataset == 'multiple'",
                br(),
                h5('Gender'),
                radioButtons('gender_radio', label='', c('All', 'Male', 'Female'), inline = TRUE),
                br(),
                h5('Class'),
                checkboxInput('class_1_checkbox', '1st Class', value = TRUE),
                checkboxInput('class_2_checkbox', '2nd Class', value = TRUE),
                checkboxInput('class_3_checkbox', '3rd Class', value = TRUE)
            )
        ),
    
        # Main
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", verbatimTextOutput("summary")), 
                tabPanel("Table", tableOutput("table")),
                tabPanel("Plot", plotOutput("plot"))
            )
        )
    )
    
))

