# <<<<--------------------------------------------<>-------------------------------------------->>>>
#
#       server.R
#       By: Fredrick Stakem
#       Date: 10.18.14
#
#       Purpose: the shiny server for the data analysis done on the Kaggle data for the Titanic.    
#
# <<<<--------------------------------------------<>-------------------------------------------->>>>


# Libararies
library(shiny)
library(reshape2)
library(ggplot2)

# Get data
training_data <- read.csv('../raw_data/train.csv')


# <<<<-----------------------------------------< Server >----------------------------------------->>>>
shinyServer(function(input, output) {
    
    # Return the requested dataset
    get_data <- reactive({
        switch(input$dataset,
               "gender" = training_data[, c('Sex', 'Survived')],
               "class" = training_data[, c('Pclass', 'Survived')],
               "age" = training_data[, c('Age', 'Survived')],
               "multiple" = training_data[, c('PassengerId', 'Sex', 'Pclass', 'Age')]
        )
    })
    
    # Generate plot of data
    output$plot <- renderPlot({
        data <- get_data()
        columns <- names(data)
        
        if(columns[1] == 'Sex')
        {
            gender_data <- table(data$Survived, data$Sex)
        
            barplot(gender_data, 
                    ylab="People",
                    xlab="Gender", 
                    beside=TRUE,
                    col=c("darkblue","red"),
                    legend = c('Died', 'Survived'),
                    names.arg=c("Female", "Male"))
        }
        else if(columns[1] == 'Pclass')
        {
            survival_data <- table(data$Survived, data$Pclass)
            
            barplot(survival_data, 
                    ylab="People",
                    xlab="Class", 
                    beside=TRUE,
                    col=c("darkblue","red"),
                    legend = c('Died', 'Survived'),
                    names.arg=c("1st", "2nd", "3rd"))
        }
        else if(columns[1] == 'Age')
        {
            data <- na.omit(data)
            row.names(data)<-NULL
            survived_data <- subset(data, data$Survived==1)
            died_data <- subset(data, data$Survived==0)
            
            ggplot() + 
                geom_density(alpha=.2, fill="#FF6666",aes(x=Age), colour="red", data=survived_data) + 
                geom_density(alpha=.2, fill="blue",aes(x=Age), colour="blue", data=died_data)
        }
        else if(columns[1] == 'PassengerId')
        {
            
        }
    })
    
    # Generate a summary of the data
    output$summary <- renderPrint({
        data <- get_data()
        print(nrow(data))
        columns <- names(data)
        
        if(columns[1] == 'Sex')
        {
            
        }
        else if(columns[1] == 'Pclass')
        {
            
        }
        else if(names(data)[1] == 'Age')
        {
            
        }
        else if(columns[1] == 'PassengerId')
        {
            
        }
    })
    
    # Generate an HTML table view of the data
    output$table <- renderTable({
        data <- get_data()
        columns <- names(data)
        
        if(columns[1] == 'Sex')
        {
            gender_data <- table(data$Survived, data$Sex)
            rownames(gender_data)[1] <- 'Died'
            rownames(gender_data)[2] <- 'Survived'
            
            return(gender_data)
        }
        else if(columns[1] == 'Pclass')
        {
            survival_data <- table(data$Survived, data$Pclass)
            rownames(survival_data)[1] <- 'Died'
            rownames(survival_data)[2] <- 'Survived'
            
            colnames(survival_data)[1] <- '1st Class'
            colnames(survival_data)[2] <- '2nd Class'
            colnames(survival_data)[3] <- '3rd Class'
            
            return(survival_data)
        }
        else if(columns[1] == 'Age')
        {
            data <- na.omit(data)
            row.names(data)<-NULL
            survived_data <- subset(data, data$Survived==1)
            died_data <- subset(data, data$Survived==0)
            
            
                      
            return(table(x))
        }
        else if(columns[1] == 'PassengerId')
        {
            
        }
    })
    
})







