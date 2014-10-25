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
library(plyr)

# Get data
training_data <- read.csv('../raw_data/train.csv')

# <<<<-----------------------------------------< Server >----------------------------------------->>>>
shinyServer(function(input, output) 
{
    
    # Return the requested dataset
    get_data <- reactive({
        switch(input$dataset,
               "gender" = training_data[, c('Sex', 'Survived')],
               "class" = training_data[, c('Pclass', 'Survived')],
               "age" = training_data[, c('Age', 'Survived')],
               "multiple" = training_data[, c('PassengerId', 'Sex', 'Pclass', 'Age', 'Survived')]
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
                    col=c("lightblue3","darkolivegreen4"),
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
                    col=c("lightblue3","darkolivegreen4"),
                    legend = c('Died', 'Survived'),
                    names.arg=c("1st", "2nd", "3rd"))
        }
        else if(columns[1] == 'Age')
        {
            data <- na.omit(data)
            row.names(data)<-NULL
            translation <- data.frame(Values=c(0, 1), Result=c('Died', 'Survived'))
            data <- merge(data, translation, by.x='Survived', by.y='Values')[,-1]
            
            ggplot(data, aes(x=Age, colour=Result, fill=Result)) + 
                geom_density(alpha=.3) +
                ggtitle("Passager Survival Likelihood based Upon Age")
            
        }
        else if(columns[1] == 'PassengerId')
        {
            data <- na.omit(data)
            
            if(input$gender_radio == 'Male')
            {
                data <- subset(data, data$Sex=='male')
            }
            else if(input$gender_radio == 'Female')
            {
                data <- subset(data, data$Sex=='female')
            }
            
            passenger_classes <- c()
            if(input$class_1_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 1)
            }
            
            if(input$class_2_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 2)
            }
            
            if(input$class_3_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 3)
            }
            
            data <- data[data$Pclass %in% passenger_classes,]
            row.names(data)<-NULL
            
            translation <- data.frame(Values=c(0, 1), Result=c('Died', 'Survived'))
            data <- merge(data, translation, by.x='Survived', by.y='Values')[,-1]
            
            ggplot(data, aes(x=Age, colour=Result, fill=Result)) + 
                geom_density(alpha=.3) +
                ggtitle("Passager Survival Likelihood based Upon Age")
        }
    })
    
    # Generate a summary of the data
    output$summary <- renderPrint({
        data <- get_data()
        columns <- names(data)
        
        if(columns[1] == 'Sex')
        {
            'This analysis shows the difference in the survival of passengers from the Titanic based upon their gender.'
        }
        else if(columns[1] == 'Pclass')
        {
            'This analysis shows the difference in the survival of passengers from the Titanic based upon their ticket class.'
        }
        else if(names(data)[1] == 'Age')
        {
            'This analysis shows the difference in the survival of passengers from the Titanic based upon their age.'
        }
        else if(columns[1] == 'PassengerId')
        {
            'This analysis shows the difference in the survival of passengers from the Titanic based upon multiple different parameters.'
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
            
            mean_data <- c(mean(died_data$Age), mean(survived_data$Age))
            median_data <- c(median(died_data$Age), median(survived_data$Age))
            
            df <- data.frame(mean=mean_data, median=median_data)
            rownames(df)[1] <- 'Died'
            rownames(df)[2] <- 'Survived'
                      
            return(df)
        }
        else if(columns[1] == 'PassengerId')
        {
            if(input$gender_radio == 'Male')
            {
                data <- subset(data, data$Sex=='male')
            }
            else if(input$gender_radio == 'Female')
            {
                data <- subset(data, data$Sex=='female')
            }
            
            passenger_classes <- c()
            if(input$class_1_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 1)
            }
            
            if(input$class_2_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 2)
            }
            
            if(input$class_3_checkbox == TRUE)
            {
                passenger_classes <- c(passenger_classes, 3)
            }
            
            data <- data[data$Pclass %in% passenger_classes,]
            data <- na.omit(data)
            row.names(data)<-NULL
            View(data)
            survived_data <- subset(data, data$Survived==1)
            died_data <- subset(data, data$Survived==0)
            
            mean_data <- c(mean(died_data$Age), mean(survived_data$Age))
            median_data <- c(median(died_data$Age), median(survived_data$Age))
            
            df <- data.frame(mean=mean_data, median=median_data)
            rownames(df)[1] <- 'Died'
            rownames(df)[2] <- 'Survived'
            
            return(df)
        }
    })
    
})







