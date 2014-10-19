# <<<<--------------------------------------------<>-------------------------------------------->>>>
#
#       exploratory_research.R
#       By: Fredrick Stakem
#       Date: 10.18.14
#
#       Purpose: exploratory research on the Kaggle data for the Titanic. This file is 
#                meant as a working scratch pad of data analysis work. Only reference
#                this for the purpose of reconstructing my original thought train.
#
# <<<<--------------------------------------------<>-------------------------------------------->>>>


# <<<<-----------------------------------------< Import Data >----------------------------------------->>>>
setwd('~/projects/TitanicPrediction')
training_data <- read.csv('./raw_data/train.csv')

# <<<<-----------------------------------------< Complete Dataset >----------------------------------------->>>>
survived_data <- training_data[, c('Survived')]
mean(survived_data)


# <<<<-----------------------------------------< Gender Factor >----------------------------------------->>>>
gender_data <- training_data[, c('Sex', 'Survived')]

male_data <- gender_data[gender_data$Sex=='male',]
mean(male_data$Survived)

female_data <- gender_data[gender_data$Sex=='female',]
mean(female_data$Survived)

# <<<<-----------------------------------------< Class Factor >----------------------------------------->>>>
class_data <- training_data[, c('Pclass', 'Survived')]

class_1_data <- class_data[class_data$Pclass==1,]
mean(class_1_data$Survived)

class_2_data <- class_data[class_data$Pclass==2,]
mean(class_2_data$Survived)

class_3_data <- class_data[class_data$Pclass==3,]
mean(class_3_data$Survived)

# <<<<-----------------------------------------< Age Factor >----------------------------------------->>>>
age_data <- training_data[, c('Age', 'Survived')]

age_survived_data <- age_data[age_data$Survived==1,]
summary(age_survived_data)

age_died_data <- age_data[age_data$Survived==0,]
summary(age_died_data)


