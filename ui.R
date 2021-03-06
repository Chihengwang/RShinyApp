#===========================================================================
# Library
#===========================================================================
library(shiny)
library(dplyr)
library(markdown)
library(data.table)
#===========================================================================
# Data Prepare for selectInput,sliderInput,numericInput
#===========================================================================
path = getwd()
#---- Load Data ----
Azure_ML_train <- fread(file.path(path, "Azure_ML_train.csv")) %>% select(-Survived)
Azure_ML_test <- fread(file.path(path, "Azure_ML_test.csv"))

#---- Bind Data ----
Azure_ML_data <- rbind(Azure_ML_train,Azure_ML_test)
rm(Azure_ML_train,Azure_ML_test)
#---- selectInput for classfication variable ----
Pclass <- sort(unique(Azure_ML_data$PassengerClass))
gender <- sort(unique(Azure_ML_data$Gender))
Embarked <- sort(unique(Azure_ML_data$PortEmbarkation[Azure_ML_data$PortEmbarkation != ""]))

#Azure_ML_data %>% select(PortEmbarkation) %>% distinct(PortEmbarkation) %>% arrange(PortEmbarkation)

#---- sliderInput ----
Fare <- data.frame( max = max(Azure_ML_data$FarePrice ,na.rm =TRUE), 
                    min = min(Azure_ML_data$FarePrice ,na.rm =TRUE) 
)

#---- numericInput ----
age <- list( max = floor(max(Azure_ML_data$Age ,na.rm =TRUE)), 
             min = floor(min(Azure_ML_data$Age ,na.rm =TRUE))) 
SibSp <- list( max = max(as.numeric(Azure_ML_data$SiblingSpouse),na.rm =TRUE), 
               min = min(as.numeric(Azure_ML_data$SiblingSpouse),na.rm =TRUE)) 
Parch <- list( max = max(as.numeric(Azure_ML_data$ParentChild),na.rm =TRUE), 
               min = min(as.numeric(Azure_ML_data$ParentChild),na.rm =TRUE)
)

#===========================================================================
# Shiny Layout
#===========================================================================
shinyUI(fluidPage( 
  
  titlePanel("Titanic Survival Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("PassengerClass", "PassengerClass : ", choices=Pclass),
      selectInput("Gender", "Gender : ", choices=gender),
      numericInput("Age", "Age : ", min = age$min, max = age$max, value =age$max, step = 0.5)
    ),
    mainPanel( imageOutput("result_plot") )
  )
  
))
