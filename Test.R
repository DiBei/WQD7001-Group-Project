#Body Mean blood sugar detection
library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)
library(lubridate)

#load data
testdata <- read_csv("diabetes.csv")
#splitting variables for selection
C_SkinThickness<- sort(unique(unlist(strsplit(as.character(testdata$SkinThickness), ","))))
C_Glucose <- sort(unique(unlist(strsplit(as.character(testdata$Glucose), ","))))
C_BloodPressure <- sort(unique(unlist(strsplit(as.character(testdata$BloodPressure), ","))))

C_Insulin<- sort(unique(unlist(strsplit(as.character(testdata$Insulin), ","))))
C_BMI<- sort(unique(unlist(strsplit(as.character(testdata$BMI), ","))))
C_DiabetesPedigreeFunction<- sort(unique(unlist(strsplit(as.character(testdata$DiabetesPedigreeFunction), ","))))
C_Age<- sort(unique(unlist(strsplit(as.character(testdata$Age), ","))))

c_Outcome<- sort(unique(unlist(strsplit(as.character(testdata$Outcome), ","))))
#create empty matrix to store filtered values



ui <- fluidPage(
    theme= shinytheme("united"),
    navbarPage(
      " Blood Sugar of Diabetic Detection and Analysis",
      #Tab Page 1
      tabPanel("About",HTML(paste0("This app helps to collect the blood sugar, height, weight, blood press.",
                                   br(),br(),'Instruction:',br(),
                                   "i) From the date we collect we could combine the blood situation and personal detail to analyze the patient status.",
                                   br(),"ii) Provide health diet.",br(),
                                   "iii) Predict the health status.",br(),
                                   br(),"Good Health!",br(),br())),
               fluidPage(br(),br(),
                         "Slide url: ",a("Rpubs", href="https://rpubs.com/yitengzhao/854985"),br(),
                         "Source code: ",a("Github", href="https://github.com/DiBei/WQD7001-Group-Project")
               )),
    # Application title
    tabPanel("Find your Health Report",
    headerPanel("Body Mean Blood Sugar Detection"),
    sidebarPanel(
      numericInput('BloodSugarAfterBreakfast', 'BloodSugarAfterBreakfast mmol/L', 50, min = 1, max = 10, step = 1
      ),
      numericInput('BloodSugarAfterLunch', 'BloodSugarAfterLunch mmol/L', 50, min = 1, max = 10, step = 1
      ),
	numericInput('BloodSugarAfterDinner', 'BloodSugarAfterDinner mmol/L', 50, min = 1, max = 10, step = 1
      ),
	submitButton('Submit'),
	    selectizeInput('e1', '1. Select Glucose Level', choices = C_Glucose, multiple = F ),
	    selectizeInput('e2', '2. Select BloodPressure Level', choices = C_BloodPressure, multiple = F),
    	selectizeInput('e3', '3. Select Insulin Level', choices = C_Insulin, multiple = F),
    	selectizeInput('e4', '4. Select SkinThickness Level', choices = C_SkinThickness, multiple = F),
    	selectizeInput('e5', '5. Select BMI Level', choices = C_BMI, multiple = F),
    	selectizeInput('e6', '6. Select DiabetesPedigreeFunction Level', choices = C_DiabetesPedigreeFunction, multiple = F),
	    selectizeInput('e7', '7. Select Age Level', choices = C_Age, multiple = F),
	

    ),
	
    mainPanel(
      h3('Results of Mean Blood Sugar'),
      h4('You entered BloodSugarAfterBreakfast mmol/L'),
      verbatimTextOutput("inputValueBloodSugarAfterBreakfast"),
      h4('You entered BloodSugarAfterLunch mmol/L'),
      verbatimTextOutput("inputValueBloodSugarAfterLunch"),
	    h4('You entered BloodSugarAfterDinner mmol/L'),
      verbatimTextOutput("inputValueBloodSugarAfterDinner"),
      h4('Which resulted in Mean Blood Sugar :'),
      verbatimTextOutput("MeanBloodSugarCalculation"),
      dataTableOutput("mytable")

    )
  )))


Mean <- function(BloodSugarAfterBreakfast, BloodSugarAfterLunch, BloodSugarAfterDinner) (BloodSugarAfterBreakfast+BloodSugarAfterLunch+BloodSugarAfterDinner) / 3

server<- function(input, output) {
    output$inputValueBloodSugarAfterBreakfast <- renderPrint({input$BloodSugarAfterBreakfast})
    output$inputValueBloodSugarAfterLunch <- renderPrint({input$BloodSugarAfterLunch})
    output$inputValueBloodSugarAfterDinner <- renderPrint({input$BloodSugarAfterDinner})
    output$MeanBloodSugarCalculation <- renderPrint({Mean (input$BloodSugarAfterBreakfast,input$BloodSugarAfterLunch,input$BloodSugarAfterDinner)})
   
    output$mytable <- renderDataTable(
      
      {testdata} %>% filter(Glucose == input$e1) 
      %>% filter(BloodPressure == input$e2)  
      %>% filter(Insulin == input$e3)  
      %>% filter(SkinThickness == input$e4)  
      %>% filter(BMI == input$e5)
      %>% filter(DiabetesPedigreeFunction == input$e6)
      %>% filter(Age == input$e7)
    )
     
  }


shinyApp(ui = ui, server = server)
