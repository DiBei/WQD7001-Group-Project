library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(data.table)
library(e1071)

library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(shinythemes)
library(shinyWidgets)
library(DT)


ui <- fluidPage(
  theme= shinytheme("united"),
  navbarPage(
    " Blood Sugar of Diabetic Detection and Analysis",
    #Tab Page 1
    tabPanel("Introduction",HTML(paste0("This app helps to collect the blood sugar, height, weight, blood press.",
                                 br(),br(),'Instruction:',br(),
                                 "i) From the date we collect we could combine the blood situation and personal detail to analyze the patient status.", br(),
                                 "ii) Provide health diet.",br(),
                                 "iii) Predict the health status.",br(),br(),
                                 'User Guide:',br(),
                                 "i) Input the currenct factors and  click the submit button. The app will show your health status in right table",br(),
                                 "ii) According the data modeling, it will show the accuracy with each modeling ",br(),br(),
                                 'Data Explanation:',br(),
                                 "Pregnancies = Number of pregnancies had happpened",br(),
                                 "Glucose = Glucose level present in the body",br(),
                                 "Bloodpressure = Variable: Low blood pressure = Below 90, Normal blood pressure = 90-140, High blood pressure = Above 140",br(),
                                 "Skinthickness = Thickness of skin",br(),
                                 "Insulin = level of insulin",br(),
                                 "BMI = Body mass index (to understand whether the person is underweight, fit or overweight",br(),
                                 "DiabetesPedigreeFunction = Probability of having diabetes",br(),
                                 "Age = Age of the person",br(),
                                 "Outcome = Variable : 0 = NOT have diabetes, 1 = have diabetes",br(),br(),
                                 
                                 "Causes of diabetes",br(),
                                 "1. Genetic factors: Diabetes has a familial susceptibility to it.",br(),
                                 "2. Obesity: Obesity is an important cause of diabetes.",br(),
                                 "3. Psycho-neural factors: In the process of occurrence and development of diabetes, the important role of psycho-neural factors has been recognized by Chinese and foreign scholars in recent years.",br(),
                                 
                                 br(),"Good Health!",br(),br())),
             fluidPage(br(),br(),
                       "Slide url: ",a("Rpubs", href="https://rpubs.com/yitengzhao/854985"),br(),
                       "Source code: ",a("Github", href="https://github.com/DiBei/WQD7001-Group-Project")
                      
             )),
    # Application title
   
    tabPanel("Data Analysis & Data Explortary",icon = icon("bar-chart-o"),
             fluidRow(
               column(6,plotOutput("scatter2")),
               column(6,plotOutput("scatter1")),
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
             ),
             fluidRow(
               column(6,plotOutput("scatter")),
               column(6,plotOutput("barplot1")),
           
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             ),
             fluidRow(
               column(6,plotOutput("EDA1")),

               tags$h4(" We can see that people who have fewer pregnancies are less likely to develop diabetes."),

               
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             ),
             fluidRow(

               
               column(6,plotOutput("EDA2")), 
               tags$h4(" We can see that people who have fewer pregnancies are less likely to develop diabetes."),
               tags$h4(" We can see that people with Glucose above 140 are more likely to develop diabetes."),
               
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             ),
             fluidRow(
               column(6,plotOutput("EDA3")),
               tags$h4("We can see that people with Blood Pressure above 140 are more likely to develop diabetes."),
         
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             ),
             fluidRow(
              column(6,plotOutput("EDA4")),
               tags$h4(" We can see that people with the BMI above 35 are more likely to develop diabetes."),
               
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             ),
             fluidRow(
               column(6,plotOutput("EDA5")),
               tags$h4(" We can see that diabetes is more closely related to Glucose, Blood Pressure and BMI."),
               
               tags$h4(" Note: 0 = Not Diabetic and 1 = Diabetic")
               
             )
       
    ),
    tabPanel("Prediction",icon = icon("table"),
             headerPanel("Find your Health Report"),
             sidebarPanel(
           
                 numericInput(inputId = "Pregnancies",label = "Number of Pregnancies:",value = 2,min = 0),
                 numericInput(inputId = "Glucose",label = "Glucose level:",value = 25,min=0),
                 numericInput(inputId = "BloodPressure",label = "Bloodpressure level (mm/Hg):",value = 95,min=0),
                 numericInput(inputId = "SkinThickness",label = "Body SkinThickness (mm):",value = 3,min=0),
                 numericInput(inputId = "Insulin",label = "Insulin level:",value = 10,min=0),
                 numericInput(inputId = "BMI",label = "Body Mass Index (weight in kg/(height in m)^2):",value = 25,min=0),
                 sliderInput(inputId = "DiabetesPedigreeFunction",label = "Select your Pedigree:",min=0.00,max=1.00,step = 0.01,value = 0.2),
                 numericInput(inputId = "Age",label = "Enter your Age:",value = 24,min=0),
                 actionButton("refresh",label = "Submit")
               
               
             ),
             
             mainPanel(
               icon = icon("chart-line"),
               fluidRow(
                 valueBoxOutput("Diabetic"),
                 valueBoxOutput("Diabetes_Yes"),
                 valueBoxOutput("Diabetes_No"),
                 DTOutput("ref_table"),
                 tags$h4("Note: This is a sample data set considered for model development")
                 
               )
               
             )
    ),
    tabPanel("Modeling Evaluation",icon = icon("table"),
             fluidRow(
               column(6,DTOutput("metric")),
               column(6,plotOutput("accuracy"))
             ),
             fluidRow(
               column(6,plotOutput("sensitivity")),
               column(6,
                      tags$h4("Note: 
                                        1. Logistics Regression Model 
                                        has considered for making the predictions. 2. Sensitivity, Specificity, F1-Score and Accuracy 
                                        has been considered while selecting the model. 3. Random forest model is performing well."))
             )
    )
  
    
    
    ))


server <- function(input,output){
  df = read.csv("diabetes.csv")
  # data cleaning
  df_1 = df %>% filter(BloodPressure>0)
  df = NULL
  df_1$Outcome = as.factor(df_1$Outcome)
  df_2 = df_1 %>% filter(BMI > 0) %>% filter(Glucose > 0) 
  names(df_2)[names(df_2)=="Outcome"] = "Diabetic"
  #checking corelation
  c = as.data.frame(round(cor(df_2[,-9]),2))
  corr = setDT(c,keep.rownames = TRUE)
  output$corelation = renderTable(corr)
  
  # developing prediction model
  library(caret)
  # shuffling the data
  set.seed(100)
  model_data = sample(nrow(df_2)) # shuffling the rows
  model_df = df_2[model_data,] # creating data with shuffled rows
  
  # splitting data into train and test
  split_rows = createDataPartition(model_df$Diabetic,p=0.7,list = FALSE)
  
  Train = model_df[split_rows,]
  
  # developing logistic regression model
  log_model = train(Diabetic~.,data = Train,method = "glm",family="binomial")
  
  dt_table = eventReactive(input$refresh,{
    data = data.frame(Pregnancies=input$Pregnancies,
                      Glucose=input$Glucose,
                      BloodPressure=input$BloodPressure,
                      SkinThickness=input$SkinThickness,
                      Insulin=input$Insulin,
                      BMI=input$BMI,
                      DiabetesPedigreeFunction=input$DiabetesPedigreeFunction,
                      Age=input$Age)
    
    return(data)
  })
  
  pred_table = eventReactive(input$refresh,{
    diab_pred = predict(log_model,dt_table(),type="prob")
    names(diab_pred) = c("Unhealth","Health")
    diab_pred = diab_pred %>% mutate(Diabetic = ifelse(Unhealth > Health,"Unhealth","Health"),
                                     Unhealth = paste0(round((Unhealth*100),2),"%"),
                                     Health = paste0(round((Health*100),2),"%"))
    
    
    return(diab_pred)
  })
  
  output$Diabetic = renderValueBox({
    valueBox(pred_table()[3],"Is the person Diabetic?",color = "orange",icon = icon("user-plus"))
  })
  
  output$Diabetes_Yes = renderValueBox({
    valueBox(pred_table()[2],"Probability of person is diabetic",color = "yellow",icon = icon("pills"))
  })
  
  output$Diabetes_No = renderValueBox({
    valueBox(pred_table()[1],"Probability of person is not diabetic",color = "green",icon = icon("dumbbell"))
  })
  
  output$plot1 =  renderPlot(barplot(pred_table()[2]))
  
  models_used = data.frame("Model_Name"=c("Logistics Regression","Decision Tree","SVM","Random Forest"),
                           "Accuracy" = c(80.09,72.69,78.24,77.31),
                           "Sensitivity" = c(88.73,88.73,90.14,85.92),
                           "Specificity"= c(63.51,41.89,55.41,60.81),
                           "F1_Score" = c(0.8542,0.8103,0.8449,0.8328))
  
  models_used = models_used %>% mutate(Accuracy = paste0(Accuracy,"%"),
                                       Sensitivity = paste0(Sensitivity,"%"),
                                       Specificity = paste0(Specificity,"%"))
  
  
  
  output$ref_table = renderDT(datatable(df_2))
  output$metric = renderDT(datatable(models_used))
  
  output$accuracy = renderPlot({
    ggplot(models_used,aes(x= Model_Name,y=Accuracy))+
      geom_bar(position = "dodge",stat = "identity",fill = "#58FAF4")+
      geom_text(aes(label=Accuracy),position = position_dodge(0.9),size=6,face="bold",vjust = -1.3)+
      xlab("Model Name")+ylab("Accuracy %")+
      ggtitle("Prediction Accuracy by each model")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
  })
  
  output$sensitivity = renderPlot({
    ggplot(models_used,aes(x= Model_Name,y=Sensitivity))+
      geom_bar(position = "dodge",stat = "identity",fill = "#F7BE81")+
      geom_text(aes(label=Sensitivity),position = position_dodge(0.9),size=6,face="bold",vjust = -1.3)+
      xlab("Model Name")+ylab("Sensitivity %")+
      ggtitle("Prediction Sensitivity by each model")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
  })
  
  output$specificity = renderPlot({
    ggplot(models_used,aes(x= Model_Name,y=Specificity))+
      geom_bar(position = "dodge",stat = "identity",fill = "yellow")+
      geom_text(aes(label=Specificity),position = position_dodge(0.9),size=6,face="bold",vjust = -1.3)+
      xlab("Model Name")+ylab("Specificity %")+
      ggtitle("Prediction Specificity by each model")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
  })
  
  output$scatter2 =  renderPlot({
    ggplot(df_2,aes(x = BloodPressure, y = Insulin,color=Diabetic))+
      geom_point()+
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      ggtitle("Insulin and BloodPressure")+
      # xlab("Diabetic  (0 = No, 1 = Yes)")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
    
    
  })
  
  output$barplot1 =  renderPlot({
    # relation between pregnancies and diabetes
    ggplot(df_2,aes(x= Pregnancies,fill = Diabetic))+
      geom_bar(position = "fill")+
      ggtitle("Pregnancies vs Diabetes")+ylab("Percentage Share")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
    
    
  })
  
  
  output$scatter = renderPlot({
    ggplot(df_2,aes(x=BloodPressure,y=BMI,color=Diabetic))+
      geom_point()+
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      ggtitle("Bloodpressure vs BMI")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
    
  }) 
  
  output$scatter1 = renderPlot({
    ggplot(df_2 %>% filter(SkinThickness>0),aes(x=SkinThickness,y=BloodPressure,color=Diabetic))+
      geom_point()+
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      ggtitle("SkinThickness vs BloodPressure")+
      theme(axis.text=element_text(size=14,face = "bold"),
            axis.title = element_text(size=16,face = "bold"),
            plot.title = element_text(size=18,face = "bold"))
    
  }) 
  
  output$EDA1 = renderPlot({  
    
    ggplot(data = df_2,aes(x = Pregnancies)) +
      geom_histogram(binwidth = 0.5,aes(fill = Diabetic),position = "dodge") +
      ggtitle("Pregnancies Data Distribution") + ylab("Number of people") +
      theme_gray() +
      theme_update(plot.title = element_text(hjust = 0.5))
  
})  
  output$EDA2 = renderPlot({   
  ggplot(data = df_2,aes(x = Diabetic, y = Glucose)) +
    geom_boxplot( aes(fill= Diabetic)) +
    scale_y_continuous(breaks = seq(100,200,10),limits = c(100,200)) +
    ggtitle("Glucose Histogram") +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
                 shape=20, size=5,show.legend = TRUE) +
    theme_gray() + 
    theme_update(plot.title = element_text(hjust = 0.5))

  })    
  output$EDA3 = renderPlot({    
    ggplot(data = df_2,aes(x = Diabetic, y = BloodPressure)) +
      geom_boxplot( aes(fill= Diabetic)) +
      scale_y_continuous(breaks = seq(60,100,10),limits = c(60,100)) +
      ylab("Blood Pressure") +
      ggtitle("Blood Pressure Histogram") +
      stat_summary(fun.y=mean, colour="darkred", geom="point", 
                   shape=20, size=5,show.legend = TRUE) +
      theme_gray() +
      theme_update(plot.title = element_text(hjust = 0.5))
  })  
  output$EDA4 = renderPlot({    
    ggplot(data = df_2,aes(x = Diabetic, y = BMI)) +
      geom_boxplot( aes(fill= Diabetic),outlier.colour = "red", outlier.size = 5) +
      scale_y_continuous(breaks = seq(20,70,5),limits = c(20,70)) +
      ylab("BMI") +
      ggtitle("Body mass index Histogram") +
      stat_summary(fun.y=mean, colour="darkred", geom="point", 
                   shape=20, size=5,show.legend = TRUE) +
      theme_gray() +
      theme_update(plot.title = element_text(hjust = 0.5))
  })  
  output$EDA5 = renderPlot({    
    
    pairs.panels(df_2)
  })  

  # We can see that diabetes is more closely related to Glucose, Blood Pressure and BMI.
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
