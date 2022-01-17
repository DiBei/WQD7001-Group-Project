---
title: "EDA"
author: "WQD7001 Group 3"
date: "2022/1/16"
output: html_document
---

```{r}
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
data <- read.csv("new_diabetes.csv")
data$Outcome <- as.factor(data$Outcome)

# Pregnancies data
ggplot(data = data,aes(x = Pregnancies)) +
  geom_histogram(binwidth = 0.5,aes(fill = Outcome),position = "dodge") +
  ggtitle("Pregnancies Data Distribution") + ylab("Number of people") +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
# We can see that people who have fewer pregnancies are less likely to develop diabetes.

# Glucose data
ggplot(data = data,aes(x = Outcome, y = Glucose)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(100,200,10),limits = c(100,200)) +
  ggtitle("Glucose Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() + 
  theme_update(plot.title = element_text(hjust = 0.5))
# We can see that people with Glucose above 140 are more likely to develop diabetes.

# Blood Pressure data
ggplot(data = data,aes(x = Outcome, y = BloodPressure)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(60,100,10),limits = c(60,100)) +
  ylab("Blood Pressure") +
  ggtitle("Blood Pressure Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
# We can see that people with Blood Pressure above 140 are more likely to develop diabetes.

# Skin Thickness data
ggplot(data = data,aes(x = Outcome, y = SkinThickness)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100)) +
  ylab("Triceps skin fold thickness") +
  ggtitle("Skin Thickness Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))

# Insulin data
ggplot(data = data,aes(x = Outcome, y = Insulin)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100)) +
  ylab("Insulin Level") +
  ggtitle("Insulin Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))

# BMI data
ggplot(data = data,aes(x = Outcome, y = BMI)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(20,70,5),limits = c(20,70)) +
  ylab("BMI") +
  ggtitle("Body mass index Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
# We can see that people with the BMI above 35 are more likely to develop diabetes.

# Diabetes pedigree function
ggplot(data = data,aes(x = Outcome, y = DiabetesPedigreeFunction)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,2,0.2),limits = c(0,2)) +
  ylab("Diabetes Pedigree Function") +
  ggtitle("Diabetes Pedigree Function") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=20, size=5,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))

# Age data
tbl <- with(data, table(Age, Outcome))
barplot(tbl[, 1], col = "royalblue", main = "Patients without diabete by age")

barplot(tbl[, 2], col = "tomato", main = "Patients with diabete by age")
# Young people are less likely to get diabetes.

# Correlation
pairs.panels(data)
# We can see that diabetes is more closely related to Glucose, Blood Pressure and BMI.
```