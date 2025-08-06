# -------------------------------------------------------
# Project: Employee Attrition Analysis
# Purpose: Explore and model employee attrition patterns
# Author: Lucas Valpreda
# Date: 04/08/2025
# -------------------------------------------------------
# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(readr)
# 1. Getting to Know the Data
df <- read_csv("Termination_Data.csv")
head(df)
str(df)
summary(df)
names(df)

# 2. Attrition status breakdown
table(df$STATUS)
round(prop.table(table(df$STATUS)) * 100, 1)
library(ggplot2)
ggplot(df, aes(x = STATUS, fill = STATUS)) +
  geom_bar() +
  labs(title = "Employee Status Distribution", x = "Status", y = "Count") +
  theme_minimal()
ggplot(df, aes(x = gender_full, fill = STATUS)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition by Gender", x = "Gender", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
ggplot(df, aes(x = department_name, fill = STATUS)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Attrition by Department", x = "Department", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
df$age_group <- cut(df$age,
                    breaks = c(0, 25, 35, 45, 55, 65),
                    labels = c("Under 25", "25–34", "35–44", "45–54", "55+"),
                    right = FALSE)

ggplot(df, aes(x = age_group, fill = STATUS)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition by Age Group", x = "Age Group", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
ggplot(df, aes(x = length_of_service, fill = STATUS)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(title = "Attrition by Length of Service", x = "Years of Service", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
ggplot(df, aes(x = length_of_service, fill = STATUS)) +
  geom_histogram(binwidth = 1, position = "fill") +
  facet_wrap(~ gender_full) +
  labs(title = "Attrition by Length of Service and Gender", 
       x = "Years of Service", 
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
df$STATUS_BINARY <- ifelse(df$STATUS == "TERMINATED", 1, 0)
model <- glm(STATUS_BINARY ~ gender_full + length_of_service + department_name, 
             data = df, 
             family = binomial)
summary(model)
exp(coef(model))
# Get predicted probabilities
df$predicted_prob <- predict(model, type = "response")
library(caret)
df$predicted_prob <- predict(model, type = "response")
df$predicted_class <- ifelse(df$predicted_prob >= 0.5, 1, 0)
df$actual <- as.factor(df$STATUS_BINARY)
df$predicted <- as.factor(df$predicted_class)
confusionMatrix(data = df$predicted, reference = df$actual, positive = "1")
df$STATUS_BINARY <- as.factor(df$STATUS_BINARY)
rf_df <- df[, c("STATUS_BINARY", "gender_full", "length_of_service", "department_name")]
set.seed(123)

# Analysis still underway
