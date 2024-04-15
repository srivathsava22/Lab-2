library(dplyr)
library(tidyverse)
library(caret)
library(class)

data_oulad <- read.csv("C:\\Users\\Home\\Downloads\\oulad-students.csv")

summary(data_oulad)

data$final_result <- as.factor(data$final_result)
threshold <- 0.5
predicted_classes <- ifelse(test_predictions > threshold, "Pass", "Fail")

target_variable <- data$final_result

data_oulad$disability <- ifelse(data_oulad$disability == "Y", "Y", "N")

#split data
set.seed(123) 
size <- floor(0.70 *  nrow(data_oulad))


ind_train<- sample(seq_len(nrow(data_oulad)), size = size)

train_labels <- data[ind_train, 5]

train <- data[ind_train,1:5]
test <- data[-ind_train,1:5]

#print data
cat("Training data size:", nrow(train), "\n")
cat("Testing data size:", nrow(test), "\n")

#Model Training

formula <- target_variable ~ id_student + age

dim(target_variable)
dim(train)

# Subset the train dataset to match the length of target_variable
train_subset <- subset(train, select = -c(code_module))

#print(target_variable)
model <- lm(target_variable ~., data = train_subset, na.rm = TRUE)
summary(model)  





