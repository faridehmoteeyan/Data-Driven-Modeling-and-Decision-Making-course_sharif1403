data <- read.csv("AdviseInvestData.csv",stringsAsFactors = T)

# shape of data
dim(data)

head(data)
tail(data)

colnames(data)

# change data structures
str(data)

# به جز ستون های INCOMEو NUM_DEPENDENTSو AGEو NUM_ACCTS 
#Except for columns INCOME, NUM_DEPENDENTS, AGE and  NUM_ACCTS, which are numerical, the rest of the columns are categorical.
data$JOB <- as.factor(data$JOB)
data$CHK_ACCT <- as.factor(data$CHK_ACCT)
data$SAV_ACCT <- as.factor(data$SAV_ACCT)
data$PRODUCT <- as.factor(data$PRODUCT)
data$ANSWERED <- as.factor(data$ANSWERED)
data$FEMALE <- as.factor(data$FEMALE)
data$RENT <- as.factor(data$RENT)
data$OWN_RES <- as.factor(data$OWN_RES)
data$NEW_CAR <- as.factor(data$NEW_CAR)
data$MOBILE <- as.factor(data$MOBILE)
library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)

library(caret)
library(caTools)
library(rpart)
library(rpart.plot)

library(caret)
library(ROCR)


set.seed(102)
split <- sample.split(data$ANSWERED, SplitRatio = 0.7)
train <- subset(data, split==T)
test <- subset(data, split==F)

tree_model <- rpart(ANSWERED ~ .-PRODUCT, data=train, method = "class", cp= 0.007)


# محاسبه اهمیت ویژگی‌ها
importance_values <- tree_model$variable.importance


# تبدیل به دیتافریم برای نمایش بهتر
importance_df <- data.frame(Feature = names(importance_values), Importance = importance_values)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# نمایش اهمیت ویژگی‌ها
print(importance_df)







# تعداد تکرارهای اعتبارسنجی متقابل
k_folds <- 5

# ایجاد یک لیست برای ذخیره AUC
auc_values <- c()

# انجام اعتبارسنجی متقابل
for (i in 1:k_folds) {
  # تقسیم داده‌ها به آموزش و آزمون
  train_index <- sample.split(data$ANSWERED, SplitRatio = .8)
  train <- subset(data, split==T)
  test <- subset(data, split==F)
  
  # آموزش مدل درخت تصمیم
  dt_model <- rpart( ANSWERED~.-PRODUCT-NEW_CAR-NUM_DEPENDENTS-RENT-OWN_RES, data = train, method = "class")
  
  # پیش‌بینی بر روی داده‌های آزمون
  pred_test <- predict(dt_model, test, type = "prob")[, 2]  # احتمال کلاس مثبت
  
  # محاسبه AUC
  pred_test <- prediction(pred_test, test$ANSWERED)
  auc_value_test <- performance(pred_test, "auc")
  auc_value_test <- auc_value_test@y.values[[1]]
  auc_value_test
  
  # ذخیره AUC
  auc_values <- c(auc_values, auc_value_test)
  print(auc_value)
}

# محاسبه میانگین AUC
mean_auc <- mean(auc_values)
print(importance_df)
print(paste("Mean AUC over", k_folds, "folds:", mean_auc))

