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

data<-data%>%select(!OBS.)
# نصب و بارگذاری کتابخانه ها

library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)

library(caTools)
library(rpart)
library(rpart.plot)

library(caret)
library(ROCR)


levels(data$ANSWERED) <- make.names(levels(data$ANSWERED))
levels(data$FEMALE) <- make.names(levels(data$FEMALE))
levels(data$JOB) <- make.names(levels(data$JOB))

levels(data$RENT) <- make.names(levels(data$RENT))
levels(data$OWN_RES) <- make.names(levels(data$OWN_RES))
levels(data$NEW_CAR) <- make.names(levels(data$NEW_CAR))
levels(data$CHK_ACCT) <- make.names(levels(data$CHK_ACCT))
levels(data$SAV_ACCT) <- make.names(levels(data$SAV_ACCT))
levels(data$MOBILE) <- make.names(levels(data$MOBILE))
levels(data$PRODUCT) <- make.names(levels(data$PRODUCT))

data

# جداسازی مدل به تست و ترین
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



# فرض کنید داده‌های شما در data_frame قرار دارد
set.seed(123)  # برای تولید نتایج قابل تکرار

# تنظیمات اعتبارسنجی متقابل
control <- trainControl(method = "cv", number = 3,classProbs = TRUE,summaryFunction = twoClassSummary)

# آموزش مدل درخت تصمیم با اعتبارسنجی متقابل
dt_model <- train( ANSWERED~ .-PRODUCT , data = train, method = "rpart", trControl = control, metric = "ROC")

# مشاهده AUC
print(paste("Mean AUC over", k_folds, "folds:", dt_model$results$ROC))
# نمایش اهمیت ویژگی‌ها
print(importance_df)
print(mean(dt_model$results$ROC))

