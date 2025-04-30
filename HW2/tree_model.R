# کتابخانه های استفاده شده
library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)

library(caTools)
library(rpart)
library(rpart.plot)

library(caret)
library(ROCR)


# ------------------------------------DataStructure------------------------------------

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

# تبدیل برخی داد های عددی به کتگوریکال


str(data)

summary(data)

#  چک کردن وجود داده های گمشده
colSums(is.na(data))

#داپلیکیت بودن شماره مشاهده
data[table(data$OBS.)>1,]
data%>%filter(OBS.==129203)

# remove first column
data <- data[2:15]

#----------------------decision tree for predict answering----------------------
# رسم مدل قبل از انتخاب ویژگی
# split data set to test and train subsets
set.seed(102)
split <- sample.split(data$ANSWERED, SplitRatio = 0.7)
train <- subset(data, split==T)
test <- subset(data, split==F)

# tunning hyper parameters (cp)
cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree

# decision tree model
# ساختن مدل
tree_model <- rpart(ANSWERED ~ .-PRODUCT, data=train, method = "class", cp= 0.015)
# رسم مدل ساخته شده
rpart.plot(tree_model) # same as prp
prp(tree_model)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree <-predict(tree_model,newdata = train , type = "prob")

#پیش بینی کردن داده های تست با مدل
test_pred_tree <- predict(tree_model, newdata = test, type="prob")



# برای داده آموزشroc رسم نمودار
pred_train <- prediction(train_pred_tree[,2], train$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="green",lwd=3)
  labels(title("ROC"))
# رسم خط تصادفی
abline(a = 0, b = 1, col = "red", lty = 2)

#محاسبه مساحت زیر نمودار
auc_value <- performance(pred_train, "auc")
auc_value <- auc_value@y.values[[1]]
auc_value

#برای داده تستroc رسم نمودار
pred_test <- prediction(test_pred_tree[,2], test$ANSWERED)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, 2),col="orange",add=T,lwd=3)
abline(a = 0, b = 1, col = "red", lty = 2)

#اضافه کردن راهنمای نمودار
legend("bottomright", legend = c("train", "test"), col = c( "green", "orange"), lwd = 2)

# محاسبه مساحت زیر نمودار
auc_value_test <- performance(pred_test, "auc")
auc_value_test <- auc_value_test@y.values[[1]]
auc_value_test



#پیدا کردن بهترین ترشهولد برای مدل 
#استخراج TPR و FPR
tpr <-perf_train@x.values[[1]]
fpr<-perf_train@y.values[[1]]

# محاسبه فاصله از نقطه ایده ال (0,1)
distances <- sqrt((fpr^2) + ((1 - tpr)^2))

# پیدا کردن ایندکس نزدیک ترین نقطه به (0,1)
optimal_index <- which.min(distances)

# optimal_index خواندن ترشهولد مربوط به
perf_train@alpha.values[[1]]


#ارزیابی شاخص های مختلف مدل برای داده ترین
box_ <-table(train$ANSWERED,train_pred_tree[,2]>.6)
tn <-box_[1,1]
fp <-box_[1,2]
fn <-box_[2,1]
tp <-box_[2,2]
sensitivity_ <- tp/(tp+fn)
specificity_ <- tn/(tn+fp)
accuracy_ <- (tp+tn)/(tp+tn+fn+fp)
lift_ <- (tp*(tp+tn+fp+fn))/((tp+fn)*(tp+fp)) 
precision_ <- tp/(tp+fp)
negprecision_ <- tn/(tn+fn)

#ارزیابی شاخص های مختلف مدل برای داده تست
box_ <-table(test$ANSWERED,test_pred_tree[,2]>.6)
tn <-box_[1,1]
fp <-box_[1,2]
fn <-box_[2,1]
tp <-box_[2,2]
sensitivity_1 <- tp/(tp+fn)
specificity_1 <- tn/(tn+fp)
accuracy_1 <- (tp+tn)/(tp+tn+fn+fp)
lift_1 <- (tp*(tp+tn+fp+fn))/((tp+fn)*(tp+fp)) 
precision_1 <- tp/(tp+fp)
negprecision_1 <- tn/(tn+fn)
#رسم جدول مقایسه
cat(" metric      ","   train   ","    test    ",
    "\n sensitivity  :",sensitivity_,"  ",sensitivity_1,
    "\n specificity  :",specificity_,"   ",specificity_1,
          "\n accuracy     :",accuracy_,"   ",accuracy_1,
          "\n lift         :",lift_,"    ",lift_1,
          "\n precision    :",precision_ ,"   ",precision_1,
    "\n negprecision :",negprecision_,"   ",negprecision_1,
    "\n auc_value    :",auc_value,"    ",auc_value_test) 

#----------------------------------------feature selection----------------------
# با استفاده از کراس ولیدیشن و معیار importance 

#تبدیل داده های کتگوریکال که با عدد شروع می شوند به داده قابل استفاده در کراس ولیدیشن
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


#ساخت مدل
tree_model <- rpart(ANSWERED ~ .-PRODUCT, data=train, method = "class", cp= 0.007)


# محاسبه اهمیت ویژگی ها
importance_values <- tree_model$variable.importance


# تبدیل به دیتافریم برای نمایش بهتر
importance_df <- data.frame(Feature = names(importance_values), Importance = importance_values)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# نمایش اهمیت ویژگی ها
print(importance_df)


#استفاده از کراس ولیدیشن برای معیارAUC  
#پس از هر سری اجرای کد  کم اهمیت ترین فیچر انتخاب و از مدل کنار گذاشته می شود و معیار محاسبه میشود.این فرایند تا زمانی که بهتر میشود ادامه دارد

set.seed(123)  # برای تولید نتایج قابل تکرار

# تنظیمات اعتبارسنجی متقابل
control <- trainControl(method = "cv", number = 3,classProbs = TRUE,summaryFunction = twoClassSummary)

# آموزش مدل درخت تصمیم با اعتبارسنجی متقابل
dt_model <- train( ANSWERED~ .-PRODUCT , data = train, method = "rpart", trControl = control, metric = "ROC")

# مشاهده AUC
print(paste("Mean AUC over", k_folds, "folds:", dt_model$results$ROC))
# نمایش اهمیت ویژگی ها
print(importance_df)
#میانگین  AUC
print(mean(dt_model$results$ROC))
#----------decision tree for predict answering after feature selection----------
#حالا که از طریق روش  بالا میدانیم کدام مدل بهتر اس آن را رسم می کنیم

# tunning hyper parameters (cp)
cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT-NEW_CAR, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree

# decision tree model
# ساختن مدل
tree_model <- rpart(ANSWERED ~ .-PRODUCT-NEW_CAR, data=train, method = "class", cp= 0.007)
# رسم مدل ساخته شده
rpart.plot(tree_model) # same as prp
prp(tree_model)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree <-predict(tree_model,newdata = train , type = "prob")

#پیش بینی کردن داده های تست با مدل
test_pred_tree <- predict(tree_model, newdata = test, type="prob")



## برای داده آموزشroc رسم نمودار
pred_train <- prediction(train_pred_tree[,2], train$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="green",lwd=3)
  labels(title("ROC"))
# رسم خط تصادفی
abline(a = 0, b = 1, col = "red", lty = 2)

# محاسبه مساحت زیر نمودار
auc_value <- performance(pred_train, "auc")
auc_value <- auc_value@y.values[[1]]
auc_value

#برای داده تستroc رسم نمودار
pred_test <- prediction(test_pred_tree[,2], test$ANSWERED)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, 2),col="orange",add=T,lwd=3)
abline(a = 0, b = 1, col = "red", lty = 2)

#اضافه کردن راهنمای نمودار
legend("bottomright", legend = c("train", "test"), col = c( "green", "orange"), lwd = 2)

# محاسبه مساحت زیر نمودار
auc_value_test <- performance(pred_test, "auc")
auc_value_test <- auc_value_test@y.values[[1]]
auc_value_test



#پیدا کردن بهترین ترشهولد برای مدل
#استخراج TPR و FPR
tpr <-perf_train@x.values[[1]]
fpr<-perf_train@y.values[[1]]

# محاسبه فاصله از نقطه (0,1)
distances <- sqrt((fpr^2) + ((1 - tpr)^2))

# پیدا کردن ایندکس نزدیکترین نقطه به (0,1)
optimal_index <- which.min(distances)

perf_train@alpha.values[[1]]


#ارزیابی شاخص های مختلف مدل برای داده ترین
box_ <-table(train$ANSWERED,train_pred_tree[,2]>.92)
tn <-box_[1,1]
fp <-box_[1,2]
fn <-box_[2,1]
tp <-box_[2,2]
sensitivity_ <- tp/(tp+fn)
specificity_ <- tn/(tn+fp)
accuracy_ <- (tp+tn)/(tp+tn+fn+fp)
lift_ <- (tp*(tp+tn+fp+fn))/((tp+fn)*(tp+fp)) 
precision_ <- tp/(tp+fp)
negprecision_ <- tn/(tn+fn)

#ارزیابی شاخص های مختلف مدل برای داده تست
box_ <-table(test$ANSWERED,test_pred_tree[,2]>.87)
tn <-box_[1,1]
fp <-box_[1,2]
fn <-box_[2,1]
tp <-box_[2,2]
sensitivity_1 <- tp/(tp+fn)
specificity_1 <- tn/(tn+fp)
accuracy_1 <- (tp+tn)/(tp+tn+fn+fp)
lift_1 <- (tp*(tp+tn+fp+fn))/((tp+fn)*(tp+fp)) 
precision_1 <- tp/(tp+fp)
negprecision_1 <- tn/(tn+fn)

# رسم جدول مقایسه
cat(" metric      ","   train   ","    test    ",
    "\n sensitivity  :",sensitivity_,"  ",sensitivity_1,
    "\n specificity  :",specificity_,"   ",specificity_1,
    "\n accuracy     :",accuracy_,"   ",accuracy_1,
    "\n lift         :",lift_,"    ",lift_1,
    "\n precision    :",precision_ ,"   ",precision_1,
    "\n negprecision :",negprecision_,"   ",negprecision_1,
    "\n auc_value    :",auc_value,"    ",auc_value_test) 
#--------------------------other way for feature selection----------------------
#ROC حذف دو فیچر داشتن خانه و وضعیت حساب جاری مشاهده نمودار  

#پیدا کردن بهترین  cp
cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree$bestTune[[1]]


# ساختن مدل
tree_model <- rpart(ANSWERED ~ .-PRODUCT, data=train, method = "class", cp= 0.008)
# رسم مدل ساخته شده
prp(tree_model)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree <-predict(tree_model,newdata = train , type = "prob")


#roc رسم نمودار
pred_train <- prediction(train_pred_tree[,2], train$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.1, 1.2),col="orange",lwd=2)
abline(a = 0, b = 1, col = "red", lty = 2)

# مدل دوم 
# یدا کردن بهترین  cp
best_model_tree_2 <- train(ANSWERED~.-PRODUCT-OWN_RES, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)
best_model_tree_2

# ساختن مدل
tree_model_2 <- rpart(ANSWERED ~ .-PRODUCT-OWN_RES, data=train, method = "class", cp= 0.001)
# رسم مدل ساخته شده
rpart.plot(tree_model_2) # same as prp
prp(tree_model_2)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree_2 <-predict(tree_model_2,newdata = train , type = "prob")


#roc رسم نمودار
pred_train_2 <- prediction(train_pred_tree_2[,2], train$ANSWERED)
perf_train_2 <- performance(pred_train_2, "tpr", "fpr")
plot(perf_train_2, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="green",lwd=2,add=T)

#مدل سوم 
# یدا کردن بهترین  cp
cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT-CHK_ACCT, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)
best_model_tree


# ساختن مدل
tree_model_3 <- rpart(ANSWERED~.-PRODUCT-CHK_ACCT,, data=train, method = "class", cp= 0.049)

prp(tree_model_3)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree_3 <-predict(tree_model_3,newdata = train , type = "prob")


#roc رسم نمودار
pred_train_3 <- prediction(train_pred_tree_3[,2], train$ANSWERED)
perf_train_3 <- performance(pred_train_3, "tpr", "fpr")
plot(perf_train_3, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -.2),col="yellow",lwd=2,add=T)

# رسم راهنمای کنار نمودار
legend("bottomleft", legend = c("train_all", "train_all_without_OWN_RES","train_all_without_CHK_ACCT"),
       col = c( "orange","green","yellow"),lwd = 2, 
       cex = 0.5, 
       box.lwd = 1)

# رسم نمودارهای مربوط به داده های تست مدل های یک دو وسه

#آموزش داده تست روی مدل اول
test_pred_tree <-predict(tree_model,newdata = test , type = "prob")

#roc رسم نمودار
pred_test <- prediction(test_pred_tree[,2], test$ANSWERED)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.1, 1.2),col="orange",lwd=2,lty=2,)
abline(a = 0, b = 1, col = "red", lty = 2) 

# آموزش داده تست روی مدل دوم
test_pred_tree_2 <-predict(tree_model_2,newdata = test , type = "prob")

#roc رسم نمودار
pred_test_2 <- prediction(test_pred_tree_2[,2], test$ANSWERED)
perf_test_2 <- performance(pred_test_2, "tpr", "fpr")
plot(perf_train_2, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="green",lwd=2,add=T,lty=2)

# اموزش داده تست روی مدل سوم
test_pred_tree_3 <-predict(tree_model_3,newdata = test , type = "prob")

#roc رسم نمودار
pred_test_3 <- prediction(test_pred_tree_3[,2], test$ANSWERED)
perf_test_3 <- performance(pred_test_3, "tpr", "fpr")
plot(perf_train_3, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -.2),col="yellow",lwd=2,add=T,lty=2)

# رسم راهنمایی جدول
legend("bottomleft", legend = c("train_all", "train_all_without_OWN_RES","train_all_without_CHK_ACCT"),
       col = c( "orange","green","yellow"),        lwd = 2, 
       cex = 0.5, 
       box.lwd = 1)

# محاسبه زیر نمودار برای داده تست مدل دوم 
auc_value_test_1 <- performance(pred_train, "auc")
auc_value_test_1 <- auc_value_test_1@y.values[[1]]
auc_value_test_1

#محاسبه زیر نمودار برای داده تست مدل دوم
auc_value_test_2 <- performance(pred_train_2, "auc")
auc_value_test_2 <- auc_value_test_2@y.values[[1]]
auc_value_test
#----------------------decision tree for predict product------------------------
# split data set to test and train subsets
set.seed(10)
split <- sample.split(data$PRODUCT, SplitRatio = 0.7)
train <- subset(data, split==T)
test<- subset(data, split==F)

# tunning hyper parameters (cp)
cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree_2 <- train(PRODUCT~.-ANSWERED, data = train , method = "rpart",
                           trControl = control, tuneGrid = cp_grid)

# ساختن مدل
tree_model_2 <- rpart(PRODUCT~.-ANSWERED, data=train, method = "class", cp= 0.007)
# رسم مدل ساخته شده
rpart.plot(tree_model_2) # same as prp
prp(tree_model_2)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree_2 <-predict(tree_model_2,newdata = train , type = "class")

#پیش بینی کردن داده های تست با مدل
test_pred_tree_2 <- predict(tree_model_2, newdata = test, type="class")



#roc رسم نمودار
pred_train_2 <- prediction(train_pred_tree_2, train$PRODUCT)
perf_train_2 <- performance(pred_train_2, "tpr", "fpr")
plot(perf_train_2, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -.6),col="orange",ldw=2)
abline(a = 0, b = 1, col = "red", lty = 2)

# محاسبه مساحت زیر نمودار
auc_value_2 <- performance(pred_train_2, "auc")
auc_value_2 <- auc_value_2@y.values[[1]]
auc_value_2

#مقایسه مدل با داده واقعی
table(test$PRODUCT,test_pred_tree_2)

