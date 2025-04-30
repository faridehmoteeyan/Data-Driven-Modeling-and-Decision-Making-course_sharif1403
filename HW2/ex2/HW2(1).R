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


# لیست متغیرهای کتگوریکال
categorical_vars <- c("ANSWERED", "FEMALE", "JOB", "RENT", "OWN_RES",
                      "NEW_CAR", "CHK_ACCT", "SAV_ACCT", "MOBILE", 
                      "PRODUCT","NUM_DEPENDENTS","NUM_ACCTS")

# ایجاد جداول فراوانی برای هر متغیر کتگوریکال
for (var in categorical_vars) {
  cat("Frequency Table for", var, ":\n") 
  print(table(data[[var]]))  # نمایش جدول فراوانی
  cat("\n")

}

#-----------------------------------------outliers------------------------------

nrow(data)
# انتخاب فقط متغیرهای عددی
numeric_vars <- names(data)[sapply(data, is.numeric)]
numeric_vars <- numeric_vars[numeric_vars != "NUM_DEPENDENTS" & numeric_vars!="NUM_ACCTS"]
numeric_vars <- c("INCOME","AGE","NUM_DEPENDENTS","NUM_ACCTS")

par(mfrow=c(2, 3))  # تنظیم 2 ردیف و 3 ستون برای نمودارها (بسته به تعداد متغیرها تغییر دهید)

# رسم باکس پلات
for (col in numeric_vars) {
  boxplot(data[[col]], main=paste("Boxplot of", col), col="skyblue", border="darkblue")
}
#رسم 

# محاسبه IQR و حذف مقادیر خارج از محدوده
"for (col in numeric_vars) {
  Q1 <- quantile(data[[col]], 0.25)
  Q3 <- quantile(data[[col]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # حذف مقادیر بیرون از محدوده
  data <- data %>% filter((data[[col]] = lower_bound) | (data[[col]] >= upper_bound))
}

nrow(data)"
#----------------------------UniVariate Data Analysis---------------------------

# رسم نمودارها با استفاده از حلقه for
for (var in categorical_vars) {

  # جدول تعداد و درصد
  counts <- data %>%
    group_by(.data[[var]]) %>%
    summarize(Count = n()) %>%
    mutate(Percent = Count / sum(Count) * 100)

  # نمودار بر حسب تعداد
  plot_count <- ggplot(counts, aes_string(x = var, y = "Count", fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes_string(label = "Count"), vjust = -0.5) +
    theme_minimal() +
    labs(title = paste("Bar Chart of", var, "(Count)"), x = var, y = "Count",) +
    theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
  print(plot_count)

  # نمودار بر حسب درصد
  plot_percent <- ggplot(counts, aes_string(x = var, y = "Percent", fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes_string(label = "paste0(round(Percent, 1), '%')"), vjust = -0.5) +
    theme_minimal() +
    labs(title = paste("Bar Chart of", var, "(Percent)"), x = var, y = "Percent (%)") +
    theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
  print(plot_percent)  # نمایش نمودار در بخش Plots
}

#......
median(data$AGE)
freq_table<-table(data$MOBILE,data$PRODUCT) 
prop.table(freq_table, margin = 1)

a <-data %>% filter(NUM_ACCTS == 0)
table(a$SAV_ACCT,a$CHK_ACCT)
table(data$SAV_ACCT,data$CHK_ACCT)
ggplot(data, aes_string(x =NEW_CAR, y = , fill = var)) +
    geom_bar(stat = "identity", color = "black") +

#------------------------unUniVariate Data Analysis-----------------------------
#......................correlation ........
#برای متغییرهای عددی پیوسته
correlation <- cor(data$AGE, data$INCOME, method = "pearson")
print(correlation)

#برای متغییرهای کتگوریکال و شبه کتگوریکال
categorical_vars <- c("ANSWERED", "FEMALE", "JOB", "RENT", "OWN_RES",
                      "NEW_CAR", "CHK_ACCT", "SAV_ACCT", "MOBILE", 
                      "PRODUCT","NUM_DEPENDENTS","NUM_ACCTS")

for (col in categorical_vars) {par(mfrow=c(3, 4))
  for (col2 in categorical_vars){
    if (col != col2){
      cat(col,"VS",col2, ":\n")
      print((table(data[[col]], data[[col2]])))
      cat("\n")
    }
  }
}

#-----------------------------keep better columns-------------------------------
# بررسی متمم بودن دو ستون اجاره دهنده بودن و صاحب خانه بودن
nrow(data %>% filter((RENT=="0") & (OWN_RES=="1")))
nrow(data %>% filter((RENT=="1") & (OWN_RES=="1")))

nrow(data %>% filter((RENT=="0") & (OWN_RES=="1")) %>% filter(INCOME<mean(INCOME)))

# ایا کسانی که ادعای داشتن خانه میکنند دروغ میگویند
nrow(data %>% filter((NEW_CAR=="1") & (OWN_RES=="1"))%>% filter(INCOME<mean(INCOME)))
#----------------------decision tree for predict answering----------------------

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
tree_model <- rpart(ANSWERED ~ .-PRODUCT, data=train, method = "class", cp= 0.008)
# رسم مدل ساخته شده
rpart.plot(tree_model) # same as prp
prp(tree_model)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree <-predict(tree_model,newdata = train , type = "prob")

#پیش بینی کردن داده های تست با مدل
test_pred_tree <- predict(tree_model, newdata = test, type="prob")



#roc رسم نمودار
pred_train <- prediction(train_pred_tree[,2], train$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="orange",ldw=2)
abline(a = 0, b = 1, col = "red", lty = 2)

auc_value <- performance(pred_train, "auc")
auc_value <- auc_value@y.values[[1]]
auc_value
#مشخص کردن ترشهولد مدل
pred_test <- prediction(test_pred_tree[,2], test$ANSWERED)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, 2),col="orange",add=T)
abline(a = 0, b = 1, col = "red", lty = 2)

auc_value_test <- performance(pred_test, "auc")
auc_value_test <- auc_value_test@y.values[[1]]
auc_value_test



#پیدا کردن بهترین ترشهولد برای مدل

#ارزیابی شاخص های مختلف مدل
box_ <-table(test$ANSWERED,pred_tree[,2]>.7)
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
#----------------------decision tree for predict answering----------------------
tree_model$variable.importance
# tunning hyper parameters (cp)


cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree$bestTune[[1]]

# decision tree model
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


 
best_model_tree <- train(ANSWERED~.-PRODUCT-OWN_RES, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)
best_model_tree

# decision tree model
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

# tunning hyper parameters (cp)

cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree <- train(ANSWERED~.-PRODUCT-CHK_ACCT, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree

# decision tree model
# ساختن مدل
tree_model_3 <- rpart(ANSWERED~.-PRODUCT-CHK_ACCT,, data=train, method = "class", cp= 0.049)

prp(tree_model_3)

#پیش بینی کردن داده های ترین با مدل
train_pred_tree_3 <-predict(tree_model_3,newdata = train , type = "prob")


#roc رسم نمودار
pred_train_3 <- prediction(train_pred_tree_3[,2], train$ANSWERED)
perf_train_3 <- performance(pred_train_3, "tpr", "fpr")
plot(perf_train_3, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -.2),col="yellow",lwd=2,add=T)
legend("bottomleft", legend = c("train_all", "train_all_without_OWN_RES","train_all_without_CHK_ACCT"),
       col = c( "orange","green","yellow"),        lwd = 2, 
       cex = 0.5, 

       box.lwd = 1)
# --------------------test--
#پیش بینی کردن داده های ترین با مدل
train_pred_tree <-predict(tree_model,newdata = test , type = "prob")


#roc رسم نمودار
pred_train <- prediction(train_pred_tree[,2], test$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.1, 1.2),col="orange",lwd=2,lty=2,)
abline(a = 0, b = 1, col = "red", lty = 2)  
train_pred_tree_2 <-predict(tree_model_2,newdata = test , type = "prob")


#roc رسم نمودار
pred_train_2 <- prediction(train_pred_tree_2[,2], test$ANSWERED)
perf_train_2 <- performance(pred_train_2, "tpr", "fpr")
plot(perf_train_2, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -2),col="green",lwd=2,add=T,lty=2)

train_pred_tree_3 <-predict(tree_model_3,newdata = test , type = "prob")


#roc رسم نمودار
pred_train_3 <- prediction(train_pred_tree_3[,2], test$ANSWERED)
perf_train_3 <- performance(pred_train_3, "tpr", "fpr")
plot(perf_train_3, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.01, -.2),col="yellow",lwd=2,add=T,lty=2)

legend("bottomleft", legend = c("train_all", "train_all_without_OWN_RES","train_all_without_CHK_ACCT"),
       col = c( "orange","green","yellow"),        lwd = 2, 
       cex = 0.5, 
       box.lwd = 1)
auc_value_test <- performance(pred_train, "auc")
auc_value_test <- auc_value_test@y.values[[1]]
auc_value_test

auc_value_test <- performance(pred_train_2, "auc")
auc_value_test <- auc_value_test@y.values[[1]]
auc_value_test
#----------------------decision tree for predict product------------------------
# split data set to test and train subsets
set.seed(10)
split <- sample.split(data$PRODUCT, SplitRatio = 0.7)
train <- subset(data, split==T)
test<- subset(data, split==F)

# tunning hyper parameters (minbucket and maxdepth)

cp_grid <- expand.grid(cp = seq(0.001,0.1,0.001))
control <- trainControl(method = "cv", number = 10)
best_model_tree_2 <- train(PRODUCT~.-ANSWERED, data = train , method = "rpart",
                         trControl = control, tuneGrid = cp_grid)

best_model_tree

# decision tree model
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

auc_value_2 <- performance(pred_train_2, "auc")
auc_value_2 <- auc_value_2@y.values[[1]]
auc_value_2

table(test$PRODUCT,test_pred_tree_2)
table(test_pred_tree_2)
table(test$PRODUCT)

