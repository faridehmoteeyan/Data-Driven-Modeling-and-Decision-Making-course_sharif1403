# کتابخانه های استفاده شده
library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)
library(caTools)
library(ROCR)
# ------------------------------------DataStructure------------------------------------

data <- read.csv("C:/Users/amirg/OneDrive/Desktop/ترم یک ارشد/مدلسازی داده محور/taklif/HW2/AdviseInvestData.csv",stringsAsFactors = T)

dim(data)

head(data)
tail(data)

colnames(data)

str(data)

data$OBS. <- as.factor(data$OBS.)
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

str(data)

summary(data)

colSums(is.na(data))

# لیست متغیرهای کتگوریکال
categorical_vars <- c("ANSWERED", "FEMALE", "JOB", "RENT", "OWN_RES",
                      "NEW_CAR", "CHK_ACCT", "SAV_ACCT", "MOBILE", "PRODUCT")

# ایجاد جداول فراوانی برای هر متغیر کتگوریکال
for (var in categorical_vars) {
  cat("Frequency Table for", var, ":\n")
  print(table(data[[var]]))  # نمایش جدول فراوانی
  cat("\n")

}

#-----------------------------------------outliers------------------------------
# 
# nrow(data)
# انتخاب فقط متغیرهای عددی
numeric_vars <- names(data)[sapply(data, is.numeric)]
numeric_vars2 <- numeric_vars[numeric_vars != "NUM_DEPENDENTS" & numeric_vars!="NUM_ACCTS"]

par(mfrow=c(2, 3))  # تنظیم 2 ردیف و 3 ستون برای نمودارها (بسته به تعداد متغیرها تغییر دهید)

# رسم باکس پلات
for (col in numeric_vars) {
  boxplot(data[[col]], main=paste("Boxplot of", col), col="skyblue", border="darkblue")
}
# 
# # محاسبه IQR و حذف مقادیر خارج از محدوده
# for (col in numeric_vars2) {
#   Q1 <- quantile(data[[col]], 0.25)
#   Q3 <- quantile(data[[col]], 0.75)
#   IQR <- Q3 - Q1
#   lower_bound <- Q1 - 1.5 * IQR
#   upper_bound <- Q3 + 1.5 * IQR
# 
#   # حذف مقادیر بیرون از محدوده
#   data <- data %>% filter(data[[col]] >= lower_bound & data[[col]] <= upper_bound)
# }
# nrow(data)
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
  print(plot_percent)
}
#-------------------------------multivariate Analysis---------------------------
numeric_data <- data[, numeric_vars]

# رسم ماتریس همبستگی
ggpairs(numeric_data, 
        upper = list(continuous = "cor"),    # همبستگی در بالای قطر
        lower = list(continuous = "smooth"),# نمودار پراکندگی در پایین قطر
        diag = list(continuous = "densityDiag"))# نمودار تراکم روی قطر






#-----------------------------logistic model------------------------------------
data <- data[, 2:ncol(data)]
set.seed(102)
split <- sample.split(data$ANSWERED, SplitRatio = 0.70)
train <- subset(data, split==T)
test <- subset(data, split==F)



#-----------------------backward elimination (AIC)------------------------------

# ساخت مدل اولیه لجستیک رگرسیون
full_model <- glm(ANSWERED ~ . - PRODUCT, data = train, family = "binomial")

# اجرای backward elimination با استفاده از معیار AIC
backward_model <- step(full_model, direction = "backward")

# نمایش خلاصه مدل نهایی
summary(backward_model)





#-----------------------backward elimination (P-value)--------------------------

fit0 <- glm(ANSWERED ~ .- PRODUCT , data = train, family = "binomial")
summary(fit0)

fit1 <- glm(ANSWERED ~ . - PRODUCT - JOB , data = train, family = "binomial")
summary(fit1)

fit2 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS , data = train, family = "binomial")
summary(fit2)

fit3 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT, data = train, family = "binomial")
summary(fit3)


fit4 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT - FEMALE , data = train, family = "binomial")
summary(fit4)

fit6 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT - FEMALE - OWN_RES , data = train, family = "binomial")
summary(fit6)

fit7 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT - FEMALE - OWN_RES - SAV_ACCT , data = train, family = "binomial")
summary(fit7)

fit8 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT - FEMALE - OWN_RES - SAV_ACCT - AGE , data = train, family = "binomial")
summary(fit8)

fit9 <- glm(ANSWERED ~ . - PRODUCT - JOB - NUM_DEPENDENTS - RENT - FEMALE - OWN_RES - SAV_ACCT - AGE - NEW_CAR , data = train, family = "binomial")
summary(fit9)


#-----------------------Forward elimination (AIC)-------------------------------
# ایجاد مدل اولیه با تنها عرض از مبدا (intercept only model)
null_model <- glm(ANSWERED ~ 1, data = train, family = "binomial")

# مدل کامل شامل تمامی متغیرها (به جز PRODUCT که از قبل حذف شده)
full_model <- glm(ANSWERED ~ . - PRODUCT, data = train, family = "binomial")

# اجرای forward selection با معیار AIC
forward_model <- step(null_model,scope = list(lower = null_model, upper = full_model), direction = "forward")

# نمایش خلاصه مدل نهایی
summary(forward_model)

#-----------------------stepwise selection (AIC)-------------------------------
# اجرای stepwise selection که هم ترکیبی از forward و backward elimination است
stepwise_model <- step(full_model, 
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both")

# نمایش خلاصه مدل نهایی
summary(stepwise_model)

#------------------Logistic Regression Model evaluation-------------------------
predict_train_logistic <- predict(stepwise_model, type="response")
summary(predict_train_logistic)


T1 <- table(train$ANSWERED, predict_train_logistic > 0.47)

accuracy_train <- (T1[1,1]+T1[2,2])/nrow(train)


pred_train <- prediction(predictions = predict_train_logistic, labels = train$ANSWERED)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, print.cutoffs.at=seq(0,1,0.05), text.adj=c(-0.2, 1))
as.numeric(performance(pred_train, "auc")@y.values)



predict_test_logistic <- predict(stepwise_model, newdata = test,  type="response")

pred_test <- prediction(predictions = predict_test_logistic, labels = test$ANSWERED)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf, print.cutoffs.at=seq(0,1,0.05), text.adj=c(-0.2, 1))
as.numeric(performance(pred_test, "auc")@y.values)
T2 <- table(test$ANSWERED, predict_test_logistic > 0.47)
accuracy_test <- (T2[1,1]+T2[2,2])/nrow(test)

