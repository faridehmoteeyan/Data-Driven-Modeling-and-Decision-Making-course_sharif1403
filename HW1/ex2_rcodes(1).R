#------------------------------------2------------------------------------------
#بررسی وجود یا عدم وجود ارتباط بین دریافت پیامک و مراجعه یا عدم مراجعه
table2 <- table(data$SMS_received, data$No.show)
table2
chisq.test(table2)


#انجام آزمون نسبت
sum_sms0 <- sum(data$SMS_received == 0)
sum_sms1 <- sum(data$SMS_received == 1)

show_SMS0 <- sum(data$SMS_received == 0 & data$No.show == "No")
show_SMS1 <- sum(data$SMS_received == 1 & data$No.show == "No")

prop.test(x=c(show_SMS0, show_SMS1), n=c(sum_sms0, sum_sms1),
          alternative = "greater", correct=F)




#بررسی وجود یا عدم وجود ارتباط بین دریافت یا عدم دریافت کمک هزینه و مراجعه یا عدم مراجعه
table3 <- table(data$Scholarship, data$No.show)
table3
chisq.test(table3)

#انجام آزمون نسبت
sum_scholarship0 <- sum(data$Scholarship == 0)
sum_scholarship1 <- sum(data$Scholarship == 1)

show_scholarship0 <- sum(data$Scholarship == 0 & data$No.show == "No")
show_scholarship1 <- sum(data$Scholarship == 1 & data$No.show == "No")

prop.test(x=c(show_scholarship0, show_scholarship1), n=c(sum_scholarship0, sum_scholarship1),
          alternative = "greater", correct=F)



#بررسی وجود یا عدم وجود ارتباط بین بیماری الکل و مراجعه یا عدم مراجعه
table4 <- table(data$Alcoholism, data$No.show)
table4
chisq.test(table4)


#بررسی وجود یا عدم وجود ارتباط بین بیماری فشار خون و مراجعه یا عدم مراجعه
table5 <- table(data$Hypertension, data$No.show)
table5
chisq.test(table5)

#انجام آزمون نسبت
sum_hypertension0 <- sum(data$Hypertension == 0)
sum_hypertension1 <- sum(data$Hypertension == 1)

Show_hypertension0 <- sum(data$Hypertension == 0 & data$No.show == "No")
show_hypertension1 <- sum(data$Hypertension == 1 & data$No.show == "No")

prop.test(x=c(Show_hypertension0, show_hypertension1), n=c(sum_hypertension0, sum_hypertension1),
          alternative = "less", correct=F)



#بررسی وجود یا عدم وجود ارتباط بین بیماری دیابت و مراجعه یا عدم مراجعه

table6 <- table(data$Diabetes, data$No.show)
table6
chisq.test(table6)

#انجام آزمون نسبت
sum_diabetes0 <- sum(data$Diabetes == 0)
sum_diabetes1 <- sum(data$Diabetes == 1)

Show_diabetes0 <- sum(data$Diabetes == 0 & data$No.show == "No")
show_diabetes1 <- sum(data$Diabetes == 1 & data$No.show == "No")

prop.test(x=c(Show_diabetes0, show_diabetes1), n=c(sum_diabetes0, sum_diabetes1),
          alternative = "less", correct=F)
