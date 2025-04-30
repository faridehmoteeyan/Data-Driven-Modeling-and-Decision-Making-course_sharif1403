
# کتابخانه های استفاده شده
library(tidyverse)
library(dslabs)
library(dplyr)
library(GGally)
library(ggplot2)
# ------------------------------------DataStructure----------------------------------=-
data <- read.csv("no-show.csv",stringsAsFactors = T)


str(data)

data$Scholarship <- as.factor(data$Scholarship)

data$Hypertension <- as.factor (data$Hypertension)

data$Diabetes <- as.factor (data$Diabetes)

data$AppointmentID <- as.factor (data$AppointmentID)

data$PatientID <- as.factor (data$PatientID)

data$Alcoholism <- as.factor (data$Alcoholism)

data$Handicap <- as.factor (data$Handicap)

data$SMS_received <- as.factor (data$SMS_received)

data$AppointmentDay <- ymd_hms (data$AppointmentDay)

data$ScheduledDay <- as.Date (data$ScheduledDay)

data$No.show <- as.factor (data$No.show)

# --------------------------------------------AddNewColumns--------------------------------------------
# ایجاد ستون های جدید
# ستون روزی که برای قرار ملاقات رزرو شده است
data <- data %>% mutate (DOW_a=wday(AppointmentDay, label=TRUE)) %>% relocate(DOW_a, .after = 5)

#ستون فاصله بین روز ثبت نام تا روز رزرو شده
data <- data %>% mutate(Days_Difference = as.integer(time_length(interval(ScheduledDay, AppointmentDay), "day")))
data <- data %>% relocate(Days_Difference, .after = 6)

#ستون روزی که ملاقات رزرو شده است
data$ScheduledDay <- ymd (data$ScheduledDay)
data <- data %>% mutate (DOW_s=wday(ScheduledDay, label=TRUE)) %>% relocate(DOW_s, .after = 4)

#برای دیدن کل شرایط پزشکی بیمار(فشارخون، دیابت،لکلی بودن،معلول بودن)
data <- data %>%
  mutate(
    illness_status = paste(
      if_else(Alcoholism == 1, "Alcoholism", NA_character_),
      if_else(Hypertension == 1, "Hypertension", NA_character_),
      if_else(Diabetes == 1, "Diabetes", NA_character_),
      sep = ", "
    ),
    illness_status = gsub(", NA|NA, |NA", "", illness_status),
    illness_status = if_else(illness_status == "", "No Illness", illness_status) # مقداردهی در صورت عدم وجود بیماری
  )

data$illness_status <- as.factor (data$illness_status)
# -----------------------------------------------checkDuplicate------------------------------------------------------------
# چک کردن داده هایی که داپلیکیت شده اند
# از جایی که تمامی
str(data)
sum(as.data.frame(table(data$AppointmentID))$Freq>1)

# ----------------------------------------------CheckNan-------------------------------------------------------------------
#داده خالی در دیتا ست یافت نشد
sum(is.na.data.frame(data))


# -----------------------------Univariate data analysis------------------
# برای داده های کتگوریکال نمودار هیستوگرام رسم میکنیم
# این کد بار دیگر پس از تمیز سازی میتوان برای آنالیز مجدد استفاده کرد


columns <- c( "PatientID","Gender","DOW_s","DOW_a","Days_Difference","Neighbourhood","Scholarship","Hypertension","Diabetes","Alcoholism","Handicap","SMS_received","No.show")

# حلقه برای ایجاد نمودار برای هر ستون
for (col in columns) {
  data_percent1 <- data %>%
    group_by(!!sym(col)) %>%
    summarise(count = n()) %>%
    mutate(percentage1 = count / sum(count))
  data_percent1
  
  plot2 <- data_percent1 %>% ggplot(aes(x=!!sym(col) , y=percentage1))+
    geom_bar(stat="identity",fill="darkgreen")+
   # scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent(percentage1)), 
              vjust = -.5 ,colour = "black",size=3)+
    labs (title = paste("Percent of", col), x = col, y = "Percentage")+
    theme(plot.title = element_text(hjust = .5))
  plot2
  print(plot2)  # نمایش نمودار
}
colnames(data)

# داده های عددی
columns2 = c("ScheduledDay","AppointmentDay","Days_Difference","Age")
columns2 = c("ScheduledDay")
for (col in columns2) {
  print(col)
  plot4<- data %>% 
  ggplot(aes(x=!!sym(col)))+
  geom_boxplot()+
  labs(title = paste("box plot of",!!sym(col)),x = !!sym(col))+
    
    
  print(plot4)}
  

#-------------------------------------cleaning-----------------------------------
#معلولیت نمیتواند چهارحالت باشد درنتیجه داده هایی که معلولیت میتوانند حذف شود.اما ممکن است این دید وجود داشته باشد که اشتباها جای یک این مقادیر وارد شده اند
#در اینجا این داده ها با یک جایگزین میشوند
data$Handicap =replace(data$Handicap,data$Handicap %in% c(2,3,4),1)
table(data$Handicap)

#دو ستون اختلاف بین روز رزرو و روز رزرو و ستون سن نمیتوانند منفی باشند
data <- data %>% filter(Age>=0)

data <- data %>% filter(Days_Difference>=0)
#--------------------------------------------حذف داده پرت بیمارانی که رزرو زیاد بدون حضور دارند----------------------------------------------------------

paitent0 = as.data.frame.matrix(table(data$PatientID,data$No.show))
paitent0 %>% arrange(desc(No))
paitent0 %>% 
  ggplot(aes(x=No,y=Yes))+
  geom_point()+
  labs(title = "patient performance",y="shown up",x="did not show up")
#.باتوجه به نتیجه تعدادی از بیماران احتمالا داده پرت هستند.مثلا بیمار ی که 87 کنسلی دارد.
#تصمیم گرفته شد که داده های خیلی اختلاف دارند با روش چانکی حذف کنم .
paitent = as.data.frame(table(data$PatientID,data$No.show))
paitent = paitent %>%  filter(Var2 == "No")

# به نظر می اید با توجه به تعداد روزهایی که میتوانستیم رزرو کنیم بعضی اعداد غیر منطقی هستند.
max(data$AppointmentDay)-min(data$AppointmentDay)
max(data$ScheduledDay)-min(data$ScheduledDay)

#تصمیم گرفته شد که داده های خیلی اختلاف دارند با روش چانکی حذف کنم .
#عدد چانکی عدد خیلی کوچکی به نظر میاید چون در انالیز فقط میشود نه یا بله را دید .
#با گرفتن محاسبات باکس پلات برای هر تعداد رزروی که بیمار امده مشخص میشود چه عددی برای کنسل کردن منطقی است.در این گزارش 
#در این گزارش عدد 50 رزرو بی حضور به عنوان داده پرت در نظر گرفته میشود
Q <- quantile(paitent$Freq, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(paitent$Freq) 
up <-  Q[2]+1.5*iqr # Upper Range  
up
paitent <-paitent %>% filter(Freq>=50)
paitent
data <-data %>% filter(!data$PatientID %in% paitent$Var1)

#-------------------------------------- Bivariate data analysis-------------------------------------
#میتوان از این کتابخانه برای کاهش حجم کد محاسبات استفاده کرد
#با توجه به هدف تمرین و تعداد جدول ها کلیه جداول در بخش بعد آورده شده اند
clomn = c("Gender","DOW_s","DOW_a","Days_Difference","Neighbourhood","Scholarship","illness_status","No.show","ScheduledDay","AppointmentDay")
data %>% ggpairs(columns =clomn,cardinality_threshold = 100) 
#-----------------------------------------------رسم نمودراها--------------------------------------------------------  
data_percent1 <- data %>%
  group_by(No.show) %>%
  summarise(count = n()) %>%
  mutate(percentage1 = count / sum(count))
data_percent1

plot2 <- data_percent1 %>% ggplot(aes(x=No.show , y=percentage1))+
  geom_bar(stat="identity",fill="lightgreen")+
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage1)), 
            position = position_stack(vjust = 0.5))+
  labs (title = "percent of No Show", x="No Show", y="percentage")+
  theme(plot.title = element_text(hjust = 0.5))
plot2


plot3 <- data %>% ggplot(aes(x=No.show,fill = Gender))+
  geom_bar(position="dodge")+
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust=-0.3)+
  labs(title = "Count Of No Show by Gender",x="No show" , y="Count")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
plot3

data_percent2 <- data %>%
  group_by(No.show, Gender) %>%
  summarise(count = n()) %>%
  mutate(percentage2 = count / sum(count))

plot3 <- ggplot(data_percent2, aes(x = No.show, y = percentage2, fill = Gender)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage2), group = Gender), 
            position = position_fill(vjust = 0.5)) +
  labs(title = "percent Of No Show by Gender", y = "Percentage", x = "No show") +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
plot3


plot4 <- data %>% 
  ggplot(aes(x = No.show, fill = Scholarship)) + 
  geom_bar(position = "dodge") + 
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3) + 
  labs(title = "Count Of No Show by Scholarship", 
       x = "No Show", 
       y = "Count") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5))
plot4

scholarship_percent <- data %>% 
  group_by(No.show, Scholarship) %>%
  summarise(count = n()) %>%
  mutate(percentage3 = count / sum(count))
scholarship_percent

plot5 <- scholarship_percent %>% 
  ggplot(aes(x = No.show,y=percentage3, fill = Scholarship)) + 
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(percentage3)), position = position_fill(vjust=0.5))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Percentage of No Show by Scholarship", 
       x = "No Show", 
       y = "Percentage") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5))
plot5


plot6 <- ggplot(data , aes(x = DOW, fill= No.show)) +
  geom_bar(position = "dodge") +
  labs(title = "No-Show Counts by Day of the Week",
       x = "Day of the Week",
       y = "Count of No-Shows") +
  geom_text(stat = "count",aes(label = ..count..),position = position_dodge(width = 0.9), vjust=-0.3)+
  theme(plot.title = element_text(hjust = 0.5))
plot6

DOW_percent <- data %>%
  group_by(DOW, No.show) %>%
  summarize(count = n()) %>%
  mutate(percentage4 = count / sum(count))

plot7 <- ggplot(DOW_percent , aes(x = DOW, y=percentage4, fill= No.show)) +
  geom_bar(stat = "identity",position = "fill") +
  labs(title = "No-Show percentage by Day of the Week",
       x = "Day of the Week",
       y = "percentages of No-Shows") +
  geom_text(aes(label = scales::percent(percentage4)),position = position_fill(vjust=0.5))+
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5))
plot7

plot8 <- data %>% ggplot(aes(x=Hypertension))+
  geom_bar(fill="lightgreen")+
  geom_text(stat="count",aes(label=..count..), vjust = -0.3)+
  labs(title = "Count of Hypertension", x="No Show", y="count")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
plot8 

Hypertension_percent <- data %>% group_by(Hypertension) %>%
  summarize(count = n()) %>% 
  mutate(percentage5 = count/sum(count))

plot9 <- Hypertension_percent %>% ggplot(aes(x=Hypertension, y = percentage5))+
  geom_bar(stat = "identity", fill = "lightgreen")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(percentage5)),
            position = position_stack(vjust = 0.5))+
  labs (title = "percent of Hypertension", x = "No Show", y = "percentage")+
  theme(plot.title = element_text(hjust = 0.5))
plot9


Diabetes_percent <- data %>% group_by(Diabetes) %>%
  summarize(count = n()) %>% 
  mutate(percentage6 = count / sum(count))


plot10 <- Diabetes_percent %>% ggplot(aes(x = Diabetes, y = percentage6)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage6)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percent of Diabetes", x = "Diabetes", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot10


Alcoholism_percent <- data %>% group_by(Alcoholism) %>%
  summarize(count = n()) %>% 
  mutate(percentage7 = count / sum(count))


plot11 <- Alcoholism_percent %>% ggplot(aes(x = Alcoholism, y = percentage7)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage7)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percent of Alcoholism", x = "Alcoholism", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot11


Handicap_percent <- data %>% group_by(Handicap) %>%
  summarize(count = n()) %>% 
  mutate(percentage8 = count / sum(count))


plot12 <- Handicap_percent %>% ggplot(aes(x = Handicap, y = percentage8)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage8)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percent of Handicap", x = "Handicap", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot12


SMS_percent <- data %>% group_by(SMS_received) %>%
  summarize(count = n()) %>% 
  mutate(percentage9 = count / sum(count))


plot13 <- SMS_percent %>% ggplot(aes(x = SMS_received, y = percentage9)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage9)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percent of SMS Received", x = "SMS Received", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot13


illness_status_percent <- data %>% group_by(illness_status) %>%
  summarize(count = n()) %>% 
  mutate(percentage10 = count / sum(count))

plot14 <- illness_status_percent %>% ggplot(aes(x = illness_status, y = percentage10)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage10)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percent of Illness Status", x = "Illness Status", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot14

Hypertension_percent <- data %>% group_by(Hypertension, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage11 = count / sum(count))

plot15 <- Hypertension_percent %>% ggplot(aes(x = Hypertension, y = percentage11, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage11)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Hypertension and No Show", x = "Hypertension", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot15

Diabetes_percent <- data %>% group_by(Diabetes, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage12 = count / sum(count))

plot16 <- Diabetes_percent %>% ggplot(aes(x = Diabetes, y = percentage12, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage12)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Diabetes and No Show", x = "Diabetes", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot16


Alcoholism_percent <- data %>% group_by(Alcoholism, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage13 = count / sum(count))

plot17 <- Alcoholism_percent %>% ggplot(aes(x = Alcoholism, y = percentage13, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage13)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Alcoholism and No Show", x = "Alcoholism", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot17


Handicap_percent <- data %>% group_by(Handicap, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage14 = count / sum(count))

plot18 <- Handicap_percent %>% ggplot(aes(x = Handicap, y = percentage14, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage14)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Handicap and No Show", x = "Handicap", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot18


SMS_percent <- data %>% group_by(SMS_received, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage15 = count / sum(count))

plot19 <- SMS_percent %>% ggplot(aes(x = SMS_received, y = percentage15, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage15)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "SMS Received and No Show", x = "SMS Received", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot19


illness_status_percent <- data %>% group_by(illness_status, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage16 = count / sum(count))

plot20 <- illness_status_percent %>% ggplot(aes(x = illness_status, y = percentage16, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage16)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Illness Status and No Show", x = "Illness Status", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot20

scholarship_percent <- data %>% group_by(Scholarship, No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage = count / sum(count))
scholarship_percent

plot21 <- scholarship_percent %>% ggplot(aes(x = Scholarship, y = percentage, fill = No.show)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Scholarship and No Show", x = "Scholarship", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot21

sms_percent <- data %>% group_by(Scholarship, SMS_received) %>%
  summarize(count = n()) %>% 
  mutate(percentage = count / sum(count))
sms_percent

plot22 <- sms_percent %>% ggplot(aes(x = Scholarship, y = percentage, fill = SMS_received)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Scholarship and sms recived", x = "Scholarship", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot22


sms_percent <- data %>% group_by( SMS_received ,Scholarship) %>%
  summarize(count = n()) %>% 
  mutate(percentage = count / sum(count))
sms_percent

plot23 <- sms_percent %>% ggplot(aes(fill = Scholarship, y = percentage, x = SMS_received)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Scholarship and sms recived", x = "Scholarship", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))
plot23

neghbourhood_percent <- data %>% group_by(Neighbourhood,No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage = count / sum(count))
neghbourhood_percent


neghbourhood_percent %>% filter(No.show=="Yes")%>% arrange(desc (percentage))
plot24 <- neghbourhood_percent %>% arrange(count) %>% 
  ggplot(aes(x=Neighbourhood,fill = No.show,y=count))+
  geom_bar(position="stack", stat="identity")+
  theme(axis.text.x = element_text(size = 8,angle = 90))+
  labs(title="Count how many times ‘did not show up’ appears in different neighbors")
plot24


plot25 <-neghbourhood_percent %>% arrange(count) %>% 
  ggplot(aes(x=Neighbourhood,fill = No.show,y=percentage))+
  geom_bar(position="stack", stat="identity")+
  theme(axis.text.x = element_text(size = 8,angle = 90))+
  labs(title="percentage of how many times ‘did not show up’ appears in different neighbors")
plot25

age_percent <- data %>% group_by(Age,No.show) %>%
  summarize(count = n()) %>% 
  mutate(percentage = count / sum(count))
age_percent

plot26 <-age_percent %>% arrange(count) %>% 
  ggplot(aes(x=Age,fill = No.show,y=count))+
  geom_bar(position="stack", stat="identity")+
  theme(axis.text.x = element_text(size = 8,angle = 90))+
  labs(title="Count how many times ‘did not show up’ appears in different ages")
plot26




illness_statu_age_no_show <- data %>% group_by(illness_status,Age,No.show) %>%
  summarize(count = n()) %>% filter(No.show=="No") 
illness_statu_age_no_show

plot27 <- illness_statu_age_no_show %>% 
  ggplot(aes(x=illness_status,y=Age))+
  geom_tile(aes(fill=count))+
  labs(title="number of patient witch did not show up and their illness_status and age")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_gradient(low = "lightgreen",high="darkgreen")
plot27


scholarship_age_no_show <- data %>% group_by(Scholarship,Age,No.show) %>%
  summarize(count = n()) %>% filter(No.show=="No") 
scholarship_age_no_show 

plot27 <- scholarship_age_no_show  %>% 
  ggplot(aes(x=Scholarship,y=Age))+
  geom_tile(aes(fill=count))+
  labs(title="number of patient who did not show up and their scholarship and age")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_gradient(low = "lightgreen",high="darkgreen")
plot27
#- آیا روز هفته در میزان show-no موثر است؟ 

# روزی که وقت میگیریم
day_noshow_count =as.data.frame(table(data$DOW_s,data$No.show))
colnames(day_noshow_count)[1:2] <-c("day","no.show")
day_noshow_count = day_noshow_count %>% filter(!day %in% c("Sat","Sun"))
day_noshow_count

plot28 <-day_noshow_count %>% ggplot(aes(x=day,y=Freq,group=no.show,colour = no.show))+
  geom_line(size=1,)+
  geom_point( color = "black",size=2)+
  labs(title="no_shows and shows during the day they ware scheduled ",x="days",y="frequency")+
  geom_text(aes(label = Freq), vjust = -1, size = 3,colour ="black")
plot28

  
#محاسبه درصد 
day_noshow_count2 =as.data.frame.matrix(table(data$DOW_s,data$No.show)) 
day_noshow_count2$percentage = day_noshow_count2$Yes/sum(day_noshow_count2$Yes,day_noshow_count2$No)
day_noshow_count2 = day_noshow_count2[2:6,1:3]
day_noshow_count2

plot29<- day_noshow_count2 %>% ggplot(aes(x=c("Mon","Tue","Wed","Thu","Fri"),y=percentage,))+
  geom_line(group=1,size=1,color="lightblue")+
  geom_point( color = "red",size=2)+
  labs(title="no_shows percentage during the day they ware acheduled",x="days",y="percentage")+
  geom_text(aes(label = round(percentage,4)*100), vjust = -1, size = 3,colour ="black")
plot29



day_noshow_count2 =as.data.frame(table(data$DOW_a,data$No.show))
colnames(day_noshow_count2)[1:2] <-c("day","no.show")
day_noshow_count2 = day_noshow_count2 %>% filter(!day %in% c("Sat","Sun"))
day_noshow_count

plot30 <-day_noshow_count2 %>% ggplot(aes(x=day,y=Freq,group=no.show,colour = no.show))+
  geom_line(size=1,)+
  geom_point( color = "black",size=2)+
  labs(title="no_shows and shows during the days",x="days",y="frequency")+
  geom_text(aes(label = Freq), vjust = -1, size = 3,colour ="black")
plot30

#محاسبه درصد 
day_noshow_count3 =as.data.frame.matrix(table(data$DOW_a,data$No.show)) 
day_noshow_count3$percentage = day_noshow_count3$Yes/sum(day_noshow_count3$Yes,day_noshow_count3$No)
day_noshow_count3 = day_noshow_count3[2:6,1:3]
day_noshow_count3

plot31 <-day_noshow_count3 %>% ggplot(aes(x=c("Mon","Tue","Wed","Thu","Fri"),y=percentage,))+
  geom_line(group=1,size=1,color="lightblue")+
  geom_point( color = "red",size=2)+
  labs(title="no_shows percentage during the days",x="days",y="percentage")+
  geom_text(aes(label = round(percentage,4)*100), vjust = -1, size = 3,colour ="black")
plot31

#box days_difference
plot32<-data %>% ggplot(aes(x=No.show,y=Days_Difference))+
  geom_boxplot(aes(colour = No.show))+
  labs(title ="compersion  according to Days_Difference and show up  ")
plot32

#box age
plot33<-data %>% ggplot(aes(x=No.show,y=Age))+
  geom_boxplot(aes(colour = No.show))+
  labs(title ="compersion  according to age and show up  ")
plot33




   
