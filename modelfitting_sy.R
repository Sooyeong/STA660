library(readxl)
library(lme4)

averages <- read_excel("C:/Users/sooyeong/Desktop/sta660/averages.xlsx")

#Fit models for each subjects
Tot_time <- lmer(`Total time (s)` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
AvgHr<-lmer(`avg HR` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
METS<-lmer(`METS` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
Per_vo2max <- lmer(`%of VO2max` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
Vo2_kg<-lmer(`VO2/kg` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
RPE <- lmer(`RPE` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
RQ <- lmer(`RQ` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)
EEh <- lmer(`EEh` ~ I_ebike2+I_ebike3+I_r2nd+I_r3rd+ (1|Subject), data=averages)


#An example of the output
summary(Tot_time)
