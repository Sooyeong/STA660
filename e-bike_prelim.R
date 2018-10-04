library(readxl)
library(emmeans)
library(car)
library(agricolae)
library(gridExtra)
library(cowplot)
library(lme4)
#https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

averages <- read_excel("Desktop/660/averages.xlsx")
averages$Bike<-factor(averages$Bike)
averages<-within(averages, Bike <- relevel(Bike, ref = "Regular"))

# HR
hr <- lmer(`avg HR` ~ Bike*factor(Order)+ (1|Subject), data=averages)
summary(hr)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`avg HR`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`avg HR`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=121,label="119.75")+
  annotate(geom="text",x=2,y=112,label="107.36")+
  annotate(geom="text",x=3,y=127,label="125.92")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average Heart Rate (bpm") +
  guides(fill=FALSE)

#%VO2 Max
vo2max <- lmer(`%of VO2max` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(vo2max)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`%of VO2max`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`%of VO2max`, fill=Bike),width=.1)+
  annotate(geom="text",x=3,y=57,label="54.31")+
  annotate(geom="text",x=1,y=49,label="46.14")+
  annotate(geom="text",x=2,y=40,label="37.57")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average VO2 Max") +
  guides(fill=FALSE)

#RPE
rpe <- lmer(`RPE` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(rpe)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`RPE`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`RPE`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=11,label="9.92")+
  annotate(geom="text",x=2,y=11,label="9.93")+
  annotate(geom="text",x=3,y=14,label="12.67")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average Rate of Percieved Effort") +
  guides(fill=FALSE)

#RF
rf <- lmer(`Rf` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(rf)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`Rf`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`Rf`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=29.5,label="27.15")+
  annotate(geom="text",x=2,y=27.5,label="24.52")+
  annotate(geom="text",x=3,y=29.5,label="27.24")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average RF") +
  guides(fill=FALSE)

#RQ
rq <- lmer(`RQ` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(rq)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`RQ`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`RQ`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=.85,label=".84")+
  annotate(geom="text",x=2,y=.85,label=".84")+
  annotate(geom="text",x=3,y=.85,label=".87")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average RQ") +
  guides(fill=FALSE)

#EEm
EEm <- lmer(`EEm` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(EEm)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`EEm`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`EEm`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=8,label="7.73")+
  annotate(geom="text",x=2,y=8,label="7.07")+
  annotate(geom="text",x=3,y=9.5,label="9.53")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Average EEm") +
  guides(fill=FALSE)

#time
time <- lmer(`Total time (s)` ~ Bike*factor(Order) + (1|Subject), data=averages)
summary(time)
ggplot(data=averages) +
  geom_boxplot(aes(x=Bike, y=`Total time (s)`, fill=Bike)) + 
  geom_jitter(aes(x=Bike, y=`Total time (s)`, fill=Bike),width=.1)+
  annotate(geom="text",x=1,y=785,label="781")+
  annotate(geom="text",x=2,y=725,label="719.58")+
  annotate(geom="text",x=3,y=897,label="887")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Total Time (s)") +
  guides(fill=FALSE) #+
  scale_fill_manual(values=c("#C3142D", "gray", "#941728"))
