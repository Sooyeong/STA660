install.packages("xlsx")
library("xlsx")
library("dplyr")
library("ggplot2")
averages <- read_excel("averages.xlsx")
View(averages)


#For the preliminary data analysis, we would like to see the difference between three groups..


e2<-averages %>%
  filter(averages$Bike=="e-assist 2")

e3<-averages %>%
  filter(averages$Bike=="e-assist 3")

regular<-averages %>%
  filter(averages$Bike=="Regular")

#For each group we have 12 observations
par(mfrow=c(1,2))
boxplot_RPE<- ggplot(averages, aes(x=Bike, y=RPE)) + geom_boxplot()
boxplot_HR<- ggplot(averages, aes(x=Bike, y=`avg HR`))+geom_boxplot()


prow<-plot_grid(boxplot_RPE+theme(legend.position="none"),
                boxplot_HR+theme(legend.position="none"),
                ncol=2)
prow
p <- plot_grid(prow, rel_widths=c(3,.3))
plot_grid(p, ncol=1, rel_heights=c(0.1, 1))
