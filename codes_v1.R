######## biostat 620 project 1 #######
####### Lufeiya Liu, Anran Yao #######

#### packages ####
library(dplyr)
library(tidyverse)
library(gtsummary)
library(GGally) # for correlation plots
library(kableExtra)
library(ggplot2)
library(hms)
library(circular)
library(car)


#### read the data ####
timedata<-read.csv("Data/groupdata_01W04 - timeseries.csv")
baseline<-read.csv("Data/groupdata_01W04 - baseline.csv")
baseline<-baseline[, -ncol(baseline)]

# change the format of the 1st pickup to time format, transform to angular
timedata <- timedata %>%
  mutate(Pickup.1st = strptime(Pickup.1st, format = "%H:%M"),
         Pickup.1st.angular =
           (hour(Pickup.1st)*60+minute(Pickup.1st))/(24*60)*360)

# combine the data with baseline
combined <- timedata %>% left_join(baseline, by = c("ID"="ID"))

#### data validation ####
hm_to_min = function(hm){
  unlist(lapply(hm,function(x){
    if(grepl("h", x) && !grepl("m", x)) {
      x = gsub("h", "*60", x)
      return(eval(parse(text = x)))
    }else if(grepl("h", x)){
      x = gsub("h","*60+",x)
      x = gsub("m","",x)
      return(eval(parse(text=x)));
    }else{
      return(as.numeric(gsub("m","",x)))
    }})) #convert to total minutes
}

validation = timedata %>% 
  mutate(Total.ST.min.true = hm_to_min(Total.ST), 
         Social.ST.min.true = hm_to_min(Social.ST),
         Total.ST.match = Total.ST.min.true==Total.ST.min, 
         Social.ST.match = Social.ST.min.true == Social.ST.min)%>%
  relocate(Date,Total.ST,Total.ST.min,Total.ST.min.true,Total.ST.match,
           Social.ST,Social.ST.min,Social.ST.min.true,Social.ST.match)
validation$Total.ST.match
validation$Social.ST.match
combined_validated <- validation %>% left_join(baseline, by = c("ID"="ID"))
# write.csv(combined_validated,"combined_validated.csv")

#### descriptive statistics ####
colnames(baseline)
baseline_summary <- baseline %>% 
  select(-ID) %>%
  mutate(total = 1) %>%
  select(total,everything())%>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    digits = all_continuous() ~ 1,
    type = list(
      workmate ~ "continuous",
      academic ~ "continuous",
      non.academic ~ "continuous",
      pets ~ "continuous",
      sex ~ "categorical",
      age ~ "continuous",
      course_hours ~ "continuous",
      degree ~ "categorical",
      job ~ "categorical",
      siblings ~ "continuous",
      apps ~ "continuous",
      devices ~ "continuous",
      procrastination ~ "continuous"
    ),
    missing = "no") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Summary Statistics**")

timedata_summary <- timedata %>%
  select(Total.ST.min,Social.ST.min,Pickups,Pickup.1st.angular,
         Daily.Social.Prop,Daily.Duration.Use,Weekdays)%>%
  mutate(total = 1) %>%
  select(total,everything())%>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    type = Weekdays ~ "categorical",
    digits = all_continuous() ~ 1,
    missing = "no") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Summary Statistics**")

tbl_all <-tbl_stack(tbls = list(baseline_summary, timedata_summary))
summary_latex <-tbl_all %>% as_gt() %>%
  gt::as_latex()
cat(summary_latex[1], sep = '\n')

#### plots ######
#### correlation ####
timedata %>% 
  ggpairs(columns = c(4,6,7,9,10,14), progress = FALSE)+ theme_bw()

# first.pickup.circular distribution
layout(1)
first.pickup.cir = circular(timedata$Pickup.1st.angular, units="degrees", 
                            template="clock24")
first.pickup.cir.den = density(first.pickup.cir,bw=50)
plot(first.pickup.cir.den, points.plot = T,shrink=1.3, main = "")

#### boxplots#######
layout(matrix(c(1,2,3,4,5), nrow = 1))
boxplot(Total.ST.min~ID,timedata,ylab="Total Screen Time (min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray")
boxplot(Social.ST.min~ID,timedata,ylab="Social Screen Time (min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray")
boxplot(Pickups~ID,timedata,ylab="Number of Pickups",
        xlab="Individual",all.outliers=TRUE,col = "lightgray")
boxplot(Daily.Social.Prop~ID,timedata,
        ylab="Daily Social Apps Screen Time Proportion",
        xlab="Individual",all.outliers=TRUE,col = "lightgray")
boxplot(Daily.Duration.Use~ID,timedata,ylab="Daily Duration Use(min)",
        xlab="Individual",all.outliers=TRUE,col = "lightgray")


#### federated learning ####
combined1 <- combined_validated %>% 
  select(-Total.ST,-Total.ST.min,-Total.ST.match,
         -Social.ST,-Social.ST.min,-Social.ST.match) %>%
  mutate(Total.ST.min=Total.ST.min.true,
         Social.ST.min=Social.ST.min.true)
# data for person 1
timedata1 <- subset(combined1, ID==1)
# data for person 2
timedata2 <- subset(combined1, ID==2)

x1 = model.matrix(timedata1$Social.ST.min~timedata1$Pickup.1st.angular
                  +timedata1$Pickups
                  +timedata1$Weekdays
                  +timedata1$apps)
x2 = model.matrix(timedata2$Social.ST.min~timedata2$Pickup.1st.angular
                  +timedata2$Pickups
                  +timedata2$Weekdays
                  +timedata2$apps)
SSX1 = t(x1)%*%x1
SSXY1 = t(x1)%*%timedata1$Social.ST.min 
SSY1 = t(timedata1$Social.ST.min) %*% timedata1$Social.ST.min

SSX2 = t(x2)%*%x2
SSXY2 = t(x2)%*%timedata2$Social.ST.min 
SSY2 = t(timedata2$Social.ST.min) %*% timedata2$Social.ST.min
# beta hat estimates
beta_y <- solve(SSX1+SSX2)%*%(SSXY1+SSXY2)

sigma_y <-((SSY1+SSY2)-2*t(beta_y)%*%(SSXY1+SSXY2)+
             t(beta_y)%*%(SSX1+SSX2)%*%(beta_y))/(88-5)
ste <- sqrt(sigma_y)
# standard errors
se_beta = t(ste%*%sqrt(diag(as.matrix(solve(as.matrix(SSX1+SSX2))))))
# t statistics
t_statistic = beta_y/se_beta

# p values
p_value = c(2*(1-pt(q=abs(t_statistic),df=83)))

# federal results
fed_table = data.frame(beta_y=beta_y, se_beta=se_beta, t_statistic, p_value)
fed_table

# calculate RSS
RSS = ((SSY1+SSY2)-2*t(beta_y)%*%(SSXY1+SSXY2)+
         t(beta_y)%*%(SSX1+SSX2)%*%(beta_y))

# calculate R^2a
y_bar1 = mean(timedata1$Social.ST.min)
y_bar2 = mean(timedata2$Social.ST.min)
y_bar = 44/88*y_bar1+44/88*y_bar2
TSS = (SSY1+SSY2)-88*y_bar^2
Ra_2 = 1-RSS/(88-5)/(TSS/(88-1))
Ra_2 # 0.8030031

#### confirmation analysis ####
lm_1 <- lm(Social.ST.min~Pickup.1st.angular
           +Pickups
           +Weekdays
           +apps , data=combined1) 
(lm_1.sum<-summary(lm_1))
anova(lm_1) # see RSS

# get tables
# kbl(lm_1.sum$coefficients,format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)

##### model diagnosis #####
hist(lm_1$residuals,main = "",xlab = "Residuals")
vif(lm_1)%>% kbl(format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)
par(mfrow = c(1, 2))
plot(lm_1,c(1,2))
