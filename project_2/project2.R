getwd()
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tableone)
library(openxlsx)
library(lubridate)
library(hms)
library(chron)
library(gtsummary)
library(GGally)
library(prettyR) #Mode function
library(mice) # multiple imputation
library(car) # vif
library(kableExtra) # latex tables

# read data
dat_ScreenTime<- read.xlsx("Data/Fulldata_620W24_Project2.xlsx",sheet=1,
                           detectDates = TRUE) %>%
  filter(Date <="2024-04-02")
# change the format of the 1st pickup to time format, transform to angular
dat_ScreenTime <- 
  dat_ScreenTime %>%
  mutate(Pickup.1st = times(as.numeric(Pickup.1st)),
         Pickup.1st.angular =
           (hours(Pickup.1st)*60+minutes(Pickup.1st))/(24*60)*360)
change_time = times("03:00:00")
dat_ScreenTime <- dat_ScreenTime %>%
  mutate(Pickup.1st = times(as.numeric(ifelse(Pickup.1st <= change_time, 
                                              Pickup.1st + 0.25, Pickup.1st))))

dat_Baseline<-read.xlsx("Data/Fulldata_620W24_Project2.xlsx",sheet = 2) %>%
  mutate(pseudo_ID = pseudo_id, 
         sex = as.numeric(ifelse(sex == "male", 1, ifelse(sex == "Female",0, sex))),
         pets = as.numeric(ifelse(pets == "No", 0, pets))) %>%
  select(-pseudo_id)
fulldat <- dat_ScreenTime %>%
  left_join(dat_Baseline, by = c("pseudo_ID"))
#### data validation ####
hm_to_min <- function(hm) {
  unlist(lapply(hm, function(x) {
    # Check if the input contains 'h' and 'm'
    if (grepl("h", x) && grepl("m", x)) {
      x <- gsub("h", "*60+", x)  # Convert hours to minutes
      x <- gsub("m(in)?s?", "", x)  # Remove 'm'
      return(eval(parse(text = x)))
    } else if (grepl("h\\d", x)) {  # If 'h' followed by digits, assume missing 'm'
      digits_before_h <- as.numeric(gsub("\\D", "", regmatches(x, regexpr("\\d+h", x))))  # Extract digits before 'h'
      digits_after_h <- as.numeric(gsub("\\D", "", regmatches(x, regexpr("h\\d+", x))))  # Extract digits after 'h'
      return(digits_before_h * 60 + digits_after_h)
    } else if (grepl("h", x)) {  # Only hours format
      x <- gsub("h", "*60", x)  # Convert hours to minutes
      return(eval(parse(text = x)))
    } else if (grepl("m", x)) {  # Only minutes format
      return(as.numeric(gsub("[^0-9]", "", x)))  # Extract numeric value
    } else {  # Numeric value (assumed to be hours)
      return(as.numeric(x) * 60)  # Convert hours to minutes
    }
  }))
}

validation <- dat_ScreenTime %>%
  mutate(
    Total.ST.min.true = as.numeric(hm_to_min(Total.ST)),
    Social.ST.min.true = as.numeric(hm_to_min(Social.ST)),
    Total.ST.match = ifelse(is.na(Total.ST.min) | is.na(Total.ST.min.true), FALSE, 
                            abs(Total.ST.min.true - as.numeric(Total.ST.min)) < 1e-5),
    # Total.ST.match1 = Total.ST.min.true == as.numeric(Total.ST.min),
    Social.ST.match = ifelse(is.na(Social.ST.min) | is.na(Social.ST.min.true), FALSE,
                             abs(Social.ST.min.true - as.numeric(Social.ST.min)) < 1e-5)#,
    # Social.ST.match1 = Social.ST.min.true == as.numeric(Social.ST.min)
  ) %>%
  relocate(Date, Total.ST, Total.ST.min, Total.ST.min.true, Total.ST.match, 
           Social.ST, Social.ST.min, Social.ST.min.true, Social.ST.match )
combined_validated <- validation %>% left_join(dat_Baseline, by = "pseudo_ID")

mean(validation$Total.ST.match)
mean(validation$Social.ST.match)

combined_validated <- combined_validated %>%
  mutate(
    Date = as.Date(Date),  
    Day = wday(Date, label = TRUE, abbr = FALSE),
    ifweekend = ifelse(Day=="Saturday"|Day=="Sunday",1,0)
  )
## ifweekend=1, it is weekend
## ifweekend=0, it is weekday

## Step 4: check correlation
combined_validated <- combined_validated %>%
  mutate(
    Pickups = as.numeric(Pickups),
    age = as.numeric(age),
    cousre.credit = as.numeric(cousre.credit),
    siblings = as.numeric(siblings),
    apps = as.numeric(apps),
    devices = as.numeric(devices),
    procrastination.score = as.numeric(procrastination.score),
    Pickup.1st.angular = as.numeric(Pickup.1st.angular),
    ifweekend = as.numeric(ifweekend),
    phase = ifelse(Date <"2024-03-27",0,1),
    compliance = case_when(
      phase == 1 & Treatment =="A" & Total.ST.min <= 200 ~ 1,
      phase == 1 & (Treatment =="A" & Total.ST.min > 200) ~ 0,
      phase == 1 & Treatment =="B" & Pickups <= 50 ~ 1,
      phase == 1 & (Treatment =="B" & Pickups > 50) ~ 0,
      phase == 0 ~ NA
    )
  )

ggpairs_plot <- ggpairs(combined_validated,
                        columns = c("Total.ST.min", "Pickups", "age",
                                    "cousre.credit", "siblings", "apps", "devices", "procrastination.score", "Pickup.1st.angular"),
                        columnLabels = c("Total Screen Time",
                                         "Total Pickups", "Age", "Course Hours", "Siblings", "# Social Apps",
                                         "# Devices", "Procrastination","1st Pickup")) +
  theme_bw()
ggpairs_plot

# Table 1:
colnames(dat_Baseline)
baseline_summary <- dat_Baseline %>% 
  select(sex,age, siblings, job, apps, procrastination.score,
         course.credit = cousre.credit, Treatment) %>% # if weekday
  mutate(total = 1) %>%
  select(total,everything())%>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    digits = all_continuous() ~ 1,
    type = list(
      sex ~ "categorical",
      age ~ "continuous",
      course.credit ~ "continuous",
      job ~ "categorical",
      siblings ~ "continuous",
      apps ~ "continuous",
      procrastination.score ~ "continuous"
    ),
    missing = "no") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Summary Statistics**") %>%
  add_n(statistic = "{n_miss} ({p_miss}%)",last = TRUE) %>%
  modify_header(n = "**Missing**")

summary_latex <-baseline_summary %>% as_gt() %>%
  gt::as_latex()
cat(summary_latex[1], sep = '\n')

# time data
timedata_summary <- combined_validated %>%
  select(Social.ST.min, Total.ST.min, Pickups, Pickup.1st.angular,ifweekend,
         phase, compliance)%>%
  mutate(total = 1) %>%
  select(total,everything()) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)",
      total ~ "N = {N}"),
    type = list(ifweekend ~ "categorical",
                compliance ~ "categorical"),
    digits = all_continuous() ~ 1,
    by = phase, # missing = "no"
    ) %>%
  bold_labels() %>%
  add_overall() 


summary_latex <-timedata_summary %>% as_gt() %>%
  gt::as_latex()
cat(summary_latex[1], sep = '\n')

combined_validated %>%
  filter(is.na(Pickup.1st.angular)) %>%
  View()

# missing patterns
library(mice)
dat <- combined_validated %>%
  select(pseudo_ID, Treatment, phase, compliance,
         Social.ST.min,Pickup.1st.angular, ifweekend,
         sex, age,  siblings, job, apps, procrastination.score, 
         course.credit = cousre.credit
         )
colnames(dat)
md.pattern(dat %>% select(-pseudo_ID))
# impute missing for baseline using mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
dat_Baseline_impute<- dat_Baseline %>%
  mutate(
    sex = ifelse(is.na(sex), getmode(sex), sex),
    age = ifelse(is.na(age), getmode(age), age),
    siblings = ifelse(is.na(siblings), getmode(siblings), siblings),
    job = ifelse(is.na(job), getmode(job), job),
    apps = ifelse(is.na(apps), getmode(apps), apps),
    procrastination.score = ifelse(is.na(procrastination.score), mean(procrastination.score, na.rm = TRUE), procrastination.score),
    cousre.credit = ifelse(is.na(cousre.credit), getmode(cousre.credit), cousre.credit)
  ) %>%
  select(pseudo_ID, sex, age, siblings, job, apps, procrastination.score,
         cousre.credit,Treatment)


# impute time data N = 2,648
# Social.ST.min 181 (6.8%)
# Pickup.1st.angular 537 (20%)
dat_time_new <- combined_validated %>%
  select(pseudo_ID, Treatment, phase, Social.ST.min, Pickup.1st.angular,
         ifweekend) %>% # no compliance
  left_join(dat_Baseline_impute)
# check which rows has na
dat_time_new[!complete.cases(dat_time_new), ] %>% View()
nrow(dat_time_new[!complete.cases(dat_time_new), ])/nrow(dat_time_new)
# [1] 0.2163897

## model and missing imputation
dat_A = dat_time_new %>% filter(Treatment == "A")

nrow(dat_A) # 1177
dat_B = dat_time_new %>% filter(Treatment == "B")
nrow(dat_B) # 1471

set.seed(1)
dat_A_impute = mice(dat_A,m=5, method = "pmm")
# dat_A_imputed = complete(dat_A_impute, "all")
dat_A_fit = with(data = dat_A_impute, 
                 exp = lm(Social.ST.min ~ age+ sex + siblings + job + apps + 
                            phase + procrastination.score + # cousre.credit +
                            ifweekend + Pickup.1st.angular))
sum.modelA = summary(pool(dat_A_fit))
set.seed(1)
dat_B_impute = mice(dat_B,m=5, method = "pmm")
# dat_B_imputed = complete(dat_B_impute, "all")
dat_B_fit = with(data = dat_B_impute, 
                 exp = lm(Social.ST.min ~ age + sex + siblings + job + apps + 
                            phase + procrastination.score + # cousre.credit +
                            ifweekend + Pickup.1st.angular))
sum.modelB = summary(pool(dat_B_fit))


# model diagnosis
par(mfrow = c(1, 2))
plot(dat_A_fit$analyses[[1]],which = c(1,2))
plot(dat_B_fit$analyses[[1]],which = c(1,2))

vif(dat_A_fit$analyses[[1]])
vif(dat_B_fit$analyses[[1]]) # course credit> 10 multicollinearity




#### compliance ####
df_compliance <- combined_validated %>%
  filter(as.Date('2024-03-27') <= as.Date(Date) & as.Date(Date) <= as.Date('2024-04-02'))
dim(df_compliance) # 238 35

df_compliance2 <- df_compliance %>%
  mutate(Total.ST.min.true = ifelse(is.na(Total.ST.min.true), Total.ST.min, Total.ST.min.true)) %>%
  mutate(compliance_manually = ifelse((Treatment == "A" & Total.ST.min.true <= 200) | (Treatment == "B" & Pickups <= 50), 1, 0)) %>%
  mutate(compliance_manually = as.factor(compliance_manually)) %>%
  mutate(compliance = as.factor(compliance)) %>%
  mutate(IfTues_Thur = ifelse(Day == "Tuesday" | Day == "Thursday", 1, 0)) %>%
  mutate(compliance_manually = case_when(
    pseudo_ID == "9285" & Date == "2024-03-27" ~ as.factor(0),
    pseudo_ID == "9285" & Date == "2024-03-28" ~ as.factor(0),
    pseudo_ID == "9285" & Date == "2024-03-29" ~ as.factor(1),
    pseudo_ID == "9285" & Date == "2024-03-30" ~ as.factor(0),
    pseudo_ID == "9285" & Date == "2024-03-31" ~ as.factor(0),
    pseudo_ID == "9285" & Date == "2024-04-01" ~ as.factor(1),
    pseudo_ID == "9285" & Date == "2024-04-02" ~ as.factor(1),
    TRUE ~ compliance_manually
  ))

table(df_compliance2$IfTues_Thur)
table(df_compliance2$compliance_manually)
table(df_compliance2$compliance)
sum(is.na(df_compliance2$compliance_manually))
unique(df_compliance2$pseudo_ID[is.na(df_compliance2$compliance_manually)])
table(df_compliance2$compliance_manually,df_compliance2$compliance)
sum(is.na(df_compliance2$compliance))
sum(is.na(df_compliance2$Total.ST.min.true))
unique(df_compliance2$pseudo_ID[is.na(df_compliance2$Total.ST.min.true)])
unique(df_compliance2$pseudo_ID[is.na(df_compliance2$Total.ST.min)])
View(df_compliance2)

df_pre_mean <- combined_validated %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Total.ST.min.true = ifelse(is.na(Total.ST.min.true), Total.ST.min, Total.ST.min.true)) %>%
  filter(Date >= as.Date('2024-03-13') & Date <= as.Date('2024-03-27')) %>%
  group_by(pseudo_ID) %>%
  summarize(
    pre_meanTotal_screen = mean(Total.ST.min.true, na.rm = TRUE),  # Calculate mean, remove NA values
    pre_mean_pick = mean(Pickups, na.rm = TRUE)
  ) %>%
  unique()
View(df_pre_mean)

final_compliance <- df_compliance2 %>%
  left_join(df_pre_mean, by="pseudo_ID")
#View(final_compliance)

colnames(final_compliance)

model_compliance <- glm(compliance ~ age + as.factor(sex) + as.factor(job) + 
                          apps + pre_meanTotal_screen + pre_mean_pick + 
                          IfTues_Thur + procrastination.score + siblings,
                        family = binomial(link = "logit"),
                        data = final_compliance)
sum.model_C = summary(model_compliance)


# get tables
kbl(sum.modelA[,-5],format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)
kbl(sum.modelB[,-5],format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)
kbl(sum.model_C$coefficients,format = "latex",booktabs = TRUE,longtable=TRUE,digits = 2)
