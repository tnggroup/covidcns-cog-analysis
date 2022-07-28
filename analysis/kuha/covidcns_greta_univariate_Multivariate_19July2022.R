library(ggplot2)
covidcns_greta <- readRDS("C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/covidcns_greta_18July2022.rds")

write.csv(covidcns_greta, file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/covidcns_greta_18July2022.csv")

dim(covidcns_greta)
#[1]   648 3394

covidcns_cognitive_data <- readRDS("C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/covidcns_cognitive_data_18July2022.rds")

write.csv(covidcns_cognitive_data, file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/covidcns_cognitive_data_18July2022.csv")

dim(covidcns_cognitive_data)
#[1]   648  81
names(covidcns_cognitive_data)

####### Read data #####
### case definition all data
data_All=read.csv(file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/Final_Case_Definitions_updated_18July2022.csv",header=T)
dim(data_All) 
names(data_All)
table(data_All$Case.Control.Vaccine)

merge_data_All = merge(data_All,covidcns_greta,by=c("ID"))
dim(merge_data_All)
names(merge_data_All)
write.csv(merge_data_All,file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_18July2022/merge_data_All_18July22.csv")
table(merge_data_All$Case.Control.Vaccine)
#table(merge_data_All$case_control_vaccine)
################
### data Covid positive cases and positive controls
data_case_positiveControls=subset(merge_data_All,(merge_data_All$Case.Control.Vaccine=="COVID Positive Case"|merge_data_All$Case.Control.Vaccine=="COVID Positive Control"))
dim(data_case_positiveControls)
data_case_positiveControls$case_control_vaccine=data_case_positiveControls$Case.Control.Vaccine
table(data_case_positiveControls$case_control_vaccine)
## COVID Positive Case COVID Positive Control 
##      194                     81 

attach(data_case_positiveControls)

#################
##WHO Covid severity
summary(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19)
table(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)
summary(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)

dim(data_case_positiveControls)
data_case_positiveControls$WHO_Covid_severity_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==0]="Uninfected"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==1|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==2|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==3]="Ambulatory mild disease"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==4|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==5]="Hospitalised: moderate"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==6|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==7|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==8|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==9]="Hospitalised: severe"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==10]="Dead"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==-777]=NA
data_case_positiveControls$WHO_Covid_severity_group[is.na(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)]=NA

table(data_case_positiveControls$WHO_Covid_severity_group)




##Pre-morbid Rockwell Clinical Frailty Score
summary(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale)
table(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric)


data_case_positiveControls$clinical_frailty_scale_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==1|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==2|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==3]="Managing well"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==4|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==5]="Mild"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==6|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==7|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==8]="Moderate-severe"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==9]="Terminally ill "
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==-777]=NA
data_case_positiveControls$clinical_frailty_scale_group[is.na(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric)]=NA

table(data_case_positiveControls$clinical_frailty_scale_group)


##Timeframe of admission in 6 month windows starting in March 2020.
summary(data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission) 

###define how many cases vs Controls were recruited in the first COVID-19 wave in every 6 months from March 2020
data_case_positiveControls$time_window_covid19=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2020-03-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2020-09-01 00:00:00"]=1
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2020-09-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2021-03-01 00:00:00"]=2
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2021-03-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2021-09-01 00:00:00"]=3
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2021-09-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2022-03-01 00:00:00"]=4

summary(data_case_positiveControls$time_window_covid19)
table(data_case_positiveControls$time_window_covid19)

###############


#### subset of cases 
data_case=subset(data_case_positiveControls,(data_case_positiveControls$case_control_vaccine=="COVID Positive Case"))
dim(data_case)
### subset of positive controls 
data_positiveControls=subset(data_case_positiveControls,(data_case_positiveControls$case_control_vaccine=="COVID Positive Control"))
dim(data_positiveControls)

############# Normality test for continuous variables ############
#shapiro.test(All_covariates[,3])
##W = 0.9678, p-value = 8.362e-07: reject null hypothesis. Not normal data

#### Median(IQR) for continuous variables #####
Median_IQR=function(x,y,k){
  Median=median(x[which(y==k)],na.rm = TRUE)
  IQR=IQR(x[which(y==k)],na.rm = TRUE)
  return(c(Median=Median,IQR=IQR))}

###### M: Mann-Whitney test #####
Mtest_continuous=function(x,y,alpha){
  Missing=sum(is.na(x))
  Missing_percentage=sum(is.na(x))/length(x)*100
  Mtest=wilcox.test(x~y,conf.int = TRUE, conf.level = (1-alpha)) 
  p_value=Mtest$p.value
  difference= Mtest$estimate
  CI=Mtest$conf.int
  return(c(Missing=Missing,Missing_percentage=Missing_percentage,p_value=p_value,difference=difference,CI=CI))
}
####################
#Predictors:
#  Age ??? dem.dob_age
#COVID severity ??? ncrf1_vital.admission_severity_worst_covid19
#Clinical group ??? defined above
#Education ??? dem.highest_education
#Clinical frailty scale ??? ncrf1_comorbid.clinical_frailty_scale
#MH comorbidity ??? PHQ9/GAD7/PCL5
#Fatigue ??? Cfs questionnaire
#COVID19 vaccination status ??? ncrf1_pre_med.had_covid19_vaccine/case_control_vaccine/vaccine questionnaire
#Treatment (corticosteroid) ??? ncrf2_med.corticosteroid
#degree of peripheral inflammation ??? ncrf1_labresults (CRP/WCC)
#subjective cognitive impairment ??? dem.memory questions
#time since COVID-19 ??? ncrf1_admission.date_of_inpatient_admission/.date_of_positive_covid19_test

table(dem.highest_education)
######################
## Age distribution
n_obs=dim(data_case_positiveControls)[1]
summary(data_case$dem.dob_age)
sd(data_case$dem.dob_age,na.rm=T)

summary(data_positiveControls$dem.dob_age)
sd(data_positiveControls$dem.dob_age,na.rm=T)

shapiro.test(data_case_positiveControls$dem.dob_age)
##W = 0.98654, p-value = 0.01145: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$dem.dob_age,data_case_positiveControls$case_control_vaccine,alpha=0.05)


## Gender distribution
summary(data_case$dem.sex_at_birth)
summary(data_case$dem.sex_at_birth)/(nrow(data_case)-1)

summary(data_positiveControls$dem.sex_at_birth)
summary(data_positiveControls$dem.sex_at_birth)/(nrow(data_positiveControls))

table(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$dem.sex_at_birth_numeric)
chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$dem.sex_at_birth_numeric)
#fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$dem.sex_at_birth_numeric)

##WHO Covid severity
table(data_case$WHO_Covid_severity_group)
61/dim(data_case)[1]
table(data_case$WHO_Covid_severity_group)/(dim(data_case)[1]-61)
table(data_positiveControls$WHO_Covid_severity_group)
11/dim(data_positiveControls)[1]
table(data_positiveControls$WHO_Covid_severity_group)/(dim(data_positiveControls)[1]-11)
fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$WHO_Covid_severity_group, simulate.p.value = T)
#chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$WHO_Covid_severity_group)

### Clinical group including controls
data_case_positiveControls$Diagnostic.Group[data_case_positiveControls$Diagnostic.Group=="Other "]="Other"
data_case_positiveControls$Diagnostic.Group[data_case_positiveControls$Diagnostic.Group==""]="Control"

table(data_case_positiveControls$Diagnostic.Group)
### Clinical groups in cases only
table(data_case$Diagnostic.Group)
1/dim(data_case)[1]
table(data_case$Diagnostic.Group)/(dim(data_case)[1]-1)
table(data_positiveControls$Diagnostic.Group)


####Education: dem.highest_education
summary(data_case_positiveControls$dem.highest_education)
#table(data_case_positiveControls$dem.highest_education_numeric)

summary(data_case$dem.highest_education)
table(data_case$dem.highest_education)
2/dim(data_case)[1]
table(data_case$dem.highest_education)/(dim(data_case)[1]-2)

summary(data_positiveControls$dem.highest_education)
table(data_positiveControls$dem.highest_education)
table(data_positiveControls$dem.highest_education)/dim(data_positiveControls)[1]

fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$dem.highest_education, simulate.p.value = T)


##Pre-morbid Rockwell Clinical Frailty Score
table(data_case$clinical_frailty_scale_group)
40/dim(data_case)[1]
table(data_case$clinical_frailty_scale_group)/(dim(data_case)[1]-40)

table(data_positiveControls$clinical_frailty_scale_group)
12/dim(data_positiveControls)[1]
table(data_positiveControls$clinical_frailty_scale_group)/(dim(data_positiveControls)[1]-12)
fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$clinical_frailty_scale_group, simulate.p.value = T)


##Timeframe of admission in 6 month windows starting in March 2020.
summary(data_case$time_window_covid19)
table(data_case$time_window_covid19)
47/dim(data_case)[1]
table(data_case$time_window_covid19)/(dim(data_case)[1]-1)

summary(data_positiveControls$time_window_covid19)
table(data_positiveControls$time_window_covid19)
11/dim(data_positiveControls)[1]
table(data_positiveControls$time_window_covid19)/(dim(data_positiveControls)[1]-11)

chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$time_window_covid19)
#fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$time_window_covid19, simulate.p.value = T)

### Composite_global
summary(data_case$Composite_global)
sd(data_case$Composite_global,na.rm = T)
2/dim(data_case)[1]
summary(data_positiveControls$Composite_global)
sd(data_positiveControls$Composite_global,na.rm = T)

shapiro.test(data_case_positiveControls$Composite_global)
##W = 0.95092, p-value = 6.124e-08: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_global,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Composite_acc
summary(data_case$Composite_acc)
sd(data_case$Composite_acc,na.rm = T)
2/dim(data_case)[1]
summary(data_positiveControls$Composite_acc)
sd(data_positiveControls$Composite_acc,na.rm = T)

shapiro.test(data_case_positiveControls$Composite_acc)
##W = 0.97388, p-value = 6.798e-05: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_acc,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Composite_rt
summary(data_case$Composite_rt)
sd(data_case$Composite_rt,na.rm = T)
2/dim(data_case)[1]
summary(data_positiveControls$Composite_rt)
sd(data_positiveControls$Composite_rt,na.rm = T)

shapiro.test(data_case_positiveControls$Composite_rt)
##W = 0.80261, p-value < 2.2e-16: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_rt,data_case_positiveControls$case_control_vaccine,alpha=0.05)


### PHQ9 sum score
summary(data_case$phq9.sum_score)
sd(data_case$phq9.sum_score,na.rm = T)
20/dim(data_case)[1]
summary(data_positiveControls$phq9.sum_score)
sd(data_positiveControls$phq9.sum_score,na.rm = T)
11/dim(data_positiveControls)[1]
shapiro.test(data_case_positiveControls$phq9.sum_score)
##W = 0.89561, p-value = 5.828e-12: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$phq9.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### GAD7 sum score
summary(data_case$gad7.sum_score)
sd(data_case$gad7.sum_score,na.rm = T)
18/dim(data_case)[1]
summary(data_positiveControls$gad7.sum_score)
sd(data_positiveControls$gad7.sum_score,na.rm = T)
11/dim(data_positiveControls)[1]
shapiro.test(data_case_positiveControls$gad7.sum_score)
##W = 0.83812, p-value = 2.564e-15: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$gad7.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### PCL5 sum score
summary(data_case$pcl5.sum_score)
sd(data_case$pcl5.sum_score,na.rm = T)
50/dim(data_case)[1]
summary(data_positiveControls$pcl5.sum_score)
sd(data_positiveControls$pcl5.sum_score,na.rm = T)
28/dim(data_positiveControls)[1]
shapiro.test(data_case_positiveControls$pcl5.sum_score)
##W = 0.83617, p-value = 1.256e-13: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$pcl5.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Fatigue ??? Cfs questionnaire
## Fatigue Mental 
summary(data_case$cfs.mental_subscale)
sd(data_case$cfs.mental_subscale,na.rm = T)
50/dim(data_case)[1]
summary(data_positiveControls$cfs.mental_subscale)
sd(data_positiveControls$cfs.mental_subscale,na.rm = T)
28/dim(data_positiveControls)[1]
shapiro.test(data_case_positiveControls$cfs.mental_subscale)
##W = 0.83617, p-value = 1.256e-13: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$cfs.mental_subscale,data_case_positiveControls$case_control_vaccine,alpha=0.05)


## Fatigue physical 
summary(data_case$cfs.physical_subscale)
sd(data_case$cfs.physical_subscale,na.rm = T)
26/dim(data_case)[1]
summary(data_positiveControls$cfs.physical_subscale)
sd(data_positiveControls$cfs.physical_subscale,na.rm = T)
13/dim(data_positiveControls)[1]
shapiro.test(data_case_positiveControls$cfs.physical_subscale)
##W = 0.93461, p-value = 9.55e-09: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$cfs.physical_subscale,data_case_positiveControls$case_control_vaccine,alpha=0.05)


### COVID19 vaccination status ??? ncrf1_pre_med.had_covid19_vaccine/case_control_vaccine/vaccine questionnaire
summary(data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine)
data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group[data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine=="Yes"]=1
data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group[data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine=="No"]=0

table (data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group)

table(data_case$ncrf1_pre_med.had_covid19_vaccine)
40/dim(data_case)[1]
table(data_case$ncrf1_pre_med.had_covid19_vaccine)/(dim(data_case)[1]-40)

summary(data_positiveControls$ncrf1_pre_med.had_covid19_vaccine)
table(data_positiveControls$ncrf1_pre_med.had_covid19_vaccine)
12/dim(data_positiveControls)[1]
table(data_positiveControls$ncrf1_pre_med.had_covid19_vaccine)/(dim(data_positiveControls)[1]-12)

chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine)
chisq.test(data_case_positiveControls$case_control_vaccine[data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine=="Yes"|data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine=="No"],data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine[data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine=="Yes"|data_case_positiveControls$ncrf1_pre_med.had_covid19_vaccine=="No"])

##### Treatment (corticosteroid) ??? ncrf2_med.corticosteroid
summary(data_case_positiveControls$ncrf2_med.corticosteroid)
data_case_positiveControls$ncrf2_med.corticosteroid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$ncrf2_med.corticosteroid_group[data_case_positiveControls$ncrf2_med.corticosteroid=="Yes"]=1
data_case_positiveControls$ncrf2_med.corticosteroid_group[data_case_positiveControls$ncrf2_med.corticosteroid=="No"]=0
table (data_case_positiveControls$ncrf2_med.corticosteroid_group)
table(data_case$ncrf2_med.corticosteroid)
40/dim(data_case)[1]
table(data_case$ncrf2_med.corticosteroid)/(dim(data_case)[1]-40)

summary(data_positiveControls$ncrf2_med.corticosteroid)
table(data_positiveControls$ncrf2_med.corticosteroid)
14/dim(data_positiveControls)[1]
table(data_positiveControls$ncrf2_med.corticosteroid)/(dim(data_positiveControls)[1]-14)

#chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$ncrf2_med.corticosteroid)
fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$ncrf2_med.corticosteroid)
chisq.test(data_case_positiveControls$case_control_vaccine[data_case_positiveControls$ncrf2_med.corticosteroid=="Yes"|data_case_positiveControls$ncrf2_med.corticosteroid=="No"],data_case_positiveControls$ncrf2_med.corticosteroid[data_case_positiveControls$ncrf2_med.corticosteroid=="Yes"|data_case_positiveControls$ncrf2_med.corticosteroid=="No"])

#### degree of peripheral inflammation ??? ncrf1_labresults (CRP/WCC)
## CRP levels
summary(data_case_positiveControls$ncrf1_lab.crp_level)

summary(data_case$ncrf1_lab.crp_level)
table(data_case$ncrf1_lab.crp_level)
39/dim(data_case)[1]
table(data_case$ncrf1_lab.crp_level)/(dim(data_case)[1]-39)

summary(data_positiveControls$ncrf1_lab.crp_level)
table(data_positiveControls$ncrf1_lab.crp_level)
17/dim(data_positiveControls)[1]
table(data_positiveControls$ncrf1_lab.crp_level)/(dim(data_positiveControls)[1]-17)

fisher.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$ncrf1_lab.crp_level)
##### WBC counts 
summary (data_case_positiveControls$ncrf1_lab.wbc_count_level)
summary(data_case$ncrf1_lab.wbc_count_level)
table(data_case$ncrf1_lab.wbc_count_level)
40/dim(data_case)[1]
table(data_case$ncrf1_lab.wbc_count_level)/(dim(data_case)[1]-40)

summary(data_positiveControls$ncrf1_lab.wbc_count_level)
table(data_positiveControls$ncrf1_lab.wbc_count_level)
14/dim(data_positiveControls)[1]
table(data_positiveControls$ncrf1_lab.wbc_count_level)/(dim(data_positiveControls)[1]-14)

chisq.test(data_case_positiveControls$case_control_vaccine,data_case_positiveControls$ncrf1_lab.wbc_count_level)

##### subjective cognitive impairment ??? dem.memory questions
##"dem.concerned_memory" 
data_case_positiveControls$dem.concerned_memory_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_group[data_case_positiveControls$dem.concerned_memory=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_group[data_case_positiveControls$dem.concerned_memory=="No"]=0

summary (data_case_positiveControls$dem.concerned_memory)
table (data_case_positiveControls$dem.concerned_memory_group)

summary(data_case$dem.concerned_memory)
table(data_case$dem.concerned_memory)
1/dim(data_case)[1]
table(data_case$dem.concerned_memory)/(dim(data_case)[1]-1)

summary(data_positiveControls$dem.concerned_memory)
table(data_positiveControls$dem.concerned_memory)
table(data_positiveControls$dem.concerned_memory)/(dim(data_positiveControls)[1])

chisq.test(data_case_positiveControls$case_control_vaccine[data_case_positiveControls$dem.concerned_memory=="Yes"|data_case_positiveControls$dem.concerned_memory=="No"],data_case_positiveControls$dem.concerned_memory[data_case_positiveControls$dem.concerned_memory=="Yes"|data_case_positiveControls$dem.concerned_memory=="No"])


##"dem.concerned_memory_before_covid" 
data_case_positiveControls$dem.concerned_memory_before_covid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_before_covid_group[data_case_positiveControls$dem.concerned_memory_before_covid=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_before_covid_group[data_case_positiveControls$dem.concerned_memory_before_covid=="No"]=0

summary (data_case_positiveControls$dem.concerned_memory_before_covid)
table (data_case_positiveControls$dem.concerned_memory_before_covid_group)
### "dem.concerned_memory_after_covid"
summary (data_case_positiveControls$dem.concerned_memory_after_covid) 
data_case_positiveControls$dem.concerned_memory_after_covid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_after_covid_group[data_case_positiveControls$dem.concerned_memory_after_covid=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_after_covid_group[data_case_positiveControls$dem.concerned_memory_after_covid=="No"]=0
table (data_case_positiveControls$dem.concerned_memory_after_covid_group)
###dem.memory_problem_worse_year
summary (data_case_positiveControls$dem.memory_problem_worse_year)
data_case_positiveControls$dem.memory_problem_worse_year_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.memory_problem_worse_year_group[data_case_positiveControls$dem.memory_problem_worse_year=="Yes"]=1
data_case_positiveControls$dem.memory_problem_worse_year_group[data_case_positiveControls$dem.memory_problem_worse_year=="No"]=0
table (data_case_positiveControls$dem.memory_problem_worse_year_group)
##dem.has_your_memory_got_progressively_worse
summary (data_case_positiveControls$dem.has_your_memory_got_progressively_worse) 
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group[data_case_positiveControls$dem.has_your_memory_got_progressively_worse=="Yes"]=1
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group[data_case_positiveControls$dem.has_your_memory_got_progressively_worse=="No"]=0
table (data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group)
### List of variables for multiple regression models 
attach(data_case_positiveControls)
data_multipleReg=data.frame(cbind(dem.dob_age,dem.sex_at_birth,WHO_Covid_severity_group,Diagnostic.Group,dem.highest_education,clinical_frailty_scale_group,
                                  phq9.sum_score,gad7.sum_score,pcl5.sum_score,
                                  cfs.mental_subscale,cfs.physical_subscale,ncrf1_pre_med.months_presentation.had_covid19_vaccine_group,ncrf2_med.corticosteroid_group,ncrf2_lab.crp,ncrf2_lab.wbc_count,
                       ncrf2_lab.crp_level,ncrf2_lab.wbc_count_level,dem.concerned_memory_group,dem.concerned_memory_before_covid_group,dem.concerned_memory_after_covid_group,
                       time_window_covid19,Composite_global,Composite_acc,Composite_rt))
dim(data_multipleReg)
names(data_multipleReg)
table(data_multipleReg[,1])
data_multipleReg_complete=data_multipleReg[complete.cases(data_multipleReg),]
dim(data_multipleReg_complete)

summary(as.numeric(dem.dob_age))
fit1=lm(Composite_global~as.numeric(dem.dob_age),data=data_multipleReg)
summary(fit1)
#########
summary(as.factor(dem.sex_at_birth))
fit1=lm(Composite_global~as.factor(dem.sex_at_birth),data=data_multipleReg)
summary(fit1)
##########
summary(as.factor(WHO_Covid_severity_group))
fit1=lm(Composite_global~relevel(as.factor(WHO_Covid_severity_group),ref="Uninfected"),data=data_multipleReg) ### exclude due to missing data
summary(fit1)
#########
fit1=lm(Composite_global~relevel(as.factor(Diagnostic.Group), ref = "Control"),data=data_multipleReg)
summary(fit1)
##########
fit1=lm(Composite_global~as.factor(dem.highest_education),data=data_multipleReg)
summary(fit1)
#######
summary(as.factor(clinical_frailty_scale_group))
fit1=lm(Composite_global~as.factor(clinical_frailty_scale_group),data=data_multipleReg)  ### exclude due to missing data
summary(fit1)
######
fit1=lm(Composite_global~ as.numeric(phq9.sum_score),data=data_multipleReg)
summary(fit1)
#####
fit1=lm(Composite_global~as.numeric(gad7.sum_score),data=data_multipleReg)
summary(fit1)
####
summary(as.numeric(pcl5.sum_score))
fit1=lm(Composite_global~as.numeric(pcl5.sum_score),data=data_multipleReg) ### exclude due to missing data
summary(fit1)
####
fit1=lm(Composite_global~as.numeric(cfs.mental_subscale),data=data_multipleReg)
summary(fit1)
### 
fit1=lm(Composite_global~as.numeric(cfs.physical_subscale),data=data_multipleReg)
summary(fit1)
####
summary(as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine))
fit1=lm(Composite_global~as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group),data=data_multipleReg) ### exclude due to missing data
summary(fit1)
####
summary(as.factor(ncrf2_med.corticosteroid_group))
summary(as.factor(ncrf2_med.corticosteroid))
fit1=lm(Composite_global~as.factor(ncrf2_med.corticosteroid_group),data=data_multipleReg) ## exclude due to missing data 
summary(fit1)
#####
summary(as.numeric(ncrf1_lab.crp))
summary(as.numeric(ncrf2_lab.crp))

fit1=lm(Composite_global~ as.numeric(ncrf2_lab.crp),data=data_multipleReg) ### exclude due to missing 
summary(fit1)
####
summary(as.numeric(ncrf1_lab.wbc_count))
summary(as.numeric(ncrf2_lab.wbc_count))

fit1=lm(Composite_global~as.numeric(ncrf2_lab.wbc_count),data=data_multipleReg) ### exclude due to missing data 
summary(fit1)
####
fit1=lm(Composite_global~ as.factor(dem.concerned_memory_group),data=data_multipleReg)
summary(fit1)
####
summary(as.factor(dem.concerned_memory_before_covid))
summary(as.factor(dem.concerned_memory_before_covid_group))
fit1=lm(Composite_global~as.factor(dem.concerned_memory_before_covid_group),data=data_multipleReg) ### exclude due to missing data
summary(fit1)
#####
summary(as.factor(dem.concerned_memory_after_covid))
summary(as.factor(dem.concerned_memory_after_covid_group))
fit1=lm(Composite_global~ as.factor(dem.concerned_memory_after_covid_group),data=data_multipleReg) ### exclude due to missing data 
summary(fit1)
###
###dem.memory_problem_worse_year
summary (data_case_positiveControls$dem.memory_problem_worse_year)
summary (data_case_positiveControls$dem.memory_problem_worse_year_group)
fit1=lm(Composite_global~ as.factor(dem.memory_problem_worse_year_group),data=data_multipleReg) ### exclude due to missing data 
summary(fit1)
####
##dem.has_your_memory_got_progressively_worse
summary (data_case_positiveControls$dem.has_your_memory_got_progressively_worse) 
summary (data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group) 

fit1=lm(Composite_global~ as.factor(dem.has_your_memory_got_progressively_worse_group),data=data_multipleReg) ### exclude due to missing data 
summary(fit1)
#####
fit1=lm(Composite_global~as.factor(time_window_covid19),data=data_multipleReg)
summary(fit1)
######
### fit regression model with all variables  
fit1=lm(Composite_global~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(WHO_Covid_severity_group),ref="Uninfected")+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+as.factor(clinical_frailty_scale_group)+
          as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+as.numeric(pcl5.sum_score)
          as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group)+as.factor(ncrf2_med.corticosteroid_group)+
          as.numeric(ncrf2_lab.crp)+as.numeric(ncrf2_lab.wbc_count)+
          as.factor(dem.concerned_memory_group)+ as.factor(dem.concerned_memory_before_covid_group)+ as.factor(dem.concerned_memory_after_covid_group)+
          as.factor(time_window_covid19),data=data_multipleReg)
summary(fit1)

### fit regression model exclude the variables with >20% missing data 
data_multipleReg_Exclude_missing_data_variables =data.frame(cbind(dem.dob_age,dem.sex_at_birth,Diagnostic.Group,dem.highest_education,
                                                                  phq9.sum_score,gad7.sum_score,
                                                                  cfs.mental_subscale,cfs.physical_subscale,dem.concerned_memory_group,
                                                                  time_window_covid19,Composite_global,Composite_acc,Composite_rt))
dim(data_multipleReg_Exclude_missing_data_variables)
data_multipleReg_Exclude_missing_data_variables_complete=data_multipleReg_Exclude_missing_data_variables[complete.cases(data_multipleReg_Exclude_missing_data_variables),]
dim(data_multipleReg_Exclude_missing_data_variables_complete)
attach(data_multipleReg_Exclude_missing_data_variables_complete)
### Regression model for Composite_global
fit1_1=lm(Composite_global~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
          as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
          as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
          as.factor(dem.concerned_memory_group)+
          as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_complete)
summary(fit1_1)
### step AIC
library(MASS)
fit2_1=stepAIC(fit1_1,direction = "backward",k=2)

#fit2_1=stepAIC(fit1_1,direction = "backward",k=log(nrow(data_multipleReg_Exclude_missing_data_variables_complete)))
fit2_1=lm(Composite_global~as.numeric(dem.dob_age)+as.numeric(phq9.sum_score)+
            as.numeric(cfs.mental_subscale),data=data_case_positiveControls)
summary(fit2_1)
### Regression model for Composite_acc
fit1_2=lm(Composite_acc~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
            as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
            as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
            as.factor(dem.concerned_memory_group)+
            as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_complete)
summary(fit1_2)
### step AIC
library(MASS)
fit2_2=stepAIC(fit1_2,direction = "backward",k=2)

#fit2_2=stepAIC(fit1_2,direction = "backward",k=log(nrow(data_multipleReg_Exclude_missing_data_variables_complete)))
fit2_2=lm(Composite_acc~as.factor(dem.highest_education)+
            as.numeric(phq9.sum_score),data=data_case_positiveControls)
summary(fit2_2)
### Regression model for Composite_rt
fit1_3=lm(Composite_rt~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
            as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
            as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
            as.factor(dem.concerned_memory_group)+
            as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_complete)
summary(fit1_3)
### step AIC
library(MASS)
fit2_3=stepAIC(fit1_3,direction = "backward",k=2)

#fit2_3=stepAIC(fit1_3,direction = "backward",k=log(nrow(data_multipleReg_Exclude_missing_data_variables_complete)))
fit2_3=lm(Composite_rt~as.numeric(dem.dob_age)+as.numeric(phq9.sum_score)+
            as.numeric(cfs.mental_subscale),data=data_case_positiveControls)

summary(fit2_3)
##############################
#### Multiple regression models for each task variables 
attach(data_case_positiveControls)
names(covidcns_cognitive_data)

### fit regression model exclude the variables with >20% missing data 
data_multipleReg_Exclude_missing_data_variables_tasks =data.frame(cbind(dem.dob_age,dem.sex_at_birth,Diagnostic.Group,dem.highest_education,
                                                                  phq9.sum_score,gad7.sum_score,
                                                                  cfs.mental_subscale,cfs.physical_subscale,dem.concerned_memory_group,
                                                                  time_window_covid19,rs_manipulations2D_dfe,rs_TOL_dfe,rs_prospectiveMemoryWords_1_delayed_dfe,
                                                                  rs_prospectiveMemoryWords_1_immediate_dfe,rs_spatialSpan_dfe,rs_targetDetection_dfe,
                                                                  rs_verbalAnalogies_dfe,rs_manipulations2D_RT_dfe,rs_TOL_RT_dfe,rs_prospectiveMemoryWords_1_delayed_RT_dfe,
                                                                  rs_prospectiveMemoryWords_1_immediate_RT_dfe, rs_spatialSpan_RT_dfe,rs_targetDetection_RT_dfe, rs_verbalAnalogies_RT_dfe))
dim(data_multipleReg_Exclude_missing_data_variables_tasks)
data_multipleReg_Exclude_missing_data_variables_tasks_complete=data_multipleReg_Exclude_missing_data_variables_task1[complete.cases(data_multipleReg_Exclude_missing_data_variables_task1),]
dim(data_multipleReg_Exclude_missing_data_variables_tasks_complete)
attach(data_multipleReg_Exclude_missing_data_variables_tasks_complete)
## task 1. rs_manipulations2D_dfe
fit1_t1=lm(as.numeric(rs_manipulations2D_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
            as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
            as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
            as.factor(dem.concerned_memory_group)+
            as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t1)
### step AIC
fit2_t1=stepAIC(fit1_t1,direction = "backward",k=2)

#fit2_t1=stepAIC(fit1_t1,direction = "backward",k=log(nrow(data_multipleReg_Exclude_missing_data_variables_complete)))
fit2_t1=lm(as.numeric(rs_manipulations2D_dfe)~relevel(as.factor(Diagnostic.Group), ref = "Control")+as.numeric(phq9.sum_score)+as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t1)
####### 
## task2. rs_TOL_dfe 
fit1_t2=lm(as.numeric(rs_TOL_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t2)
### step AIC
fit2_t2=stepAIC(fit1_t2,direction = "backward",k=2)

#fit2_t2=stepAIC(fit1_t2,direction = "backward",k=log(nrow(data_multipleReg_Exclude_missing_data_variables_complete)))
fit2_t2=lm(as.numeric(rs_TOL_dfe)~as.numeric(phq9.sum_score)+as.numeric(cfs.mental_subscale),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t2)
####### 
## task3. rs_prospectiveMemoryWords_1_delayed_dfe
fit1_t3=lm(as.numeric(rs_prospectiveMemoryWords_1_delayed_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t3)
### step AIC
fit2_t3=stepAIC(fit1_t3,direction = "backward",k=2)

fit2_t3=lm(as.numeric(rs_prospectiveMemoryWords_1_delayed_dfe)~as.numeric(dem.dob_age)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.numeric(phq9.sum_score)+as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t3)
####### 
## task4. rs_prospectiveMemoryWords_1_immediate_dfe
fit1_t4=lm(as.numeric(rs_prospectiveMemoryWords_1_immediate_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t4)
### step AIC
fit2_t4=stepAIC(fit1_t4,direction = "backward",k=2)

fit2_t4=lm(as.numeric(rs_prospectiveMemoryWords_1_immediate_dfe)~as.numeric(phq9.sum_score),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t4)
####### 
## task5. rs_spatialSpan_dfe
fit1_t5=lm(as.numeric(rs_spatialSpan_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t5)
### step AIC
fit2_t5=stepAIC(fit1_t5,direction = "backward",k=2)

fit2_t5=lm(as.numeric(rs_spatialSpan_dfe)~as.numeric(gad7.sum_score),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t5)
####### 
## task6. rs_targetDetection_dfe
fit1_t6=lm(as.numeric(rs_targetDetection_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t6)
### step AIC
fit2_t6=stepAIC(fit1_t6,direction = "backward",k=2)
fit2_t6=lm(as.numeric(rs_targetDetection_dfe)~as.numeric(dem.dob_age) +relevel(as.factor(Diagnostic.Group), ref = "Control")+as.numeric(phq9.sum_score)+as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t6)
####### 
## task7. rs_verbalAnalogies_dfe
fit1_t7=lm(as.numeric(rs_verbalAnalogies_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_t7)
### step AIC
fit2_t7=stepAIC(fit1_t7,direction = "backward",k=2)

fit2_t7=lm(as.numeric(rs_verbalAnalogies_dfe)~as.numeric(phq9.sum_score)+as.numeric(cfs.physical_subscale),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)

summary(fit2_t7)
####### 
## task_RT 1. rs_manipulations2D_RT_dfe
fit1_rt1=lm(as.numeric(rs_manipulations2D_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
             as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
             as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
             as.factor(dem.concerned_memory_group)+
             as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt1)
### step AIC
fit2_rt1=stepAIC(fit1_rt1,direction = "backward",k=2)
summary(fit2_rt1)
fit2_rt1=lm(as.numeric(rs_manipulations2D_RT_dfe)~as.numeric(dem.dob_age)+as.numeric(cfs.physical_subscale) +as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt1)
####### 
## task_RT 2. rs_TOL_RT_dfe
fit1_rt2=lm(as.numeric(rs_TOL_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt2)
### step AIC
fit2_rt2=stepAIC(fit1_rt2,direction = "backward",k=2)
summary(fit2_rt2)
fit2_rt2=lm(as.numeric(rs_TOL_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+as.numeric(cfs.mental_subscale),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt2)
####### 
## task_RT 3. rs_prospectiveMemoryWords_1_delayed_RT_dfe
fit1_rt3=lm(as.numeric(rs_prospectiveMemoryWords_1_delayed_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt3)
### step AIC
fit2_rt3=stepAIC(fit1_rt3,direction = "backward",k=2)
summary(fit2_rt3)
fit2_rt3=lm(as.numeric(rs_prospectiveMemoryWords_1_delayed_RT_dfe)~as.numeric(dem.dob_age)+as.numeric(phq9.sum_score)+as.numeric(cfs.mental_subscale),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt3)
####### 
## task_RT 4. rs_prospectiveMemoryWords_1_immediate_RT_dfe
fit1_rt4=lm(as.numeric(rs_prospectiveMemoryWords_1_immediate_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt4)
### step AIC
fit2_rt4=stepAIC(fit1_rt4,direction = "backward",k=2)
summary(fit2_rt4)
fit2_rt4=lm(as.numeric(rs_prospectiveMemoryWords_1_immediate_RT_dfe)~as.numeric(phq9.sum_score)+as.numeric(cfs.mental_subscale),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt4)
####### 
## task_RT 5. rs_spatialSpan_RT_dfe
fit1_rt5=lm(as.numeric(rs_spatialSpan_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt5)
### step AIC
fit2_rt5=stepAIC(fit1_rt5,direction = "backward",k=2)
summary(fit2_rt5)
fit2_rt5=lm(as.numeric(rs_spatialSpan_RT_dfe)~as.numeric(cfs.mental_subscale) ,data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt5)
####### 
## task_RT 6. rs_targetDetection_RT_dfe
fit1_rt6=lm(as.numeric(rs_targetDetection_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt6)
### step AIC
fit2_rt6=stepAIC(fit1_rt6,direction = "backward",k=2)
summary(fit2_rt6)
fit2_rt6=lm(as.numeric(rs_targetDetection_RT_dfe)~as.numeric(phq9.sum_score),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt6)
####### 
## task_RT 7. rs_verbalAnalogies_RT_dfe 
fit1_rt7=lm(as.numeric(rs_verbalAnalogies_RT_dfe)~as.numeric(dem.dob_age)+as.factor(dem.sex_at_birth)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.factor(dem.highest_education)+
              as.numeric(phq9.sum_score)+as.numeric(gad7.sum_score)+
              as.numeric(cfs.mental_subscale)+as.numeric(cfs.physical_subscale)+
              as.factor(dem.concerned_memory_group)+
              as.factor(time_window_covid19),data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit1_rt7)
### step AIC
fit2_rt7=stepAIC(fit1_rt7,direction = "backward",k=2)
summary(fit2_rt7)
fit2_rt7=lm(as.numeric(rs_verbalAnalogies_RT_dfe)~as.numeric(dem.dob_age)+relevel(as.factor(Diagnostic.Group), ref = "Control")+as.numeric(phq9.sum_score) +as.numeric(gad7.sum_score) ,data=data_multipleReg_Exclude_missing_data_variables_tasks_complete)
summary(fit2_rt7)
####### 
########
## End  of multiple regression models###
## checking the correlation between tasks and diognostic groups
data_tasks=data.frame(cbind(as.numeric(rs_manipulations2D_dfe),as.numeric(rs_TOL_dfe),as.numeric(rs_prospectiveMemoryWords_1_delayed_dfe),
as.numeric(rs_prospectiveMemoryWords_1_immediate_dfe),as.numeric(rs_spatialSpan_dfe),as.numeric(rs_targetDetection_dfe),
as.numeric(rs_verbalAnalogies_dfe),as.numeric(rs_manipulations2D_RT_dfe),as.numeric(rs_TOL_RT_dfe),as.numeric(rs_prospectiveMemoryWords_1_delayed_RT_dfe),
as.numeric(rs_prospectiveMemoryWords_1_immediate_RT_dfe), as.numeric(rs_spatialSpan_RT_dfe),as.numeric(rs_targetDetection_RT_dfe), as.numeric(rs_verbalAnalogies_RT_dfe)))
cor_mat(data_tasks)
library(caret)
library("heatmaply")
library(ggcorrplot)
win.graph()
cormat_1=cor(data_tasks)
ggcorrplot(cormat_1, 
           legend.title = "Pearson Correlation",
           lab = F,
           lab_size = 5,
           tl.cex = 7,
           tl.srt = 0,
           title = "Correlation between selected task scores")+
  theme(plot.title = element_text(hjust = 0.5, size=10),axis.text.x = element_text(angle = 0, hjust = 0.5),legend.title = element_text(size = 10))+
  ggplot2::labs(x = 'task scores', y = 'task scores') +ggplot2::theme(
    axis.title.x = element_text(angle = 0, color = 'grey20'),
    axis.title.y = element_text(angle = 90, color = 'grey20'))
###############################




#As far as I understand, there are 3 composite scores for cognitron: RT, accuracy and global.
#They are called "Component_rt", "Component_accuracy" and "Component_global" in the dataset.
summary(data_case_positiveControls$Component_rt)
sd(data_case_positiveControls$Component_rt)

summary(data_case_positiveControls$Component_accuracy)
sd(data_case_positiveControls$Component_accuracy)

summary(data_case_positiveControls$Composite_global)
sd(data_case_positiveControls$Composite_global)

######## End #############
