library(ggplot2)
covidcns_greta <- readRDS("C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/covidcns_greta_7July2022.rds")

write.csv(covidcns_greta, file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/covidcns_greta_7July2022.csv")

dim(covidcns_greta)
#[1]   623 3355

covidcns_cognitive_data <- readRDS("C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/covidcns_cognitive_data_7July2022.rds")

write.csv(covidcns_cognitive_data, file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/covidcns_cognitive_data_7July2022.csv")

dim(covidcns_cognitive_data)
#[1]   623 75
names(covidcns_cognitive_data)

####### Read data #####
### case definition all data
data_All=read.csv(file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/Final_Case_Definitions.csv",header=T)
dim(data_All) 
names(data_All)
table(data_All$Case.Control.Vaccine)

merge_data_All = merge(data_All,covidcns_greta,by=c("ID"))
dim(merge_data_All)
names(merge_data_All)
write.csv(merge_data_All,file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/data_7July2022/merge_data_All_7July22.csv")
table(merge_data_All$case_control_vaccine)
################
### data Covid positive cases and positive controls
data_case_positiveControls=subset(merge_data_All,(merge_data_All$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"|merge_data_All$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))
dim(data_case_positiveControls)
table(data_case_positiveControls$case_control_vaccine)

#### subset of cases 
data_case=subset(merge_data_All,(merge_data_All$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"))
dim(data_case)
### subset of positive controls 
data_positiveControls=subset(merge_data_All,(merge_data_All$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))
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


######################
## Age distribution
shapiro.test(data_case_positiveControls$dem.dob_age)
##W = 0.98654, p-value = 0.01145: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$dem.dob_age,data_case_positiveControls$case_control_vaccine,alpha=0.05)


### Composite_global
shapiro.test(data_case_positiveControls$Composite_global)
##W = 0.95092, p-value = 6.124e-08: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_global,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Composite_global
shapiro.test(data_case_positiveControls$Composite_global)
##W = 0.95092, p-value = 6.124e-08: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_global,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Composite_acc
shapiro.test(data_case_positiveControls$Composite_acc)
##W = 0.97388, p-value = 6.798e-05: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_acc,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### Composite_rt
shapiro.test(data_case_positiveControls$Composite_rt)
##W = 0.80261, p-value < 2.2e-16: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$Composite_rt,data_case_positiveControls$case_control_vaccine,alpha=0.05)


### PHQ9 sum score
shapiro.test(data_case_positiveControls$phq9.sum_score)
##W = 0.80261, p-value < 2.2e-16: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$phq9.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### GAD7 sum score
shapiro.test(data_case_positiveControls$gad7.sum_score)
##W = 0.80261, p-value < 2.2e-16: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$gad7.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

### PCL5 sum score
shapiro.test(data_case_positiveControls$pcl5.sum_score)
##W = 0.80261, p-value < 2.2e-16: rejeu6ct null hypothesis. Not normal data
#t.test(data_case_positiveControls$dem.dob_age~data_case_positiveControls$case_control_vaccine)
Mtest_continuous(data_case_positiveControls$pcl5.sum_score,data_case_positiveControls$case_control_vaccine,alpha=0.05)

## End of normality test ###
###############################
n_obs=dim(data_case_positiveControls)[1]
summary(data_case_positiveControls$dem.dob_age)
sd(data_case_positiveControls$dem.dob_age,na.rm=T)

## Gender distribution
summary(data_case_positiveControls$dem.sex_at_birth)
summary(data_case_positiveControls)/(n_obs)

##WHO Covid severity
summary(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19)
table(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)
covidcns_greta=data_case_positiveControls
dim(covidcns_greta)
covidcns_greta$WHO_Covid_severity_group=rep(NA,nrow(covidcns_greta))
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==0]="Uninfected"
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==1|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==2|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==3]="Ambulatory mild disease"
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==4|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==5]="Hospitalised: moderate"
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==6|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==7|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==8|covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==9]="Hospitalised: severe"
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==10]="Dead"
covidcns_greta$WHO_Covid_severity_group[covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric==-777]="Seen but not answered"
table(covidcns_greta$WHO_Covid_severity_group)
35/n_obs
table(covidcns_greta$WHO_Covid_severity_group)/ (n_obs-35)


##Pre-morbid Rockwell Clinical Frailty Score
summary(covidcns_greta$ncrf1_comorbid.clinical_frailty_scale)
table(covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric)


covidcns_greta$clinical_frailty_scale_group=rep(NA,nrow(covidcns_greta))
covidcns_greta$clinical_frailty_scale_group[covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==1|covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==2|covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==3]="Managing well"
covidcns_greta$clinical_frailty_scale_group[covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==4|covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==5]="Mild"
covidcns_greta$clinical_frailty_scale_group[covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==6|covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==7|covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==8]="Moderate-severe"
covidcns_greta$clinical_frailty_scale_group[covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==9]="Terminally ill "
covidcns_greta$clinical_frailty_scale_group[covidcns_greta$ncrf1_comorbid.clinical_frailty_scale_numeric==-777]="Seen but not answered"
table(covidcns_greta$clinical_frailty_scale_group)
35/n_obs
table(covidcns_greta$clinical_frailty_scale_group)/(n_obs-35)

##Timeframe of admission in 6 month windows starting in March 2020.
summary(covidcns_greta$ncrf1_admission.date_of_inpatient_admission) 

###define how many cases vs Controls were recruited in the first COVID-19 wave in every 6 months from March 2020
covidcns_greta$time_window_covid19=rep(NA,nrow(covidcns_greta))
covidcns_greta$time_window_covid19[covidcns_greta$ncrf1_admission.date_of_inpatient_admission>="2020-03-01 00:00:00" & covidcns_greta$ncrf1_admission.date_of_inpatient_admission<"2020-09-01 00:00:00"]=1
covidcns_greta$time_window_covid19[covidcns_greta$ncrf1_admission.date_of_inpatient_admission>="2020-09-01 00:00:00" & covidcns_greta$ncrf1_admission.date_of_inpatient_admission<"2021-03-01 00:00:00"]=2
covidcns_greta$time_window_covid19[covidcns_greta$ncrf1_admission.date_of_inpatient_admission>="2021-03-01 00:00:00" & covidcns_greta$ncrf1_admission.date_of_inpatient_admission<"2021-09-01 00:00:00"]=3
covidcns_greta$time_window_covid19[covidcns_greta$ncrf1_admission.date_of_inpatient_admission>="2021-09-01 00:00:00" & covidcns_greta$ncrf1_admission.date_of_inpatient_admission<"2022-03-01 00:00:00"]=4

summary(covidcns_greta$time_window_covid19)
table(covidcns_greta$time_window_covid19)
35/n_obs
table(covidcns_greta$time_window_covid19)/(n_obs-35)

#As far as I understand, there are 3 composite scores for cognitron: RT, accuracy and global.
#They are called "Component_rt", "Component_accuracy" and "Component_global" in the dataset.
summary(covidcns_greta$Component_rt)
sd(covidcns_greta$Component_rt)

summary(covidcns_greta$Component_accuracy)
sd(covidcns_greta$Component_accuracy)

summary(covidcns_greta$Composite_global)
sd(covidcns_greta$Composite_global)

######## End #############
