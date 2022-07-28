library(ggplot2)
covidcns_greta <- readRDS("C:/Users/tkuka80/Downloads/covidcns_greta.rds")
covidcns_greta <- readRDS("C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/Cognition_4May2022/covidcns_greta_vesion2.rds")

write.csv(covidcns_greta, file="C:/Users/tkuka80/Desktop/Liverpool_uni/Brain_injury_marker_data/COVID-CNS/covidcns_greta_23March22.csv")

dim(covidcns_greta)
#[1]   480 1353
unique(covidcns_greta$ID)
names(covidcns_greta[,1:900])
names(covidcns_greta[,901:1374])
covidcns_greta[,1]
head(covidcns_greta[,1:20])
head(covidcns_greta[,759:794])
table(covidcns_greta[,759])
summary(covidcns_greta[,759])
summary(covidcns_greta[,625])
table(covidcns_greta[,625])
covidcns_greta$rs_TOL
summary(as.numeric(covidcns_greta$rs_TOL))
summary(as.numeric(covidcns_greta$rs_prospectiveMemoryWords_1_immediate))
summary((covidcns_greta[,1354:1374]))

### create the variables 
attach(covidcns_greta)
##1. Cage
summary(covidcns_greta$cage.sum_score)
table(covidcns_greta$cage.sum_score)
summary(covidcns_greta$cage.investigate_numeric)
summary(covidcns_greta$cage.investigate)
### 2. Age: dem.dob_age
summary(covidcns_greta$dem.dob_age)
hist(covidcns_greta$dem.dob_age,breaks = seq(0,100,length.out = 20))
### 3 Gender
summary(covidcns_greta$dem.what_gender_do_you_identify_with)
table(covidcns_greta$dem.what_gender_do_you_identify_with)
summary(covidcns_greta$dem.what_gender_do_you_identify_with_numeric)
table(covidcns_greta$dem.what_gender_do_you_identify_with_numeric)
### 4. Qualification 
summary(covidcns_greta$dem.highest_education)
table(covidcns_greta$dem.highest_education_numeric)
summary(covidcns_greta$dem.highest_education_numeric)

### 5. Ethnicity
#1. White (1, 2, 3, 4)
#2. Black, African, Black British, or Caribbean (13, 14, 15). 
#3. Asian (9,10,11,12,16)
#4. Mixed (5,6,7,8) 
#5. Another ethnic group(17). 
covidcns_greta$Ethnicity=covidcns_greta$Ethnicity_numeric=rep (NA, nrow(covidcns_greta))
covidcns_greta$Ethnicity[covidcns_greta$dem.british_mixed_british_numeric==1|covidcns_greta$dem.irish_numeric==1|covidcns_greta$dem.northern_irish_numeric==1|covidcns_greta$dem.any_other_white_background_numeric==1]="White"
covidcns_greta$Ethnicity_numeric[covidcns_greta$dem.british_mixed_british_numeric==1|covidcns_greta$dem.irish_numeric==1|covidcns_greta$dem.northern_irish_numeric==1|covidcns_greta$dem.any_other_white_background_numeric==1]  =1                                                                  
covidcns_greta$Ethnicity[covidcns_greta$dem.caribbean_numeric==1|covidcns_greta$dem.african_numeric==1|covidcns_greta$dem.any_other_black_background_numeric==1]="Black background"
covidcns_greta$Ethnicity_numeric[covidcns_greta$dem.caribbean_numeric==1|covidcns_greta$dem.african_numeric==1|covidcns_greta$dem.any_other_black_background_numeric==1]  =2                                                                  
covidcns_greta$Ethnicity[covidcns_greta$dem.indian_or_british_indian_numeric==1|covidcns_greta$dem.pakistani_or_british_pakistani_numeric==1|covidcns_greta$dem.bangladeshi_or_british_bangladeshi_numeric==1|covidcns_greta$dem.any_other_asian_background_numeric==1|covidcns_greta$dem.chinese_numeric==1]="Asian"
covidcns_greta$Ethnicity_numeric[covidcns_greta$dem.indian_or_british_indian_numeric==1|covidcns_greta$dem.pakistani_or_british_pakistani_numeric==1|covidcns_greta$dem.bangladeshi_or_british_bangladeshi_numeric==1|covidcns_greta$dem.any_other_asian_background_numeric==1|covidcns_greta$dem.chinese_numeric==1]=3                                                                  
covidcns_greta$Ethnicity[covidcns_greta$dem.white_and_black_caribbean_numeric==1|covidcns_greta$dem.white_and_black_africa_numeric==1|covidcns_greta$dem.white_and_asian_numeric==1|covidcns_greta$dem.any_other_mixed_background_numeric==1]="Mixed"
covidcns_greta$Ethnicity_numeric[covidcns_greta$dem.white_and_black_caribbean_numeric==1|covidcns_greta$dem.white_and_black_africa_numeric==1|covidcns_greta$dem.white_and_asian_numeric==1|covidcns_greta$dem.any_other_mixed_background_numeric==1]=4                                                                  
covidcns_greta$Ethnicity[covidcns_greta$dem.any_other_ethnic_group_numeric==1]="Another ethnic group"
covidcns_greta$Ethnicity_numeric[covidcns_greta$dem.any_other_ethnic_group_numeric==1]=5                                                                  

table(covidcns_greta$dem.any_other_ethnic_group)
table(covidcns_greta$dem.any_other_ethnic_group_numeric)
#table(covidcns_greta$dem.british_mixed_british_numeric)
#table(covidcns_greta$dem.irish_numeric)
table(covidcns_greta$dem.other)
table(covidcns_greta$dem.othertext.txt)
                                                         
summary(covidcns_greta$Ethnicity)
table(covidcns_greta$Ethnicity)
summary(covidcns_greta$Ethnicity_numeric)
table(covidcns_greta$Ethnicity_numeric)

## 6. First_Language 
table(covidcns_greta$dem.what_is_your_first_language)
table(covidcns_greta$dem.what_is_your_first_language_numeric)
summary(covidcns_greta$dem.what_is_your_first_language_numeric)
table(covidcns_greta$dem.is_english_your_first_language)
table(covidcns_greta$dem.is_english_your_first_language_numeric)



### 7. Memory
table(covidcns_greta$dem.affects_concerned_live_memory.1)
table(covidcns_greta$dem.affects_concerned_live_memory.1_numeric)
summary(covidcns_greta$dem.affects_concerned_live_memory.1_numeric)

###  8a. Pre_COVID_Memory 
table(covidcns_greta$dem.affects_concerned_live_memory.2)
table(covidcns_greta$dem.affects_concerned_live_memory.2_numeric)
summary(covidcns_greta$dem.affects_concerned_live_memory.2_numeric)

### 8b. After_covid_memory
table(covidcns_greta$dem.affects_concerned_live_memory)
table(covidcns_greta$dem.affects_concerned_live_memory_numeric)
summary(covidcns_greta$dem.affects_concerned_live_memory_numeric)

#### 9.Worse_Memory 
table(covidcns_greta$dem.has_your_memory_got_progressively_worse)
table(covidcns_greta$dem.has_your_memory_got_progressively_worse_numeric)
summary(covidcns_greta$dem.has_your_memory_got_progressively_worse_numeric)

### 10. Education 

### 11. Fatigue_score
#table(covidcns_greta$nis_outp.fatigue_numeric)
## Fatigue total core 0-33
table(covidcns_greta$cfs.sum_score)
summary(covidcns_greta$cfs.sum_score)
#barplot(table(covidcns_greta$cfs.sum_score))
#hist(covidcns_greta$cfs.sum_score,breaks = seq(-1,33,length.out = 34))
## bar plot 
sum_scale<- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)
length(sum_scale)
value <- c(0,0,0,1,0,0,0,0,  3,  3,  5, 35,  9, 11,  9, 16,  6, 11, 19,  8,  8,  7,  3,  5,  4,  4,  6,  4,  4,  2,  1,  2,0,0)
length(value)
data <- data.frame(sum_scale,value)
ggplot(data, aes( y=value, x=sum_scale)) + 
  geom_bar( stat="identity")+labs(y= "Frequency of number of patients", x = "Scores")+ theme(legend.position="bottom")+theme_minimal()

### Physical Fatigue 7 items (1-21 ; 11.1 - 11.7) 
table(covidcns_greta$cfs.physical_subscale) 
summary(covidcns_greta$cfs.physical_subscale) 

#hist(covidcns_greta$cfs.physical_subscale,breaks = seq(0,21,length.out = 22))
## bar plot 
Physical_subscale<- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
length(Physical_subscale)
value <- c(0,0,1,  1,  3,  3,  4, 40, 13, 13,  9, 20,  8, 13, 19,  8,  6,  3,  6,  5,  7,  5)
length(value)
data <- data.frame(Physical_subscale,value)
ggplot(data, aes( y=value, x=Physical_subscale)) + 
  geom_bar( stat="identity")+labs(y= "Frequency of number of patients", x = "Scores")+ theme(legend.position="bottom")+theme_minimal()

### Mental Fatigue 4 items (1-12; 11.8 - 11.11)
table(covidcns_greta$cfs.mental_subscale) 
summary(covidcns_greta$cfs.mental_subscale)                                                          

#hist(covidcns_greta$cfs.mental_subscale,breaks = seq(-1,13,length.out = 13))

## bar plot 
#table(covidcns_greta$cfs.mental_subscale)                                                          
#barplot(table(covidcns_greta$cfs.mental_subscale) )

# create a dataset
mental_subscale<- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
length(mental_subscale)
value <- c(1, 1,  1,  9, 80, 26, 28, 13, 13,  9,  4,  2,0)
length(value)
data <- data.frame(mental_subscale,value)
ggplot(data, aes( y=value, x=mental_subscale)) + 
  geom_bar( stat="identity")+labs(y= "Frequency of number of patients", x = "Scores")+ theme(legend.position="bottom")+theme_minimal()


### 11.1 Tiredness
table(covidcns_greta$cfs.tiredness_problems_month)     
table(covidcns_greta$cfs.tiredness_problems_month_numeric)                                                 
summary(covidcns_greta$cfs.tiredness_problems_month_numeric)                                                 

covidcns_greta$Tiredness_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Tiredness_binary[covidcns_greta$cfs.tiredness_problems_month_numeric==0|covidcns_greta$cfs.tiredness_problems_month_numeric==1]=0
covidcns_greta$Tiredness_binary[covidcns_greta$cfs.tiredness_problems_month_numeric==2|covidcns_greta$cfs.tiredness_problems_month_numeric==3]=1
table(covidcns_greta$Tiredness_binary)
summary(covidcns_greta$Tiredness_binary)

### 11.2 Rest
table(covidcns_greta$cfs.rest_month)     
table(covidcns_greta$cfs.rest_month_numeric)                                                 
summary(covidcns_greta$cfs.rest_month_numeric)                                                 

covidcns_greta$Rest_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Rest_binary[covidcns_greta$cfs.rest_month_numeric==0|covidcns_greta$cfs.rest_month_numeric==1]=0
covidcns_greta$Rest_binary[covidcns_greta$cfs.rest_month_numeric==2|covidcns_greta$cfs.rest_month_numeric==3]=1
table(covidcns_greta$Rest_binary)
summary(covidcns_greta$Rest_binary)
 
### 11.3 Sleep_drowsy
table(covidcns_greta$cfs.feel_sleepy_drowsy_month)     
table(covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric)                                                 
summary(covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric)                                                 

covidcns_greta$Sleep_drowsy_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Sleep_drowsy_binary[covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric==0|covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric==1]=0
covidcns_greta$Sleep_drowsy_binary[covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric==2|covidcns_greta$cfs.feel_sleepy_drowsy_month_numeric==3]=1
table(covidcns_greta$Sleep_drowsy_binary)
summary(covidcns_greta$Sleep_drowsy_binary)

### 11.4 Problems

table(covidcns_greta$cfs.problems_starting_things_month)     
table(covidcns_greta$cfs.problems_starting_things_month_numeric)                                                 
summary(covidcns_greta$cfs.problems_starting_things_month_numeric)                                                 

covidcns_greta$Problems_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Problems_binary[covidcns_greta$cfs.problems_starting_things_month_numeric==0|covidcns_greta$cfs.problems_starting_things_month_numeric==1]=0
covidcns_greta$Problems_binary[covidcns_greta$cfs.problems_starting_things_month_numeric==2|covidcns_greta$cfs.problems_starting_things_month_numeric==3]=1
table(covidcns_greta$Problems_binary)
summary(covidcns_greta$Problems_binary)

### 11.5 Lack_energy 

table(covidcns_greta$cfs.lack_energy_month)     
table(covidcns_greta$cfs.lack_energy_month_numeric)                                                 
summary(covidcns_greta$cfs.lack_energy_month_numeric)                                                 

covidcns_greta$Lack_energy_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Lack_energy_binary[covidcns_greta$cfs.lack_energy_month_numeric==0|covidcns_greta$cfs.lack_energy_month_numeric==1]=0
covidcns_greta$Lack_energy_binary[covidcns_greta$cfs.lack_energy_month_numeric==2|covidcns_greta$cfs.lack_energy_month_numeric==3]=1
table(covidcns_greta$Lack_energy_binary)
summary(covidcns_greta$Lack_energy_binary)

### 11.6 Strength_muscle

table(covidcns_greta$cfs.muscles_strength_month)     
table(covidcns_greta$cfs.muscles_strength_month_numeric)                                                 
summary(covidcns_greta$cfs.muscles_strength_month_numeric)                                                 

covidcns_greta$Strength_muscle_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Strength_muscle_binary[covidcns_greta$cfs.muscles_strength_month_numeric==0|covidcns_greta$cfs.muscles_strength_month_numeric==1]=0
covidcns_greta$Strength_muscle_binary[covidcns_greta$cfs.muscles_strength_month_numeric==2|covidcns_greta$cfs.muscles_strength_month_numeric==3]=1
table(covidcns_greta$Strength_muscle_binary)
summary(covidcns_greta$Strength_muscle_binary)

### 11.7 Weak

table(covidcns_greta$cfs.feel_weak_week)     
table(covidcns_greta$cfs.feel_weak_week_numeric)                                                 
summary(covidcns_greta$cfs.feel_weak_week_numeric)                                                 

covidcns_greta$Weak_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Weak_binary[covidcns_greta$cfs.feel_weak_week_numeric==0|covidcns_greta$cfs.feel_weak_week_numeric==1]=0
covidcns_greta$Weak_binary[covidcns_greta$cfs.feel_weak_week_numeric==2|covidcns_greta$cfs.feel_weak_week_numeric==3]=1
table(covidcns_greta$Weak_binary)
summary(covidcns_greta$Weak_binary)

### 11.8 Difficult_concentratiion

table(covidcns_greta$cfs.difficulty_concentrating_month)     
table(covidcns_greta$cfs.difficulty_concentrating_month_numeric)                                                 
summary(covidcns_greta$cfs.difficulty_concentrating_month_numeric)                                                 

covidcns_greta$Difficult_concentratiion_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Difficult_concentratiion_binary[covidcns_greta$cfs.difficulty_concentrating_month_numeric==0|covidcns_greta$cfs.difficulty_concentrating_month_numeric==1]=0
covidcns_greta$Difficult_concentratiion_binary[covidcns_greta$cfs.difficulty_concentrating_month_numeric==2|covidcns_greta$cfs.difficulty_concentrating_month_numeric==3]=1
table(covidcns_greta$Difficult_concentratiion_binary)
summary(covidcns_greta$Difficult_concentratiion_binary)

### 11.9 Slips_tongue

table(covidcns_greta$cfs.slips_tongue_speaking_month)     
table(covidcns_greta$cfs.slips_tongue_speaking_month_numeric)                                                 
summary(covidcns_greta$cfs.slips_tongue_speaking_month_numeric)                                                 

covidcns_greta$Slips_tongue_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Slips_tongue_binary[covidcns_greta$cfs.slips_tongue_speaking_month_numeric==0|covidcns_greta$cfs.slips_tongue_speaking_month_numeric==1]=0
covidcns_greta$Slips_tongue_binary[covidcns_greta$cfs.slips_tongue_speaking_month_numeric==2|covidcns_greta$cfs.slips_tongue_speaking_month_numeric==3]=1
table(covidcns_greta$Slips_tongue_binary)
summary(covidcns_greta$Slips_tongue_binary)

### 11.10 Difficult_correct_word

table(covidcns_greta$cfs.correct_word_difficult_find)     
table(covidcns_greta$cfs.correct_word_difficult_find_numeric)                                                 
summary(covidcns_greta$cfs.correct_word_difficult_find_numeric)                                                 

covidcns_greta$Difficult_correct_word_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Difficult_correct_word_binary[covidcns_greta$cfs.correct_word_difficult_find_numeric==0|covidcns_greta$cfs.correct_word_difficult_find_numeric==1]=0
covidcns_greta$Difficult_correct_word_binary[covidcns_greta$cfs.correct_word_difficult_find_numeric==2|covidcns_greta$cfs.correct_word_difficult_find_numeric==3]=1
table(covidcns_greta$Difficult_correct_word_binary)
summary(covidcns_greta$Difficult_correct_word_binary)

### 11.11 Memory_last_month
                                                    
table(covidcns_greta$cfs.memory_month)     
table(covidcns_greta$cfs.memory_month_numeric)                                                 
summary(covidcns_greta$cfs.memory_month_numeric)                                                 

covidcns_greta$Memory_last_month_binary=rep(NA, nrow(covidcns_greta))
covidcns_greta$Memory_last_month_binary[covidcns_greta$cfs.memory_month_numeric==0|covidcns_greta$cfs.memory_month_numeric==1]=0
covidcns_greta$Memory_last_month_binary[covidcns_greta$cfs.memory_month_numeric==2|covidcns_greta$cfs.memory_month_numeric==3]=1
table(covidcns_greta$Memory_last_month_binary)
summary(covidcns_greta$Memory_last_month_binary)

## Fatigue_score_binary 
covidcns_greta$Fatigue_score_binary=rep(NA, nrow(covidcns_greta))
for (i in 1:nrow(covidcns_greta)){
if (is.na(covidcns_greta$Tiredness_binary[i])&is.na(covidcns_greta$Rest_binary[i])& is.na(covidcns_greta$Sleep_drowsy_binary[i])&is.na(covidcns_greta$Problems_binary[i])&is.na(covidcns_greta$Lack_energy_binary[i])&is.na(covidcns_greta$Strength_muscle_binary[i])&is.na(covidcns_greta$Weak_binary[i])&is.na(covidcns_greta$Difficult_concentratiion_binary[i])&is.na(covidcns_greta$Slips_tongue_binary[i])&is.na(covidcns_greta$Difficult_correct_word_binary[i])&is.na(covidcns_greta$Memory_last_month_binary[i])) {covidcns_greta$Fatigue_score_binary[i]=NA} else {
if (sum(covidcns_greta$Tiredness_binary[i],covidcns_greta$Rest_binary[i],covidcns_greta$Sleep_drowsy_binary[i],covidcns_greta$Problems_binary[i],covidcns_greta$Lack_energy_binary[i],covidcns_greta$Strength_muscle_binary[i],covidcns_greta$Weak_binary[i],covidcns_greta$Difficult_concentratiion_binary[i],covidcns_greta$Slips_tongue_binary[i],covidcns_greta$Difficult_correct_word_binary[i],covidcns_greta$Memory_last_month_binary[i],na.rm=T)<4) {covidcns_greta$Fatigue_score_binary[i]=0} else {covidcns_greta$Fatigue_score_binary[i]=1}}
}
table(covidcns_greta$Fatigue_score_binary)
summary(covidcns_greta$Fatigue_score_binary)

### 12 GAD-7
table(covidcns_greta$gad7.feeling_nervous_anxious_or_on_edge_numeric)                         
table(covidcns_greta$gad7.control_worrying_stop_numeric)                                           
table(covidcns_greta$gad7.worrying_too_much_about_different_things_numeric)                      
table(covidcns_greta$gad7.trouble_relaxing_numeric)                                               
table(covidcns_greta$gad7.sit_restless_hard_numeric)                                               
table(covidcns_greta$gad7.becoming_easily_annoyed_or_irritable_numeric)                            
table(covidcns_greta$gad7.feeling_afraid_awful_happen_numeric)                                     
table(covidcns_greta$gad7.sum_score) 
summary(covidcns_greta$gad7.sum_score)
### barplot
gad7_sum_score<- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
length(gad7_sum_score)
value <- c(52, 17, 15, 10, 13,  8,  7,  8,  5,  3,  6,  7,  4,  3,  1,  5,  4,  1,  1,  1,  1,  1)
length(value)
data <- data.frame(gad7_sum_score,value)
ggplot(data, aes( y=value, x=gad7_sum_score)) + 
  geom_bar( stat="identity")+labs(y= "Frequency of number of patients", x = "Scores")+ theme(legend.position="bottom")+theme_minimal()

table(covidcns_greta$gad7.binary_clinical_numeric)  
summary(covidcns_greta$gad7.binary_clinical_numeric)                                              

table(covidcns_greta$gad7.binary_clinical)  
## None: 0-4; Mild: 5-9; Moderate 10-14;   Severe 15-21
table(covidcns_greta$gad7.severity_thresholds)  
table(covidcns_greta$gad7.severity_thresholds_numeric)                                            
summary(covidcns_greta$gad7.severity_thresholds_numeric)                                            

## 13	Similar_experiences
table(covidcns_greta$dem.diagnosed_covid19_experienced_similar)
table(covidcns_greta$dem.diagnosed_covid19_experienced_similar_numeric)  
summary(covidcns_greta$dem.diagnosed_covid19_experienced_similar_numeric)  

## 14	Employment_status_pre_COVID
table(covidcns_greta$impact.fulltime_employed) 
table(covidcns_greta$impact.fulltime_employed_numeric)
table(covidcns_greta$impact.unemployed)  
table(covidcns_greta$impact.unemployed_numeric)
table(covidcns_greta$impact.stayathome_parent_or_carer_numeric)                                    
table(covidcns_greta$impact.contract_or_freelance_work_numeric)                                    
table(covidcns_greta$impact.receiving_state_income_numeric)                                        
table(covidcns_greta$impact.student__numeric)                                                      
table(covidcns_greta$impact.prefer_not_to_say_numeric)                                             
table(covidcns_greta$impact.parttime_employed_numeric)                                             
table(covidcns_greta$impact.zerohours_contract_numeric)                                            
table(covidcns_greta$impact.selfemployed_numeric)                                                  
table(covidcns_greta$impact.small_buisness_owner_numeric)                                          
table(covidcns_greta$impact.retired_numeric)                                                       
table(covidcns_greta$impact.student_.1_numeric) 

####  re- group this variable
#Employed ( 1, 4, 8, 9, 10, 11,)
#Unemployed: (2, 3, 5)
#Retired (12)
#Student (6, 13) 
#Prefer not to say  (7)

covidcns_greta$Employment_status_pre_COVID=covidcns_greta$Employment_status_pre_COVID_numeric=rep(NA, nrow(covidcns_greta))
covidcns_greta$Employment_status_pre_COVID[covidcns_greta$impact.fulltime_employed_numeric==1|covidcns_greta$impact.contract_or_freelance_work_numeric==1|covidcns_greta$impact.parttime_employed_numeric==1|covidcns_greta$impact.zerohours_contract_numeric==1|covidcns_greta$impact.selfemployed_numeric==1|covidcns_greta$impact.small_buisness_owner_numeric==1]="Employed"
covidcns_greta$Employment_status_pre_COVID[covidcns_greta$impact.unemployed_numeric==1|covidcns_greta$impact.stayathome_parent_or_carer_numeric==1|covidcns_greta$impact.receiving_state_income_numeric==1]="Unemployed"
covidcns_greta$Employment_status_pre_COVID[covidcns_greta$impact.retired_numeric==1]="Retired"
covidcns_greta$Employment_status_pre_COVID[covidcns_greta$impact.student__numeric==1|covidcns_greta$impact.student_.1_numeric==1]="Student"
covidcns_greta$Employment_status_pre_COVID[covidcns_greta$impact.prefer_not_to_say_numeric==1]="Prefer not to say"
summary(covidcns_greta$Employment_status_pre_COVID)
table(covidcns_greta$Employment_status_pre_COVID)

covidcns_greta$Employment_status_pre_COVID_numeric[covidcns_greta$impact.fulltime_employed_numeric==1|covidcns_greta$impact.contract_or_freelance_work_numeric==1|covidcns_greta$impact.parttime_employed_numeric==1|covidcns_greta$impact.zerohours_contract_numeric==1|covidcns_greta$impact.selfemployed_numeric==1|covidcns_greta$impact.small_buisness_owner_numeric==1]=1
covidcns_greta$Employment_status_pre_COVID_numeric[covidcns_greta$impact.unemployed_numeric==1|covidcns_greta$impact.stayathome_parent_or_carer_numeric==1|covidcns_greta$impact.receiving_state_income_numeric==1]=2
covidcns_greta$Employment_status_pre_COVID_numeric[covidcns_greta$impact.retired_numeric==1]=3
covidcns_greta$Employment_status_pre_COVID_numeric[covidcns_greta$impact.student__numeric==1|covidcns_greta$impact.student_.1_numeric==1]=4
covidcns_greta$Employment_status_pre_COVID_numeric[covidcns_greta$impact.prefer_not_to_say_numeric==1]=5
summary(covidcns_greta$Employment_status_pre_COVID_numeric)
table(covidcns_greta$Employment_status_pre_COVID_numeric)

### 15 Employment_status_changed
table(covidcns_greta$impact.my_employment_status_has_not_changed_numeric)                          
table(covidcns_greta$impact.become_unemployed_numeric)                                             
table(covidcns_greta$impact.become_employed_numeric)                                               
table(covidcns_greta$impact.reduction_in_hours_numeric)                                            
table(covidcns_greta$impact.increased_hours_numeric)                                               
table(covidcns_greta$impact.benefits_increased_numeric)                                           
table(covidcns_greta$impact.furloughed_or_paid_leave__numeric)                                     
table(covidcns_greta$impact.paid_leave_furloughed_numeric)                                         
table(covidcns_greta$impact.other_numeric) 
table(covidcns_greta$impact.other)
table(covidcns_greta$impact.changes_in_duties_or_responsibilities_numeric)                         
table(covidcns_greta$impact.reduction_in_salary_numeric)                                           
table(covidcns_greta$impact.increased_salary_numeric)                                              
table(covidcns_greta$impact.benefits_decreased_numeric)                                            
table(covidcns_greta$impact.furloughed_or_paid_leave_.1_numeric)                                   
table(covidcns_greta$impact.taking_unpaid_leave_numeric)                                           
table(covidcns_greta$impact.prefer_not_to_say.1_numeric)                                           

#-	Positive or neutral change : 1, 3,5,6,11,13
#-	Negative change : 2,4, 7,8,12,14,15,16
#-	Other 9 (check text), not able to find the text for other
#-	Prefer not to say : 17 ??
covidcns_greta$Employment_status_changed=rep(NA, nrow(covidcns_greta))
covidcns_greta$Employment_status_changed[covidcns_greta$impact.my_employment_status_has_not_changed_numeric==1|covidcns_greta$impact.become_employed_numeric==1|covidcns_greta$impact.increased_hours_numeric==1|covidcns_greta$impact.benefits_increased_numeric==1|covidcns_greta$impact.changes_in_duties_or_responsibilities_numeric==1|covidcns_greta$impact.increased_salary_numeric==1]= "Positive or neutral change"
covidcns_greta$Employment_status_changed[covidcns_greta$impact.become_unemployed_numeric==1|covidcns_greta$impact.reduction_in_hours_numeric==1|covidcns_greta$impact.furloughed_or_paid_leave__numeric==1|covidcns_greta$impact.paid_leave_furloughed_numeric==1|covidcns_greta$impact.reduction_in_salary_numeric==1|covidcns_greta$impact.benefits_decreased_numeric==1|covidcns_greta$impact.furloughed_or_paid_leave_.1_numeric==1|covidcns_greta$impact.taking_unpaid_leave_numeric==1]= "Negative change"
covidcns_greta$Employment_status_changed[covidcns_greta$impact.other_numeric==1]= "Other"
covidcns_greta$Employment_status_changed[covidcns_greta$impact.prefer_not_to_say.1_numeric==1]= "Prefer not to say"
summary(covidcns_greta$Employment_status_changed)
table(covidcns_greta$Employment_status_changed)
covidcns_greta$Employment_status_changed_numeric=rep(NA, nrow(covidcns_greta))
covidcns_greta$Employment_status_changed_numeric[covidcns_greta$impact.my_employment_status_has_not_changed_numeric==1|covidcns_greta$impact.become_employed_numeric==1|covidcns_greta$impact.increased_hours_numeric==1|covidcns_greta$impact.benefits_increased_numeric==1|covidcns_greta$impact.changes_in_duties_or_responsibilities_numeric==1|covidcns_greta$impact.increased_salary_numeric==1]= 1
covidcns_greta$Employment_status_changed_numeric[covidcns_greta$impact.become_unemployed_numeric==1|covidcns_greta$impact.reduction_in_hours_numeric==1|covidcns_greta$impact.furloughed_or_paid_leave__numeric==1|covidcns_greta$impact.paid_leave_furloughed_numeric==1|covidcns_greta$impact.reduction_in_salary_numeric==1|covidcns_greta$impact.benefits_decreased_numeric==1|covidcns_greta$impact.furloughed_or_paid_leave_.1_numeric==1|covidcns_greta$impact.taking_unpaid_leave_numeric==1]= 2
covidcns_greta$Employment_status_changed_numeric[covidcns_greta$impact.other_numeric==1]= 3
covidcns_greta$Employment_status_changed_numeric[covidcns_greta$impact.prefer_not_to_say.1_numeric==1]= 4
summary(covidcns_greta$Employment_status_changed_numeric)
table(covidcns_greta$Employment_status_changed_numeric)
## 16 Risk_of_losing_job 
table(covidcns_greta$impact.losing_skip_risk_prefer) 
table(covidcns_greta$impact.losing_skip_risk_prefer_numeric)
summary(covidcns_greta$impact.losing_skip_risk_prefer_numeric)

## 17 Symptoms_impacted
table(covidcns_greta$impact.symptoms_impacted_carry_employment) 
table(covidcns_greta$impact.symptoms_impacted_carry_employment_numeric) 
summary(covidcns_greta$impact.symptoms_impacted_carry_employment_numeric) 

table(covidcns_greta$impact.employment_status_changed_contracting)
table(covidcns_greta$impact.employment_status_changed_contracting_numeric)
summary(covidcns_greta$impact.employment_status_changed_contracting_numeric)

## 18 Mental_health
table(covidcns_greta$mhd.depression_numeric)  
summary(covidcns_greta$mhd.depression_numeric)
table(covidcns_greta$mhd.depression)                                                               
table(covidcns_greta$mhd.postnatalantenatal_depression_numeric)                                    
table(covidcns_greta$mhd.postnatalantenatal_depression)                                            
table(covidcns_greta$mhd.pmdd_numeric)                                                            
table(covidcns_greta$mhd.pmdd)                                                                     
table(covidcns_greta$mhd.mania_hypomania_bipolar_or_manicdepression_numeric)                       
table(covidcns_greta$mhd.mania_hypomania_bipolar_or_manicdepression)                               
table(covidcns_greta$mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric)                   
table(covidcns_greta$mhd.anxiety_nerves_or_generalised_anxiety_disorder)                           
table(covidcns_greta$mhd.social_anxiety_or_social_phobia_numeric)                                  
table(covidcns_greta$mhd.social_anxiety_or_social_phobia)                                         
table(covidcns_greta$mhd.specific_phobia_numeric)                                                  
table(covidcns_greta$mhd.specific_phobia)                                                          
table(covidcns_greta$mhd.agoraphobia_numeric)                                                      
table(covidcns_greta$mhd.agoraphobia)                                                              
table(covidcns_greta$mhd.panic_attacks_numeric)                                                    
table(covidcns_greta$mhd.panic_attacks)                                                            
table(covidcns_greta$mhd.panic_disorder_numeric)                                                   
table(covidcns_greta$mhd.panic_disorder)                                                           
table(covidcns_greta$mhd.ptsd_numeric)                                                             
table(covidcns_greta$mhd.ptsd)                                                                     
table(covidcns_greta$mhd.obsessivecompulsive_disorder__numeric)                                    
table(covidcns_greta$mhd.obsessivecompulsive_disorder_)                                            
table(covidcns_greta$mhd.bdd_numeric)                                                              
table(covidcns_greta$mhd.bdd)                                                                      
table(covidcns_greta$mhd.other_ocd_numeric)                                                        
table(covidcns_greta$mhd.other_ocd)                                                                
table(covidcns_greta$mhd.none_of_the_above_numeric)                                                
table(covidcns_greta$mhd.none_of_the_above)                                                        
table(covidcns_greta$mhd.dont_know_numeric)                                                        
table(covidcns_greta$mhd.dont_know)                                                                
table(covidcns_greta$mhd.prefer_not_to_answer_numeric)                                             
table(covidcns_greta$mhd.prefer_not_to_answer)                                                     
table(covidcns_greta$mhd.mental_health_numeric) 
summary(covidcns_greta$mhd.mental_health_numeric)
table(covidcns_greta$mhd.mental_health)                                                            

#Depression (1, 2)
#Anxiety (4, 5, 6, 7, 8, 9, 14) 
#Other (3, 11, 10, 12, 13)
#0 - None 
covidcns_greta$Mental_health=rep(NA, nrow(covidcns_greta))
covidcns_greta$Mental_health[covidcns_greta$mhd.depression_numeric==1|covidcns_greta$mhd.postnatalantenatal_depression_numeric==1|covidcns_greta$mhd.mania_hypomania_bipolar_or_manicdepression_numeric==1]="Depression"
covidcns_greta$Mental_health[covidcns_greta$mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric==1|covidcns_greta$mhd.social_anxiety_or_social_phobia_numeric==1|covidcns_greta$mhd.specific_phobia_numeric==1|covidcns_greta$mhd.agoraphobia_numeric==1|covidcns_greta$mhd.panic_attacks_numeric==1|covidcns_greta$mhd.panic_disorder_numeric==1|covidcns_greta$mhd.ptsd_numeric==1]= "Anxiety"
covidcns_greta$Mental_health[covidcns_greta$mhd.pmdd_numeric==1|covidcns_greta$mhd.obsessivecompulsive_disorder__numeric==1|covidcns_greta$mhd.bdd_numeric==1|covidcns_greta$mhd.other_ocd_numeric==1]= "Other"
covidcns_greta$Mental_health[covidcns_greta$mhd.none_of_the_above_numeric==1]= "None"
covidcns_greta$Mental_health[covidcns_greta$mhd.prefer_not_to_answer_numeric==1]= "Prefer not to say"
summary(covidcns_greta$Mental_health)
table(covidcns_greta$Mental_health)

### 19 Neurodevelopmental_disorders
## Not able to find the corresponding variables in the data set

#### 20 Diagnostic_Major_depression
table(covidcns_greta$phq9.dead_hurting_thoughts)                                                  
table(covidcns_greta$phq9.feeling_bad_failure_family)                                              
table(covidcns_greta$phq9.feeling_down_depressed_or_hopeless)                                      
table(covidcns_greta$phq9.feeling_tired_or_having_little_energy)                                  
table(covidcns_greta$phq9.little_interest_or_pleasure_in_doing_things)                             
table(covidcns_greta$phq9.moving_fidgety_noticed_opposite)                                         
table(covidcns_greta$phq9.poor_appetite_or_overeating)                                            
table(covidcns_greta$phq9.staying_asleep_sleeping_trouble)                                         
table(covidcns_greta$phq9.trouble_concentrating_reading_newspaper)                                 

table(covidcns_greta$phq9.dead_hurting_thoughts_numeric)                                                  
table(covidcns_greta$phq9.feeling_bad_failure_family_numeric)                                              
table(covidcns_greta$phq9.feeling_down_depressed_or_hopeless_numeric)                                      
table(covidcns_greta$phq9.feeling_tired_or_having_little_energy_numeric)                                  
table(covidcns_greta$phq9.little_interest_or_pleasure_in_doing_things_numeric)                             
table(covidcns_greta$phq9.moving_fidgety_noticed_opposite_numeric)                                         
table(covidcns_greta$phq9.poor_appetite_or_overeating_numeric)                                            
table(covidcns_greta$phq9.staying_asleep_sleeping_trouble_numeric)                                         
table(covidcns_greta$phq9.trouble_concentrating_reading_newspaper_numeric) 

table(covidcns_greta$phq9.sum_score)
summary(covidcns_greta$phq9.sum_score)   

## bar plot 
sum_scale<- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
length(sum_scale)
value <- c(26,  9, 15, 18, 14,  8,  9,  6,  9, 12,  6,  4,  6,  8,  4,  5,  2,  2,  0,1,  1,  1,  1, 0, 4,  1,  1,0 )
length(value)
data <- data.frame(sum_scale,value)
ggplot(data, aes( y=value, x=sum_scale)) + 
  geom_bar( stat="identity")+labs(y= "Frequency of number of patients", x = "Scores")+ theme(legend.position="bottom")+theme_minimal()



#### binary 
#Major_depression (1)
#If 5 or more of 9 criteria  should have 2 or 3
#and 
#either 20.1 or 20.2 should be 5 of those criteria
#one of those 5 criteria might be 20.9 should have 1 or 2 or 3 

#Other_depression (0)
#If 2 or 3 or 4 of 9 criteria should have 2 or 3
#and 
#either 20.1 or 20.2 should be 5 of those criteria
#one of those 5 criteria might be 20.9 should have 1 or 2 or 3 
#####################################################################
### define 9 criteria as binary
covidcns_greta$phq9_1_binary=covidcns_greta$phq9_2_binary=covidcns_greta$phq9_3_binary=covidcns_greta$phq9_4_binary=covidcns_greta$phq9_5_binary=covidcns_greta$phq9_6_binary=covidcns_greta$phq9_7_binary=covidcns_greta$phq9_8_binary=covidcns_greta$phq9_9_binary=rep(NA,nrow(covidcns_greta))
table(covidcns_greta$phq9.dead_hurting_thoughts_numeric) 
covidcns_greta$phq9_1_binary[covidcns_greta$phq9.dead_hurting_thoughts_numeric==2 |covidcns_greta$phq9.dead_hurting_thoughts_numeric==3]=1
covidcns_greta$phq9_1_binary[covidcns_greta$phq9.dead_hurting_thoughts_numeric==0 |covidcns_greta$phq9.dead_hurting_thoughts_numeric==1]=0
table(covidcns_greta$phq9_1_binary)

table(covidcns_greta$phq9.feeling_bad_failure_family_numeric)
covidcns_greta$phq9_6_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_6_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_6_binary)

table(covidcns_greta$phq9.feeling_down_depressed_or_hopeless_numeric)
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.feeling_tired_or_having_little_energy_numeric) 
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.little_interest_or_pleasure_in_doing_things_numeric)  
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.moving_fidgety_noticed_opposite_numeric)  
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.poor_appetite_or_overeating_numeric)
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.staying_asleep_sleeping_trouble_numeric)
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

table(covidcns_greta$phq9.trouble_concentrating_reading_newspaper_numeric) 
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==2 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==3]=1
covidcns_greta$phq9_2_binary[covidcns_greta$phq9.feeling_bad_failure_family_numeric==0 |covidcns_greta$phq9.feeling_bad_failure_family_numeric==1]=0
table(covidcns_greta$phq9_2_binary)

## this is from data base
table(covidcns_greta$phq9.binary_depression)                                                       
table(covidcns_greta$phq9.binary_depression_numeric)
summary(covidcns_greta$phq9.binary_depression_numeric)                                               

## Cut-off scores 
#None <5
#Mild 5-9
#Moderate 10 -14
#Moderately severe  15-19
#Server ??? 20 
covidcns_greta$phq9_depression_stage=covidcns_greta$phq9_depression_stage_numeric=rep(NA,nrow(covidcns_greta))
covidcns_greta$phq9_depression_stage[covidcns_greta$phq9.sum_score<5]="None"
covidcns_greta$phq9_depression_stage_numeric[covidcns_greta$phq9.sum_score<5]=0
covidcns_greta$phq9_depression_stage[covidcns_greta$phq9.sum_score>=5 & covidcns_greta$phq9.sum_score<10]="Mild"
covidcns_greta$phq9_depression_stage_numeric[covidcns_greta$phq9.sum_score>=5 & covidcns_greta$phq9.sum_score<10]=1
covidcns_greta$phq9_depression_stage[covidcns_greta$phq9.sum_score>=10 & covidcns_greta$phq9.sum_score<15]="Moderate"
covidcns_greta$phq9_depression_stage_numeric[covidcns_greta$phq9.sum_score>=10 & covidcns_greta$phq9.sum_score<15]=2
covidcns_greta$phq9_depression_stage[covidcns_greta$phq9.sum_score>=15 & covidcns_greta$phq9.sum_score<20]="Moderately severe"
covidcns_greta$phq9_depression_stage_numeric[covidcns_greta$phq9.sum_score>=15 & covidcns_greta$phq9.sum_score<20]=3
covidcns_greta$phq9_depression_stage[covidcns_greta$phq9.sum_score>=20]="Severe"
covidcns_greta$phq9_depression_stage_numeric[covidcns_greta$phq9.sum_score>=20]=4
table(covidcns_greta$phq9_depression_stage)
summary(covidcns_greta$phq9_depression_stage)
table(covidcns_greta$phq9_depression_stage_numeric)
summary(covidcns_greta$phq9_depression_stage_numeric)
## this is from data base
table(covidcns_greta$phq9.severity_depression)                                                   
table(covidcns_greta$phq9.severity_depression_numeric)       
summary(covidcns_greta$phq9.severity_depression_numeric)       

### 21 Nervous_system_illnesses
table(covidcns_greta$phh.epilepsy_or_convulsions_numeric)                                          
table(covidcns_greta$phh.epilepsy_or_convulsions)                                                  
table(covidcns_greta$phh.migraines_numeric)                                                        
table(covidcns_greta$phh.migraines)                                                                
table(covidcns_greta$phh.multiple_sclerosis_numeric)                                               
table(covidcns_greta$phh.multiple_sclerosis)                                                       
table(covidcns_greta$phh.parkinsons_disease_numeric)                                               
table(covidcns_greta$phh.parkinsons_disease)                                                      
table(covidcns_greta$phh.severe_memory_loss__numeric)                                              
table(covidcns_greta$phh.severe_memory_loss_)                                                     
table(covidcns_greta$phh.hay_fever_numeric)                                                        
table(covidcns_greta$phh.hay_fever)                                                                
table(covidcns_greta$phh.drug_allergy_numeric)                                                    
table(covidcns_greta$phh.drug_allergy)                                                             
table(covidcns_greta$phh.food_allergy__numeric)                                                    
table(covidcns_greta$phh.food_allergy_)                                                            
table(covidcns_greta$phh.any_other_allergy__numeric)                                               
table(covidcns_greta$phh.any_other_allergy_)                                                       
table(covidcns_greta$phh.dont_know_numeric)                                                        
table(covidcns_greta$phh.dont_know)                                                                
table(covidcns_greta$phh.prefer_not_to_answer_numeric)                                            
table(covidcns_greta$phh.prefer_not_to_answer)                                                     
table(covidcns_greta$phh.none_of_the_above_numeric)                                                
table(covidcns_greta$phh.none_of_the_above)                                                       

## 22. Steroids
table(covidcns_greta$phh.steroids)
table(covidcns_greta$phh.steroids_numeric)
summary(covidcns_greta$phh.steroids_numeric)

## 23. Immunosuppressants
table(covidcns_greta$phh.immunosuppressants)
table(covidcns_greta$phh.immunosuppressants_numeric)
summary(covidcns_greta$phh.immunosuppressants_numeric)

## 24	Taste_ability
table(covidcns_greta$smelltaste.taste_food_ability_sense)                                          
table(covidcns_greta$smelltaste.taste_food_ability_sense_numeric) 
summary(covidcns_greta$smelltaste.taste_food_ability_sense_numeric) 

## 25	Taste_loss_severity
table(covidcns_greta$smelltaste.how_severe_iswas_the_loss) 
table(covidcns_greta$smelltaste.how_severe_iswas_the_loss_numeric)
summary(covidcns_greta$smelltaste.how_severe_iswas_the_loss_numeric)

## 26	Taste_returned
table(covidcns_greta$smelltaste.taste_returned_sense_smell) 
table(covidcns_greta$smelltaste.taste_returned_sense_smell_numeric) 
summary(covidcns_greta$smelltaste.taste_returned_sense_smell_numeric) 

## 27	Taste_loss_since_COVID-19
table(covidcns_greta$smelltaste.required_start_question_covid19)                                   
table(covidcns_greta$smelltaste.required_start_question_covid19_numeric)
summary(covidcns_greta$smelltaste.required_start_question_covid19_numeric)                           


## 28	Vaccine_firstdose
table(covidcns_greta$vaccine.have_you_had_a_covid19_vaccine)  
table(covidcns_greta$vaccine.have_you_had_a_covid19_vaccine_numeric)                               
summary(covidcns_greta$vaccine.have_you_had_a_covid19_vaccine_numeric)                               

## 29	Vaccine_Date_firstdose
table(covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt) 
summary(covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt) 

## 30	Vaccine_seconddose
table(covidcns_greta$vaccine.have_you_had_your_second_dose)
table(covidcns_greta$vaccine.have_you_had_your_second_dose_numeric)                                
summary(covidcns_greta$vaccine.have_you_had_your_second_dose_numeric)                                

## 31	Vaccine_Date_seconddose
table(covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1)
summary(covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1)
## 32	Vaccine_type
table(covidcns_greta$vaccine.which_vaccine)
table(covidcns_greta$vaccine.which_vaccine_numeric)
summary(covidcns_greta$vaccine.which_vaccine_numeric)

## 33	Vaccine_before_covid
### Covid +ve date 
table(covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt)
## Vaccine_Date_seconddose
table(covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1)                                       
## 
covidcns_greta$Vaccine_before_covid=covidcns_greta$Vaccine_before_covid_numeric=rep(NA, nrow(covidcns_greta))
covidcns_greta$Vaccine_before_covid[covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt-covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1 >14]= "yes"
covidcns_greta$Vaccine_before_covid[covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt-covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1 <=14]= "No"
summary(covidcns_greta$Vaccine_before_covid)
table(covidcns_greta$Vaccine_before_covid)

covidcns_greta$Vaccine_before_covid_numeric[covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt-covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1 >14]= 1
covidcns_greta$Vaccine_before_covid_numeric[covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt-covidcns_greta$vaccine.date_day_ddmmyyyy_doseplease.txt.1 <=14]= 0
summary(covidcns_greta$Vaccine_before_covid_numeric)
table(covidcns_greta$Vaccine_before_covid_numeric)

## 34. Exam_Smell
# Not able to find the corresponding variable in the data

## 35. Exam_Alertness
# Not able to find the corresponding variable in the data

#35.1	Alertness_outpatients
#35.2	Exam_AMT4Patient_outpatients
#35.3	Attention_outpatients
#35.4	Acute_change_fluctuation_outpatients
#35.2a	AMT 4 abnormal yes/ no

#36	Cognitive
                             
table(covidcns_greta$nis_outp.cognitive_function)
table(covidcns_greta$nis_outp.cognitive_function_numeric)
summary(covidcns_greta$nis_outp.cognitive_function_numeric)

#37	Cognitive_Impairment_type
table(covidcns_greta$nis_outp.consciousness_cognitive_problems_related)                           
table(covidcns_greta$nis_outp.time_place_orientation_person)                                      
table(covidcns_greta$nis_outp.memory_problems_recalling_past)                                     
table(covidcns_greta$nis_outp.attention_choose_concentrate_ability)                              
table(covidcns_greta$nis_outp.initiate_start_initiation_action)                                    
table(covidcns_greta$nis_outp.executive_function)                                                  

table(covidcns_greta$nis_outp.consciousness_cognitive_problems_related_numeric)                           
table(covidcns_greta$nis_outp.time_place_orientation_person_numeric)                                      
table(covidcns_greta$nis_outp.memory_problems_recalling_past_numeric)                                     
table(covidcns_greta$nis_outp.attention_choose_concentrate_ability_numeric)                              
table(covidcns_greta$nis_outp.initiate_start_initiation_action_numeric)                                    
table(covidcns_greta$nis_outp.executive_function_numeric)
covidcns_greta$Cognitive_Impairment_type=rep(NA, nrow(covidcns_greta))
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.consciousness_cognitive_problems_related_numeric==1]="Consciousness"
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.time_place_orientation_person_numeric==1]="Orientation"
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.memory_problems_recalling_past_numeric==1]="Memory"
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.attention_choose_concentrate_ability_numeric==1]="Attention"
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.initiate_start_initiation_action_numeric==1]="Initiation"
covidcns_greta$Cognitive_Impairment_type[covidcns_greta$nis_outp.executive_function_numeric==1]="Executive function"
summary(covidcns_greta$Cognitive_Impairment_type)
table(covidcns_greta$Cognitive_Impairment_type)

covidcns_greta$Cognitive_Impairment_type_numeric=rep(NA, nrow(covidcns_greta))
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.consciousness_cognitive_problems_related_numeric==1]=1
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.time_place_orientation_person_numeric==1]=2
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.memory_problems_recalling_past_numeric==1]=3
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.attention_choose_concentrate_ability_numeric==1]=4
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.initiate_start_initiation_action_numeric==1]=5
covidcns_greta$Cognitive_Impairment_type_numeric[covidcns_greta$nis_outp.executive_function_numeric==1]=6
summary(covidcns_greta$Cognitive_Impairment_type_numeric)
table(covidcns_greta$Cognitive_Impairment_type_numeric)
      
#38	Mood 
table(covidcns_greta$nis_outp.mood)
table(covidcns_greta$nis_outp.mood_numeric)
summary(covidcns_greta$nis_outp.mood_numeric)

#39	Mood_Impairment_type
table(covidcns_greta$nis_outp.dejection_depressionlow_mood_feelings)                              
table(covidcns_greta$nis_outp.tension_feelings_anxiety_emotion)                                    
table(covidcns_greta$nis_outp.strong_emotions_feelings_occur) 

table(covidcns_greta$nis_outp.dejection_depressionlow_mood_feelings_numeric)                              
table(covidcns_greta$nis_outp.tension_feelings_anxiety_emotion_numeric)                                    
table(covidcns_greta$nis_outp.strong_emotions_feelings_occur_numeric)

covidcns_greta$Mood_Impairment_type=rep(NA, nrow(covidcns_greta))
covidcns_greta$Mood_Impairment_type[covidcns_greta$nis_outp.dejection_depressionlow_mood_feelings_numeric==1]="Depression/Low mood"
covidcns_greta$Mood_Impairment_type[covidcns_greta$nis_outp.tension_feelings_anxiety_emotion_numeric==1]="Anxiety"
covidcns_greta$Mood_Impairment_type[covidcns_greta$covidcns_greta$nis_outp.strong_emotions_feelings_occur_numeric==1]="Emotional liability "
summary(covidcns_greta$Mood_Impairment_type)
table(covidcns_greta$Mood_Impairment_type)

covidcns_greta$Mood_Impairment_type=rep(NA, nrow(covidcns_greta))
for (i in 1:nrow(covidcns_greta)){
  if (covidcns_greta$nis_outp.dejection_depressionlow_mood_feelings_numeric[i]==1) {covidcns_greta$Mood_Impairment_type[i]="Depression/Low mood"} else {
    if (covidcns_greta$nis_outp.tension_feelings_anxiety_emotion_numeric[i]==1) {covidcns_greta$Mood_Impairment_type[i]="Anxiety"} else{
      if (covidcns_greta$covidcns_greta$nis_outp.strong_emotions_feelings_occur_numeric[i]==1) {covidcns_greta$Mood_Impairment_type[i]="Emotional liability"}}}}
summary(covidcns_greta$Mood_Impairment_type)
table(covidcns_greta$Mood_Impairment_type)

#40	Fatigue
table(covidcns_greta$nis_outp.fatigue)
table(covidcns_greta$nis_outp.fatigue_numeric)
summary(covidcns_greta$nis_outp.fatigue_numeric)

#41	Fatigue_Impairment_type
table(covidcns_greta$nis_outp.perform_physical_exercise_physical)                                  
table(covidcns_greta$nis_outp.muscle_fatigability_decline_muscles)                                 
table(covidcns_greta$nis_outp.cognitive_fatigue_extreme_capable)

table(covidcns_greta$nis_outp.perform_physical_exercise_physical_numeric)                                  
table(covidcns_greta$nis_outp.muscle_fatigability_decline_muscles_numeric)                                 
table(covidcns_greta$nis_outp.cognitive_fatigue_extreme_capable_numeric)

covidcns_greta$Fatigue_Impairment_type=rep(NA, nrow(covidcns_greta))
covidcns_greta$Fatigue_Impairment_type[covidcns_greta$nis_outp.perform_physical_exercise_physical_numeric==1]="Reduced cardiovascular fitness"
covidcns_greta$Fatigue_Impairment_type[covidcns_greta$nis_outp.muscle_fatigability_decline_muscles_numeric==1]="Muscle fatigability"
covidcns_greta$Fatigue_Impairment_type[covidcns_greta$nis_outp.cognitive_fatigue_extreme_capable_numeric==1]="Cognitive fatigue"
summary(covidcns_greta$Fatigue_Impairment_type)
table(covidcns_greta$Fatigue_Impairment_type)


# 42 Cognitron
table(covidcns_greta$cognitron_outp.did_the_participant_attempt_the_cognitron_assessment)
table(covidcns_greta$cognitron_outp.did_the_participant_attempt_the_cognitron_assessment_numeric)
summary(covidcns_greta$cognitron_outp.did_the_participant_attempt_the_cognitron_assessment_numeric)


# 43 Cognitron_reason
table(covidcns_greta$cognitron_outp.severity_test_attempted_due) 
table(covidcns_greta$cognitron_outp.severity_test_attempted_due_numeric)                                  
summary(covidcns_greta$cognitron_outp.severity_test_attempted_due_numeric)                                  

table(covidcns_greta$cognitron_outp.test_attempted_due_nonneurologicalphysical_numeric)                   
table(covidcns_greta$cognitron_outp.respond_appropriately_attempted_participant_numeric)                   
table(covidcns_greta$cognitron_outp.major_problems_test_attempted_numeric)                   
table(covidcns_greta$cognitron_outp.participant_illness_test_completed_numeric)                            
table(covidcns_greta$cognitron_outp.participant_difficulties_test_attempted_numeric)                       
table(covidcns_greta$cognitron_outp.test_not_attempted_due_to_examiner_error_numeric)                      
table(covidcns_greta$cognitron_outp.reasons_test_attempted_due_numeric)                      

#44	Cognitron_tasks
table(covidcns_greta$cognitron_outp.did_participant_complete_all_cognitron_tasks)                  
table(covidcns_greta$cognitron_outp.did_participant_complete_all_cognitron_tasks_numeric)                  
summary(covidcns_greta$cognitron_outp.did_participant_complete_all_cognitron_tasks_numeric)                  

#45	Cognitron_not_completed_reason
table(covidcns_greta$cognitron_outp.test_attempted_cognitiveneurological_reason)                  
table(covidcns_greta$cognitron_outp.test_attempted_completed_due)                   
table(covidcns_greta$cognitron_outp.poor_effort_random_responding)                                 
table(covidcns_greta$cognitron_outp.major_problems_test_attempted.1)                               
table(covidcns_greta$cognitron_outp.test_interrupted_test_attempted)                               
table(covidcns_greta$cognitron_outp.participant_difficulties_test_attempted.1)                     
table(covidcns_greta$cognitron_outp.test_attempted_completed_examiner)                     
table(covidcns_greta$cognitron_outp.test_attempted_reasons_completed)                             

## these are for inpatients
#46	Moca_visuospatialexecutive
table(covidcns_greta$moca_inp.visuospatialexecutive.txt) 
summary(covidcns_greta$moca_inp.visuospatialexecutive.txt) 

#47	Moca_naming
table(covidcns_greta$moca_inp.naming.txt)
summary(covidcns_greta$moca_inp.naming.txt)

#48	Moca_attention
table(covidcns_greta$moca_inp.attention)
summary(covidcns_greta$moca_inp.attention)
table(covidcns_greta$moca_inp.attention.1)
summary(covidcns_greta$moca_inp.attention.1)
table(covidcns_greta$moca_inp.attention.2)
summary(covidcns_greta$moca_inp.attention.2)

#49	Moca_language
table(covidcns_greta$moca_inp.language)
summary(covidcns_greta$moca_inp.language)

table(covidcns_greta$moca_inp.language.1)
summary(covidcns_greta$moca_inp.language.1)

#50	Moca_abstraction
table(covidcns_greta$moca_inp.abstraction.txt)
summary(covidcns_greta$moca_inp.abstraction.txt)
#51	Moca_delayed
table(covidcns_greta$moca_inp.uncued_recall_scored_delayed.txt)
summary(covidcns_greta$moca_inp.uncued_recall_scored_delayed.txt)

#52	Moca_orientation
table(covidcns_greta$moca_inp.orientation.txt)
summary(covidcns_greta$moca_inp.orientation.txt)

#53	Moca_total_score
  table(covidcns_greta$moca_inp.total_score.txt)
  summary(covidcns_greta$moca_inp.total_score.txt)
  
  table(covidcns_greta$moca_inp.adj_sum_score)
 summary(covidcns_greta$moca_inp.adj_sum_score)
  
#54	Moca_notes
#55	Moca_complete
table(covidcns_greta$moca_inp.cognition_numeric)
summary(covidcns_greta$moca_inp.cognition_numeric)

table(covidcns_greta$moca_inp.cognition)
summary(covidcns_greta$moca_inp.cognition)

## not in the data set for out patients

#56	Admission_date
table(covidcns_greta$ncrf1_admission.date_of_inpatient_admission.txt)                      
summary(covidcns_greta$ncrf1_admission.date_of_inpatient_admission.txt)                      

#57	COVID-19_positive_test_date
table(covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt)
summary(covidcns_greta$ncrf1_admission.date_of_positive_covid19_test.txt)

#58.1	ncrf_cic.viral_rna_pcr_positive
table(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive)                                       
table(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive_numeric)
summary(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive_numeric)
#58.2 	ncrf_cic.viral_rna_pcr_positive.date
summary(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive.2)

#59.1	ncrf_cic.viral_rna_pcr_positive.1
table(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive.1)                                     
table(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive.1_numeric)                                     
summary(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive.1_numeric)                                     
#59.2	ncrf_cic.viral_rna_pcr_positive.1.date
summary(covidcns_greta$ncrf1_cic.viral_rna_pcr_positive.3)

#60.1	ncrf_cic.chest_xray
table(covidcns_greta$ncrf1_cic.chest_xray_evidence_of_covid19)
table(covidcns_greta$ncrf1_cic.chest_xray_evidence_of_covid19_numeric) 
summary(covidcns_greta$ncrf1_cic.chest_xray_evidence_of_covid19_numeric) 
#60.2	ncrf_cic.chest_xray.date
summary(covidcns_greta$ncrf1_cic.chest_xray_evidence_of_covid19.1)

#61.1	ncrf_cic.positive_antibody_test
table(covidcns_greta$ncrf1_cic.positive_antibody_test)
table(covidcns_greta$ncrf1_cic.positive_antibody_test_numeric) 
summary(covidcns_greta$ncrf1_cic.positive_antibody_test_numeric) 
#61.2	ncrf_cic.positive_antibody_test.date
summary(covidcns_greta$ncrf1_cic.positive_antibody_test.1) 

#62.1	ncrf_cic.lateral_flow_assay_positive
table(covidcns_greta$ncrf1_cic.lateral_flow_assay_positive)
table(covidcns_greta$ncrf1_cic.lateral_flow_assay_positive_numeric)
summary(covidcns_greta$ncrf1_cic.lateral_flow_assay_positive_numeric)
#62.2	ncrf_cic.lateral_flow_assay_positive.date
summary(covidcns_greta$ncrf1_cic.lateral_flow_assay_positive.1)

#63.1	ncrf_cic.clinical_diagnosis_of_covid19
table(covidcns_greta$ncrf1_cic.clinical_diagnosis_of_covid19)
table(covidcns_greta$ncrf1_cic.clinical_diagnosis_of_covid19_numeric) 
summary(covidcns_greta$ncrf1_cic.clinical_diagnosis_of_covid19_numeric) 
#63.2	ncrf_cic.clinical_diagnosis_of_covid19.date
summary(covidcns_greta$ncrf1_cic.clinical_diagnosis_of_covid19.1)                              
                                                       
#64	Dementia
table(covidcns_greta$ncrf_comorbid.dementia)                                                       
table(covidcns_greta$ncrf_comorbid.dementia_numeric)                                              
summary(covidcns_greta$ncrf_comorbid.dementia_numeric)                                              

#65	Ischemic_stroke
table(covidcns_greta$ncrf_comorbid.ischemic_stroke)                                               
table(covidcns_greta$ncrf_comorbid.ischemic_stroke_numeric)
summary(covidcns_greta$ncrf_comorbid.ischemic_stroke_numeric)

#66	Psychiatric/Psychological_disorder
table(covidcns_greta$ncrf_comorbid.psychiatricpsychological_disorder)                              
table(covidcns_greta$ncrf_comorbid.psychiatricpsychological_disorder_numeric)
summary(covidcns_greta$ncrf_comorbid.psychiatricpsychological_disorder_numeric)

# 67 Clinical_frailty_scale
table(covidcns_greta$ncrf_comorbid.clinical_frailty_scale)                                         
table(covidcns_greta$ncrf_comorbid.clinical_frailty_scale_numeric)                                 
summary(covidcns_greta$ncrf_comorbid.clinical_frailty_scale_numeric)                                 

# 68	ncrf_admission_vital.respiratory_symptom_onset
table(covidcns_greta$ncrf1_vital.respiratory_symptom_onset.txt)
summary(covidcns_greta$ncrf1_vital.respiratory_symptom_onset.txt)
# 69	ncrf_admission_vital.fever_symptom_onset
table(covidcns_greta$ncrf1_vital.fever_symptom_onset.txt)
summary(covidcns_greta$ncrf1_vital.fever_symptom_onset.txt)
# 70	ncrf_admission_vital.neurological_symptom_onset
table(covidcns_greta$ncrf1_vital.neurological_symptom_onset.txt)
summary(covidcns_greta$ncrf1_vital.neurological_symptom_onset.txt)
# 71	admitted_for_covid
table(covidcns_greta$ncrf1_vital.admitted_for_covid_)
summary(covidcns_greta$ncrf1_vital.admitted_for_covid__numeric)
# 72	admitted_for_a_neurological_complication_
table(covidcns_greta$ncrf1_vital.admitted_for_a_neurological_complication_)
summary(covidcns_greta$ncrf1_vital.admitted_for_a_neurological_complication__numeric)
# 73	admitted_for_another_disease
table(covidcns_greta$ncrf1_vital.admitted_for_another_disease)
summary(covidcns_greta$ncrf1_vital.admitted_for_another_disease_numeric)
# 74	admission_severity_worst_covid19
table(covidcns_greta$ncrf1_vital.admission_severity_worst_covid19)
summary(covidcns_greta$ncrf1_vital.admission_severity_worst_covid19_numeric)
                         

                                                              
                       

