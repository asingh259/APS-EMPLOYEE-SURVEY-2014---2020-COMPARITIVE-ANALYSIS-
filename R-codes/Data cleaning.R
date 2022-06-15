# Project
library(tidyverse)
setwd('C:/Users/LIEN PHAM/Desktop/Langara/Predictive analytics - DANA 4801/Project/Data/2014')
getwd

data2014_aps = read.csv('2014-aps.csv')

data_2014 = data2014_aps %>% 
  select('q1','q2.','q6.','q18a','q18c','q18e','q18f','q18g','q19b',
         'q21e','q21h','q21g','q21c',
         'q22a','q22c','q22d','q22e','q22s','q22b','q22o',
         'q36a','q36b','q36c','q36d','q36e','q36f','q36g',
         'q70','q73','q71.1','q71.2',	'q71.3','q71.4','q71.5',
         'q71.6','q71.7','q71.8','q71.9','q71.10','q74')

ncol(data_2014) #40

replace_na = function(df, col){
  df[df[[col]] == " ", col] = NA
  return(df)
}

for (i in colnames(data_2014)){
  data_2014 = replace_na(data_2014, i)
}


#####################################
# this fun_1 applied for:
#2014: q18a, 18c,e,f,g and q19b,21e,21h,21g,21c,22a,22c,22d,22e,22s,22b,22o
#18c	18e	18f	18g	19b	21e	21h	21g	21c	22a	22c	22d	22e	22s	22b	22o->	TOTAL: 16 columns
#2020: 17a 17c	17d	17e	18	18b	20	21	21b	21c	21d	22a 	23a	23c	23d	23f	23g	23l	46	47a	47b	47c	47d	47e	47f	47g

fun_1 = function(df,i){
  df[(df[[i]] == "Strongly agree" & !is.na(df[[i]])), str_c(i, "_new")] = "1"
  df[df[[i]] == "Agree" & !is.na(df[[i]]), str_c(i, "_new")] = "2"
  df[df[[i]] == "Neither agree nor disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  df[df[[i]] == "Strongly disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "5"
  return(df)
}

for(i in c('q18a','q18c','q18e','q18f','q18g','q19b',
           'q21e','q21h','q21g','q21c','q22a','q22c','q22d','q22e','q22s','q22b','q22o')){
  data_2014 =  fun_1(data_2014,i)
}

####################################
# this fun_2 applied for:
#2014: q36a	36b	36c	36d	36f	36e	36g -> total:7 columns
#2020: q47a,b,c,d,e,f


fun_2 = function(df,i){
  df[df[[i]] == "Always" & !is.na(df[[i]]), str_c(i, "_new")] = "5"
  df[df[[i]] == "Never" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  df[df[[i]] == "Often" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Rarely" & !is.na(df[[i]]), str_c(i, "_new")] = "2"
  df[df[[i]] == "Sometimes" & !is.na(df[[i]]), str_c(i, "_new")] = "1"
  return(df)
}

for(i in c('q36a','q36b','q36c','q36d','q36e','q36f','q36g')){
  data_2014 =  fun_2(data_2014,i)
}

######################################
# this fun_3 applied for:
#2014: 73,70
#2020: 61,62,63,64

fun_3 = function(df,i){
  df[df[[i]] == "No" & !is.na(df[[i]]), str_c(i, "_new")] = "0"
  df[df[[i]] == "Yes" & !is.na(df[[i]]), str_c(i, "_new")] = "1"
  df[df[[i]] == "Not sure" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Would prefer not to answer" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  return(df)
}

for(i in c('q70','q73')){
  data_2014 = fun_3(data_2014,i)
}
################
# fun_4 applied for 
#Y2014:q71.1	q71.2	q71.3	q71.4	q71.5	q71.6	q71.7	q71.8	q71.9	q71.10

fun_4 = function(df,i){
  df[df[[i]] == "Ticked" &  !is.na(df[[i]]), str_c(i, "_new")] = "1"
  df[is.na(df[[i]]), str_c(i, "_new")] = '0'
  return(df)
}

for(i in c('q71.1','q71.2',	'q71.3','q71.4','q71.5','q71.6','q71.7','q71.8','q71.9','q71.10')){
  data_2014 = fun_4(data_2014,i)
}

table(data_2014$q71.1_new)
##############################################################
data_2014_new = data_2014 %>% 
  mutate(age_new = case_when(q2. == "Under 40 years" ~ "1",
                             q2. == "40 to 54 years" ~ "2",
                             q2. == "55 years or older" ~ "3",
                             TRUE ~ q2.),
         gender_new = case_when(q1 == "Female" ~ "F",
                                q1 == "Male" ~ "M",
                                q1 == "Prefer not to say" ~ "NS",
                                q1 == "X (Indeterminate/Intersex/Unspecified)" ~ "U",
                                TRUE ~ q1),
         level_new = case_when(q6. == "Trainee/Graduate/APS" ~ "1",
                               q6. == "EL" ~ "2",
                               q6. == "SES" ~ "3",
                               TRUE ~ q6.),
        q74_new = case_when(q74 == "Verbal abuse" ~ "2",     #same q62.4(2014)
                            q74 == "Other" ~ "0",
                            q74 == "Inappropriate and unfair application of other work policies or rules" ~ "4",   #same q62.8(2014)
                            q74 == "Inappropriate and unfair application of performance management practices" ~ "0",
                            q74 == "Harassment based on a personal characteristic (e.g. gender, disability, ethnicity, age, religion, political opinion, sex" ~ "0",
                            q74 == "Inappropriate and unfair application of fitness for duty assessments" ~ "0",
                            q74 == "'Initiations' or pranks" ~ "3",   #same q62.5(2014)
                            q74 == "Physical behaviour" ~ "1",       #same q62.1(2014)
                                          TRUE ~ q74))
data_2014_new[is.na(data_2014_new$q74_new), "q74_new"] = "0"

View(data_2014_new)

data_2014_new_trunc = data_2014_new %>% 
  select(-c(1:40))

View(data_2014_new_trunc)
##TEST
View(data_2014[, colnames(data_2014_new)[str_detect(colnames(data_2014), "_new")]])
unique(data_2014$q71.7)
unique(data_2014_new$q71.7_new)
unique(data_2014_new$q74_new)
View(data_2014_new)

# replacing mode value

getmode <- function(df,i) {
  uniqv <- unique(df[!is.na(df[[i]]), i, drop = T])
  res = uniqv[which.max(tabulate(match(df[[i]], uniqv)))]
  return(res)
}

getmode(data_2014_new_trunc,i=1)
getmode(ata_2014_new_trunc,i=9)

data_2014_final = data_2014_new_trunc

for (i in (1:ncol(data_2014_final))){
  
  data_2014_final[is.na(data_2014_final[[i]]), i] = getmode(data_2014_final,i)
}

sum(is.na(data_2014_final))  #84430
sum(is.na(data_2014_final$q74_new))#84430

library("writexl")
write_xlsx(data_2014_final,'C:/Users/LIEN PHAM/Desktop/Langara/data_2014_final.xlsx')

table(data_2014_final$q71.1_new)
table(data_2014_new$q71.1_new)
table(data_2014$q71.1)

sum(is.na(data_2014_final))
#############################################################################################

##############################################################################################
library(tidyverse)
setwd('C:/Users/LIEN PHAM/Desktop/Langara/Predictive analytics - DANA 4801/Project/Data/2020')
getwd
data2020_aps = read.csv('2020-aps-employee-census-dataset.csv',encoding = "UTF-8", stringsAsFactors = TRUE)

ncol(data2020_aps)

data_2020 = data2020_aps %>% 
  select('q1','q2.','q5.','q17a','q17c','q17d','q17e','q18b','q21b','q21c','q21d',
         'q22a','q23a','q23c','q23d','q23f','q23g','q23l',
         'q24.1','q24.2','q24.3','q24.4','q24.5','q24.6','q24.7','q24.8','q24.9','q24.10','q24.11','q24.12',
         'q25','q26.1','q26.2','q26.3','q26.4','q26.5','q26.6','q26.7','q26.8','q26.9','q26.10','q26.11',
         'q27','q30','q31',
         'q33.1','q33.2','q33.3','q33.4','q33.5','q33.6','q33.7','q33.8','q33.9',
         'q34a','q34b','q34c','q34d','q34e','q35','q36','q37',
         'q39.1','q39.2','q39.1','q39.3','q39.4','q39.5','q39.6','q39.7','q39.8','q39.9','q39.10',
         'q39.11','q39.12','q39.13','q39.14','q39.15','q39.16',
         'q40.1','q40.2','q40.3','q41.1','q41.2','q41.3',
         'q49','q51','q58','q59',
         'q60.1','q60.2','q60.3','q60.4','q60.5','q60.6','q60.7','q60.8','q60.9',
         'q46','q47a','q47b','q47c','q47d','q47e','q47f','q47g',
         'q61','q63','q62.1','q62.4','q62.5','q62.8',
         'q64.1','q64.2','q64.3','q64.4','q64.6','q64.7','q64.8','q64.9','q64.10','q64.13')

View(data_2020)

ncol(data_2020)#120 columns

unique(data_2020$q34a)

replace_na = function(df, col){
  df[df[[col]] == " ", col] = NA
  return(df)
}

for (i in colnames(data_2020)){
  data_2020 = replace_na(data_2020, i)
}

# CLEANING DATASET 2020

#2014: q18c,e,f,g and q19c,21e,21h,21g,21c,22a,22c,22d,22e,22s,22b,22o
#2020: 17a 17c	17d	17e	18b	21b	21c	21d	22a	23a	23c	23d	23f	23g	23l	46 -> total: 16	
#'q25','q34a','q34b','q34c','q34d','q34e','q51' (just added)

fun_1 = function(df,i){
  df[(df[[i]] == "Strongly agree" & !is.na(df[[i]])), str_c(i, "_new")] = "1"
  df[df[[i]] == "Agree" & !is.na(df[[i]]), str_c(i, "_new")] = "2"
  df[df[[i]] == "Neither agree nor disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  df[df[[i]] == "Strongly disagree" & !is.na(df[[i]]), str_c(i, "_new")] = "5"
  return(df)
}

for(i in c('q17a','q17c','q17d','q17e','q18b','q21b','q21c','q21d',
           'q22a','q23a','q23c','q23d','q23f','q23g','q23l','q46',
           'q25','q34a','q34b','q34c','q34d','q34e','q51')){
  data_2020 =  fun_1(data_2020,i)
}

unique(data_2020_new$q34e_new)


str(data_2020$q34e_new)

####################################
# this fun_2 applied for:
#2014: q36a,b,c,d,e,f,g  
#2020: 47a	47b	47c	47d	47e	47f	47g

fun_2 = function(df,i){
  df[df[[i]] == "Always" & !is.na(df[[i]]), str_c(i, "_new")] = "5"
  df[df[[i]] == "Never" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  df[df[[i]] == "Often" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Rarely" & !is.na(df[[i]]), str_c(i, "_new")] = "2"
  df[df[[i]] == "Sometimes" & !is.na(df[[i]]), str_c(i, "_new")] = "1"
  return(df)
}

for(i in c('q47a','q47b','q47c','q47d','q47e','q47f','q47g')){
  data_2020 =  fun_2(data_2020,i)
}

######################################
# this fun_3 applied for:
#2014: 73,70
#2020: 61,63 (q62,q64: code different)
#'q35','q36','q37','q58','q59'


fun_3 = function(df,i){
  df[df[[i]] == "No" & !is.na(df[[i]]), str_c(i, "_new")] = "0"
  df[df[[i]] == "Yes" & !is.na(df[[i]]), str_c(i, "_new")] = "1"
  df[df[[i]] == "Not sure" & !is.na(df[[i]]), str_c(i, "_new")] = "3"
  df[df[[i]] == "Would prefer not to answer" & !is.na(df[[i]]), str_c(i, "_new")] = "4"
  return(df)
}

for(i in c('q61','q63')){
  data_2020 = fun_3(data_2020,i)
}

#####################

fun_3_1 = function(df,i){
  df[df[[i]] == "No" & !is.na(df[[i]]), str_c(i, "_new")] = "0"
  df[df[[i]] == "Yes" & !is.na(df[[i]]), str_c(i, "_new")] = "1"
  return(df)
}

for(i in c('q35','q36','q37','q58','q59')){
  data_2020 = fun_3(data_2020,i)
}

table(data_2020$q35_new)
sum(is.na(data_2020$q35_new))

################
# fun_4 applied for 
#Y2014:q71.1	q71.2	q71.3	q71.4	q71.5	q71.6	q71.7	q71.8	q71.9	q71.10
#Y2020:  q62.1,q62.4,q62.5,q62.8,q64.1,q64.2,q64.3,q64.4,q64.6,q64.7,q64.8,q64.9,q64.10,q64.13
# 'q24.1','q24.2','q24.3','q24.4','q24.5','q24.6','q24.7','q24.8','q24.9','q24.10','q24.11','q24.12'
#'q26.1','q26.2','q26.3','q26.4','q26.5','q26.6','q26.7','q26.8','q26.9','q26.10','q26.11'
#'q33.1','q33.2','q33.3','q33.4','q33.5','q33.6','q33.7','q33.8','q33.9'
#'q39.1','q39.2','q39.1','q39.3','q39.4','q39.5','q39.6','q39.7','q39.8','q39.9','q39.10',
#'q39.11','q39.12','q39.13','q39.14','q39.15','q39.16'
#'q40.1','q40.2','q40.3','q41.1','q41.2','q41.3'
#'q60.1','q60.2','q60.3','q60.4','q60.5','q60.6','q60.7','q60.8','q60.9'

fun_4 = function(df,i){
  df[df[[i]] == "Tick" &  !is.na(df[[i]]), str_c(i, "_new")] = "1"
  df[is.na(df[[i]]), str_c(i, "_new")] = '0'
  return(df)
}

for(i in c('q62.1','q62.4','q62.5','q62.8','q64.1','q64.2','q64.3','q64.4','q64.6',
           'q64.7','q64.8','q64.9','q64.10','q64.13',
           'q24.1','q24.2','q24.3','q24.4','q24.5','q24.6','q24.7','q24.8','q24.9','q24.10','q24.11','q24.12',
           'q26.1','q26.2','q26.3','q26.4','q26.5','q26.6','q26.7','q26.8','q26.9','q26.10','q26.11',
           'q33.1','q33.2','q33.3','q33.4','q33.5','q33.6','q33.7','q33.8','q33.9',
           'q39.1','q39.2','q39.1','q39.3','q39.4','q39.5','q39.6','q39.7','q39.8','q39.9','q39.10',
           'q39.11','q39.12','q39.13','q39.14','q39.15','q39.16',
           'q40.1','q40.2','q40.3','q41.1','q41.2','q41.3',
           'q60.1','q60.2','q60.3','q60.4','q60.5','q60.6','q60.7','q60.8','q60.9'
           )){
  data_2020 = fun_4(data_2020,i) 
}

table(data_2020$q60.9_new)

View(data_2020_new)

###################################################
unique(data_2020$q2.)

data_2020$q2.= as.character(data_2020$q2.)
data_2020$q1= as.character(data_2020$q1)
data_2020$q5.= as.character(data_2020$q5.)
data_2020$q27= as.character(data_2020$q27)
data_2020$q30= as.character(data_2020$q30)
data_2020$q49= as.character(data_2020$q49)


data_2020_new = data_2020 %>% 
  mutate(age_new = case_when(q2. == "Under 40 years" ~ "1",
                             q2. == "40 to 54 years" ~ "2",
                             q2. == "55 years or older" ~ "3",
                             TRUE ~ q2.),
         gender_new = case_when(q1 == "Female" ~ "F",
                                q1 == "Male" ~ "M",
                                q1 == "Prefer not to say" ~ "NS",
                                q1 == "X (Indeterminate/Intersex/Unspecified)" ~ "U",
                                TRUE ~ q1),
         level_new = case_when(q5. == "Trainee/Graduate/APS" ~ "1",
                               q5. == "EL" ~ "2",
                               q5. == "SES" ~ "3",
                               TRUE ~ q5.),
         q27_new = case_when(q27 == 'Increased clarity around my role and responsibilities' ~ '1',
                             q27 == 'Increased clarity around priorities' ~ '2',
                             q27 == 'Improved technology and a more digital environment' ~ '3',
                             q27 == 'Improved internal communication' ~ '4',
                             q27 == 'Fewer layers of decision making' ~ '5',
                             q27 == 'Increased experimentation with new ideas' ~ '6',
                             q27 == 'Increased mobility' ~ '7',
                             q27 == 'Increased flexibility in work practices' ~ '8',
                             q27 == 'Increased instances of working as one APS' ~ '9',
                             q27 == 'Other' ~ '10',
                             TRUE ~ q27),
         q49_new = case_when(q49 == 'Very positive change' ~ '1',
                             q49 == 'Positive change' ~ '2',
                             q49 == 'No change' ~ '3',
                             q49 == 'Negative change' ~ '4',
                             q49 == 'Very negative change' ~ '5',
                             TRUE ~ q49),
         q30_new = case_when(q30 == 'Significantly improved' ~ '1',
                             q30 == 'Improved' ~ '2',
                             q30 == 'No change' ~ '3',
                             q30 == 'Reduced' ~ '4',
                             q30 == 'Significantly reduced' ~ '5',
                             TRUE ~ q30))

#q31_new = case_when(q31 == 'Well above capacity - too much work' ~ '1',
                    #q31 == 'Slightly above capacity - lots of work to do' ~ '2',
                    #q31 == 'At capacity - about the right amount of work to do' ~ '3',
                    #q31 == 'Slightly below capacity - available for more work ' ~ '4',
                    #q31 == 'Below capacity - not enough work' ~ '5',
                    #TRUE ~ q31

View(data_2020_new)

############################

data_2020_new_trunc = data_2020_new %>% 
  select(-c(1:121))

View(data_2020_new_trunc)

###fill mode value into NA values
# replacing mode value

getmode <- function(df,i) {
  uniqv <- unique(df[!is.na(df[[i]]), i, drop = T])
  res = uniqv[which.max(tabulate(match(df[[i]], uniqv)))]
  return(res)
}

getmode(data_2020_new_trunc,i=1)
getmode(data_2020_new_trunc,i=119)

data_2020_final = data_2020_new_trunc

for (i in (1:ncol(data_2020_final))){
  
  data_2020_final[is.na(data_2020_final[[i]]), i] = getmode(data_2020_final,i)
}

View(data_2020_final)
sum(is.na(data_2020_final)) #0


getmode(data_2020_final,"q51_new")

i = "q51_new"

library("writexl")
write_xlsx(data_2020_final,'C:/Users/LIEN PHAM/Desktop/Langara/data_2020_final.xlsx')

sum(is.na(data_2020_new))#7744870
sum(is.na(data_2020_new_trunc)) # 468465
sum(is.na(data_2020_final)) #0

ncol(data_2020_final)

