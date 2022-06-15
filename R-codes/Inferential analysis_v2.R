library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

setwd('C:/Users/LIEN PHAM/Desktop/Langara/Predictive analytics - DANA 4801/Project/Data/2014')
getwd
data_2014 <- read_excel('data_2014_final_v1.xlsx')
sum(is.na(data_2014)) #0

setwd('C:/Users/LIEN PHAM/Desktop/Langara/Predictive analytics - DANA 4801/Project/Data/2020')
getwd
data_2020 <- read_excel('data_2020_final_v1.xlsx')
View(data_2020)
ncol(data_2020) #116

#create year column to compare between 2 data sets
data_2014$year <- '2014'
data_2020$year <- '2020'

##############################################################################################
#1. My job gives me opportunities to utilize my skills
## select required data for analysis (you can select more columns you need)

q18c = as.data.frame(select(data_2014, q18c_new, year))
q17a = as.data.frame(select(data_2020, q17a_new,year))

## change name of the first column. In this step, you can change the name of all columns
#colnames(data_2014)[1] <-'AS'
#colnames(data_2020)[1] <-'AS'

#syntax change all columns: colnames(data_2014) <- c('q18c_new','year')
# d???i column name cho data

colnames(q18c) <- c('q17a_new','year') # Change name for q18c -> 17a

## combine 2 dataframes into 1 

skill_u <- rbind(q18c,q17a,make.row.names=TRUE)

##change year as factor
skill_u$year <- as.factor(skill_u$year)              
levels(skill_u$year)  ##check how many levels in the factor

## descriptive statistics and visualization
skill_u %>% group_by(year,q17a_new) %>% count()
View(skill_u)

aggregate(skill_u$q17a_new, by = list(skill_u$year),median)

ggplot(skill_u, aes(x= factor(year), fill = factor(q17a_new))) + geom_bar(position=position_dodge())  ##it can be used to check validity of assumptions

#####assumption#####
## https://statistics.laerd.com/statistical-guides/mann-whitney-u-test-assumptions.php
## for checking similar distribution test, looking to the histogram (the above ggplot)

## perform wilcox.test

wilcox.test(jitter(as.numeric(skill_u$q17a_new, 5)) ~ year, alternative = "two.sided",data = skill_u, paired= FALSE)

wilcox.test(jitter(as.numeric(skill_u$q17a_new, 5)) ~ year, alternative = "greater",data = skill_u, paired= FALSE)
levels(skill_u$year)

#W = 5274586757, p-value = 6.101e-13 # ket luan 2014> 2020

# [1] "2014" "2020"

wilcox.test(jitter(as.numeric(skill_u$q17a_new, 5)) ~ year, alternative = "less",data = skill_u, paired= FALSE)
#W = 5756859893, p-value = 1 -> not accept H1 (H1: 2014<2020)

levels(year)

#Wilcoxon rank sum test with continuity correction
#data:  jitter(as.numeric(test$q17a_new)) by year
#W = 5748681618, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

## perform mann kruskal wallis test
kruskal.test(q17a_new ~ year, data = test)
#data:  q18c_new by year
#Kruskal-Wallis chi-squared = 954.88, df = 1, p-value < 2.2e-16
#As the p-value is less than the significance level 0.05, 
#we can conclude that there are significant differences between the treatment groups.

#############################################################################################
#2. I am satisfied with the recognition I receive for doing a good job
q18e = as.data.frame(select(data_2014, q18e_new, year))
q17c = as.data.frame(select(data_2020, q17c_new,year))

colnames(q18e) <- c('q17c_new','year') # Change name for q18e -> 17c

## combine 2 dataframes into 1 

job_recognition <- rbind(q18e,q17c,make.row.names=TRUE)
View(job_recognition)

##change year as factor
job_recognition$year <- as.factor(job_recognition$year)              
levels(job_recognition $year)  ##check how many levels in the factor

## descriptive statistics and visualization
job_recognition %>% group_by(year,q17c_new) %>% count()
ggplot(job_recognition, aes(x= factor(year), fill = factor(q17c_new))) + geom_bar(position=position_dodge())  ##it can be used to check validity of assumptions

wilcox.test(jitter(as.numeric(job_recognition$q17c_new, 5)) ~ year, alternative = "two.sided",
            data = job_recognition , paired= FALSE)

#W = 5681785784, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(jitter(as.numeric(job_recognition$q17c_new, 5)) ~ year, alternative = "greater",
            data = job_recognition , paired= FALSE)
levels(job_recognition$year)

########
#I am fairly remunerated (e.g. salary, superannuation) for the work that I do
q18f = as.data.frame(select(data_2014, q18f_new, year))
q17d = as.data.frame(select(data_2020, q17d_new,year))
colnames(q18f) <- c('q17d_new','year') # Change name for q18f -> 17d

## combine 2 dataframes into 1 
remuneration <- rbind(q18f,q17d,make.row.names=TRUE)

##change year as factor
remuneration$year <- as.factor(remuneration$year)              
levels(remuneration$year)  ##check how many levels in the factor

## descriptive statistics and visualization
remuneration%>% group_by(year,q17d_new) %>% count()
ggplot(remuneration, aes(x= factor(year), fill = factor(q17d_new))) + geom_bar(position=position_dodge())  ##it can be used to check validity of assumptions

wilcox.test(jitter(as.numeric(remuneration$q17d_new, 5)) ~ year, alternative = "two.sided", 
            data = remuneration, paired= FALSE)
#W = 5.278e+09, p-value = 7.354e-12
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(jitter(as.numeric(remuneration$q17d_new, 5)) ~ year, alternative = "greater", 
            data = remuneration, paired= FALSE)
#W = 5275295704, p-value = 1
wilcox.test(jitter(as.numeric(remuneration$q17d_new, 5)) ~ year, alternative = "less", 
            data = remuneration, paired= FALSE)
# W = 5274586757, p-value = 6.101e-13 (2014<2020)

##############
# 18g/17e. I am satisfied with my non-monetary employment conditions (e.g. leave, flexible work arrangements, other benefits)
q18g = as.data.frame(select(data_2014, q18g_new, year))
q17e = as.data.frame(select(data_2020, q17e_new,year))

colnames(q18g) <- c('q17e_new','year') # Change name for q18f -> 17d

## combine 2 dataframes into 1 
non_monetary <- rbind(q18g,q17e,make.row.names=TRUE)
View(non_monetary)

##change year as factor
non_monetary$year <- as.factor(non_monetary$year)              
levels(non_monetary$year)  ##check how many levels in the factor

## descriptive statistics and visualization
non_monetary%>% group_by(year,q17e_new) %>% count()

ggplot(non_monetary, aes(x= factor(year), fill = factor(q17e_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(non_monetary$q17e_new, 5)) ~ year, alternative = "two.sided",
            data = non_monetary, paired= FALSE)
#W = 5274586757, p-value = 6.101e-13
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(jitter(as.numeric(non_monetary$q17e_new, 5)) ~ year, alternative = "greater",
            data = non_monetary, paired= FALSE)

# W = 5210856860, p-value = 1
wilcox.test(jitter(as.numeric(non_monetary$q17e_new, 5)) ~ year, alternative = "less",
            data = non_monetary, paired= FALSE)
###############################################################################
# 19. Please rate your level of agreement with the following statements regarding your immediate work group:
q19b = as.data.frame(select(data_2014, q19b_new, year))
q18b = as.data.frame(select(data_2020, q18b_new,year))
colnames(q19b) <- c('q18b_new','year') # Change name for q18f -> 17d

## combine 2 dataframes into 1 
cooperation <- rbind(q19b,q18b,make.row.names=TRUE)
View(cooperation)

##change year as factor
cooperation$year <- as.factor(cooperation$year)              
levels(cooperation$year)  ##check how many levels in the factor

## descriptive statistics and visualization
cooperation%>% group_by(year,q18b_new) %>% count()

ggplot(cooperation, aes(x= factor(year), fill = factor(q18b_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(cooperation$q18b_new, 5)) ~ year, alternative = "two.sided",
            data = cooperation, paired= FALSE)
#W = 5436515797, p-value = 1.767e-06

wilcox.test(jitter(as.numeric(cooperation$q18b_new, 5)) ~ year, alternative = "greater",
            data = cooperation, paired= FALSE)
#W = 5419129865, p-value = 0.0002303
levels(cooperation$year)

################################################################################
#E. GENERAL IMPRESSIONS: SENIOR LEADERSHIP
#Please rate your level of agreement with the following statements regarding the SES in your agency
q21e = as.data.frame(select(data_2014, q21e_new, year))
q21b = as.data.frame(select(data_2020, q21b_new,year))

colnames(q21e) <- c('q21b_new','year') # Change name for q21e -> 21b

## combine 2 dataframes into 1 
talent_develop <- rbind(q21e,q21b,make.row.names=TRUE)
View(talent_develop)

##change year as factor
talent_develop$year <- as.factor(talent_develop$year)              
levels(ctalent_develop$year)  ##check how many levels in the factor

## descriptive statistics and visualization
talent_develop%>% group_by(year,q21b_new) %>% count()
ggplot(talent_develop, aes(x= factor(year), fill = factor(q21b_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(talent_develop$q21b_new, 5)) ~ year, alternative = "two.sided",
            data = talent_develop, paired= FALSE)

# W = 6.038e+09, p-value < 2.2e-16

wilcox.test(jitter(as.numeric(talent_develop$q21b_new, 5)) ~ year, alternative = "greater",
            data = talent_develop, paired= FALSE)
#W = 6037587466, p-value < 2.2e-16
#####################

q21g = as.data.frame(select(data_2014, q21g_new, year))
q21d = as.data.frame(select(data_2020, q21d_new,year))
colnames(q21g) <- c('q21d_new','year') # Change name for q21g -> 21d

## combine 2 dataframes into 1 
aps_strategy <- rbind(q21g,q21d,make.row.names=TRUE)

##change year as factor
aps_strategy$year <- as.factor(aps_strategy$year)              
levels(aps_strategy$year)  ##check how many levels in the factor

## descriptive statistics and visualization
aps_strategy%>% group_by(year,q21d_new) %>% count()

ggplot(aps_strategy, aes(x= factor(year), fill = factor(q21d_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(aps_strategy$q21d_new, 5)) ~ year, alternative = "two.sided",
            data = aps_strategy, paired= FALSE)
# W = 5691412917, p-value < 2.2e-16

wilcox.test(jitter(as.numeric(aps_strategy$q21d_new, 5)) ~ year, alternative = "greater",
            data = aps_strategy, paired= FALSE)
# W = 5700849272, p-value < 2.2e-16
###############################

q21c = as.data.frame(select(data_2014, q21c_new, year))
q22a = as.data.frame(select(data_2020, q22a_new,year))
colnames(q21c) <- c('q22a_new','year') # Change name for q21c -> 22a

## combine 2 dataframes into 1 
communication <- rbind(q21c,q22a,make.row.names=TRUE)

##change year as factor
communication$year <- as.factor(aps_strategy$year)              
levels(communication$year)  ##check how many levels in the factor

## descriptive statistics and visualization
communication%>% group_by(year,q22a_new) %>% count()
ggplot(communication, aes(x= factor(year), fill = factor(q22a_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(communication$q22a_new, 5)) ~ year, alternative = "two.sided",
            data = communication, paired= FALSE)
#W = 5.92e+09, p-value < 2.2e-16

wilcox.test(jitter(as.numeric(communication$q22a_new, 5)) ~ year, alternative = "greater",
            data = communication, paired= FALSE)
# W = 5917915253, p-value < 2.2e-16
###############################################################################
#F - General Impressions: Agency and APS

q22a = as.data.frame(select(data_2014, q22a_new, year))
q23a = as.data.frame(select(data_2020, q23a_new,year))
colnames(q22a) <- c('q23a_new','year') # Change name for q22a -> 23a

## combine 2 dataframes into 1 
personal_attach <- rbind(q22a,q23a,make.row.names=TRUE)

##change year as factor
personal_attach$year <- as.factor(personal_attach$year)              
levels(personal_attach$year)  ##check how many levels in the factor

## descriptive statistics and visualization
personal_attach%>% group_by(year,q23a_new) %>% count()
ggplot(personal_attach, aes(x= factor(year), fill = factor(q23a_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(personal_attach$q23a_new, 5)) ~ year, alternative = "two.sided",
            data = personal_attach, paired= FALSE)
#W = 5339921414, p-value = 0.02093

wilcox.test(jitter(as.numeric(personal_attach$q23a_new, 5)) ~ year, alternative = "greater",
            data = personal_attach, paired= FALSE)
# W = 5334820602, p-value = 0.9964
wilcox.test(jitter(as.numeric(personal_attach$q23a_new, 5)) ~ year, alternative = "less",
            data = personal_attach, paired= FALSE)
#W = 5340169424, p-value = 0.01098

#############################
q22c = as.data.frame(select(data_2014, q22c_new, year))
q23c = as.data.frame(select(data_2020, q23c_new,year))
colnames(q22c) <- c('q23c_new','year') # Change name for q22c -> 23c

## combine 2 dataframes into 1 
work_pride <- rbind(q22c,q23c,make.row.names=TRUE)

##change year as factor
work_pride$year <- as.factor(work_pride$year)              
levels(work_pride$year)  ##check how many levels in the factor

## descriptive statistics and visualization
work_pride%>% group_by(year,q23c_new) %>% count()
ggplot(work_pride, aes(x= factor(year), fill = factor(q23c_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(work_pride$q23c_new, 5)) ~ year, alternative = "two.sided",
            data = work_pride, paired= FALSE)
#U = 5413161238, p-value = 0.002178

wilcox.test(jitter(as.numeric(work_pride$q23c_new, 5)) ~ year, alternative = "greater",
            data = work_pride, paired= FALSE)
#W = 5410858052, p-value = 0.001891
##############################

q22e = as.data.frame(select(data_2014, q22e_new, year))
q23f = as.data.frame(select(data_2020, q23f_new,year))
colnames(q22e) <- c('q23f_new','year') # Change name for q22e -> 23f

## combine 2 dataframes into 1 
ef_communication <- rbind(q22e,q23f,make.row.names=TRUE)

##change year as factor
ef_communication$year <- as.factor(ef_communication$year)              
levels(ef_communication$year)  ##check how many levels in the factor

## descriptive statistics and visualization
ef_communication%>% group_by(year,q23f_new) %>% count()
ggplot(ef_communication, aes(x= factor(year), fill = factor(q23f_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(ef_communication$q23f_new, 5)) ~ year, alternative = "two.sided",
            data = ef_communication, paired= FALSE)
# W = 5842614175, p-value < 2.2e-16


wilcox.test(jitter(as.numeric(ef_communication$q23f_new, 5)) ~ year, alternative = "greater",
            data = ef_communication, paired= FALSE)
#W = 5843667560, p-value < 2.2e-16
##############################

q22s = as.data.frame(select(data_2014, q22s_new, year))
q23g = as.data.frame(select(data_2020, q23g_new,year))
colnames(q22s) <- c('q23g_new','year') # Change name for q22s -> 23g

## combine 2 dataframes into 1 
recommendation <- rbind(q22s,q23g,make.row.names=TRUE)

##change year as factor
recommendation$year <- as.factor(recommendation$year)              
levels(recommendation$year)  ##check how many levels in the factor

## descriptive statistics and visualization
recommendation%>% group_by(year,q23g_new) %>% count()
ggplot(recommendation, aes(x= factor(year), fill = factor(q23g_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(recommendation$q23g_new, 5)) ~ year, alternative = "two.sided",
            data = recommendation, paired= FALSE)
#W = 5540876158, p-value < 2.2e-16

wilcox.test(jitter(as.numeric(recommendation$q23g_new, 5)) ~ year, alternative = "greater",
            data = recommendation, paired= FALSE)
#W = 5546432751, p-value < 2.2e-16

#################
q22o = as.data.frame(select(data_2014, q22o_new, year))
q46 = as.data.frame(select(data_2020, q46_new,year))

colnames(q22o) <- c('q46_new','year') # Change name for q22o -> 46

## combine 2 dataframes into 1 
learn_develop <- rbind(q22o,q46,make.row.names=TRUE)

##change year as factor
learn_develop$year <- as.factor(learn_develop$year)              
levels(learn_develop$year)  ##check how many levels in the factor

## descriptive statistics and visualization
learn_develop%>% group_by(year,q46_new) %>% count()
ggplot(learn_develop, aes(x= factor(year), fill = factor(q46_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(learn_develop$q46_new, 5)) ~ year, alternative = "two.sided",
            data = learn_develop, paired= FALSE)
wilcox.test(jitter(as.numeric(learn_develop$q46_new, 5)) ~ year, alternative = "greater",
            data = learn_develop, paired= FALSE)
#######################################################################################################################
#I - Wellbeing
q36a = as.data.frame(select(data_2014, q36a_new, year))
q47a = as.data.frame(select(data_2020, q47a_new,year))
colnames(q36a) <- c('q47a_new','year') # Change name for q36a -> 47a

## combine 2 dataframes into 1 
time_pressure <- rbind(q36a,q47a,make.row.names=TRUE)

##change year as factor
time_pressure$year <- as.factor(time_pressure$year)              
levels(time_pressure$year)  ##check how many levels in the factor

## descriptive statistics and visualization
time_pressure%>% group_by(year,q47a_new) %>% count()
ggplot(time_pressure, aes(x= factor(year), fill = factor(q47a_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(time_pressure$q47a_new, 5)) ~ year, alternative = "two.sided",
            data = time_pressure, paired= FALSE)
wilcox.test(jitter(as.numeric(time_pressure$q47a_new, 5)) ~ year, alternative = "greater",
            data = time_pressure, paired= FALSE)
wilcox.test(jitter(as.numeric(time_pressure$q47a_new, 5)) ~ year, alternative = "less",
            data = time_pressure, paired= FALSE)
#W = 5179423864, p-value < 2.2e-16

########################
q36b = as.data.frame(select(data_2014, q36b_new, year))
q47b = as.data.frame(select(data_2020, q47b_new,year))
colnames(q36b) <- c('q47b_new','year') # Change name for q36b -> 47b

## combine 2 dataframes into 1 
work_freedom <- rbind(q36b,q47b,make.row.names=TRUE)

##change year as factor
work_freedom$year <- as.factor(work_freedom$year)              
levels(work_freedom$year)  ##check how many levels in the factor

## descriptive statistics and visualization
work_freedom%>% group_by(year,q47b_new) %>% count()
ggplot(work_freedom, aes(x= factor(year), fill = factor(q47b_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(work_freedom$q47b_new, 5)) ~ year, alternative = "two.sided",
            data = work_freedom, paired= FALSE)

wilcox.test(jitter(as.numeric(work_freedom$q47b_new, 5)) ~ year, alternative = "greater",
            data = work_freedom, paired= FALSE)
wilcox.test(jitter(as.numeric(work_freedom$q47b_new, 5)) ~ year, alternative = "less",
            data = work_freedom, paired= FALSE)
#W = 5270593894, p-value = 7.015e-14
##################################################################################
q36f = as.data.frame(select(data_2014, q36f_new, year))
q47e = as.data.frame(select(data_2020, q47e_new,year))

colnames(q36f) <- c('q47e_new','year') # Change name for q36f -> 47e

## combine 2 dataframes into 1 
work_choice <- rbind(q36f,q47e,make.row.names=TRUE)
View(work_choice)

##change year as factor
work_choice$year <- as.factor(work_choice$year)              
levels(work_choice$year)  ##check how many levels in the factor

## descriptive statistics and visualization
work_choice%>% group_by(year,q47e_new) %>% count()
ggplot(work_choice, aes(x= factor(year), fill = factor(q47e_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(work_choice$q47e_new, 5)) ~ year, alternative = "two.sided",data = test, paired= FALSE)
# W = 5381504453, p-value = 0.4581

############

q36e = as.data.frame(select(data_2014, q36e_new, year))
q47f = as.data.frame(select(data_2020, q47f_new,year))
colnames(q36e) <- c('q47f_new','year') # Change name for q36e -> 47f

## combine 2 dataframes into 1 
relationship <- rbind(q36e,q47f,make.row.names=TRUE)

##change year as factor
relationship$year <- as.factor(relationship$year)              
levels(relationship$year)  ##check how many levels in the factor

## descriptive statistics and visualization
relationship%>% group_by(year,q47f_new) %>% count()
ggplot(relationship, aes(x= factor(year), fill = factor(q47f_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(relationship$q47f_new, 5)) ~ year, alternative = "two.sided",
            data = relationship, paired= FALSE)
wilcox.test(jitter(as.numeric(relationship$q47f_new, 5)) ~ year, alternative = "greater",
            data = relationship, paired= FALSE)
wilcox.test(jitter(as.numeric(relationship$q47f_new, 5)) ~ year, alternative = "less",
            data = relationship, paired= FALSE)

#################################################################################################
# KRUSKAL WALLIS TEST
# for each year, conduct kruskal test
# classification level on salary
unique(data_2014$level_new)
level_remuneration_2014 <- select(data_2014, level_new, q18f_new)
ggplot(level_remuneration_2014, aes(x= factor(level_new), fill = factor(q18f_new))) + geom_bar(position=position_dodge())
kruskal.test(q18f_new ~ level_new, data = level_remuneration_2014)

# classification level on non salary
level_nonremuneration_2014 <- select(data_2014, level_new, q18g_new)
ggplot(level_nonremuneration_2014, aes(x= factor(level_new), fill = factor(q18g_new))) + geom_bar(position=position_dodge())
kruskal.test(q18g_new ~ level_new, data = level_nonremuneration_2014)

# classification level with coperation
level_cooperation_2014 <- select(data_2014, level_new, q19b_new)
level_cooperation_2014$level_new <- as.factor(level_cooperation_2014$level_new)
ggplot(level_cooperation_2014, aes(x= factor(level_new), fill = factor(q19b_new))) + geom_bar(position=position_dodge())
kruskal.test(q19b_new ~ level_new, data = level_cooperation_2014)

# classification level with JOB recognition
level_recognition_2014 <- select(data_2014, level_new, q18e_new)
level_recognition_2014$level_new <- as.factor(level_recognition_2014$level_new)
ggplot(level_recognition_2014, aes(x= factor(level_new), fill = factor(q18e_new))) + geom_bar(position=position_dodge())
kruskal.test(q18e_new ~ level_new, data = level_recognition_2014)

# classification level with attachment
level_engagement_2014 <- select(data_2020, level_new, q22a_new)
ggplot(level_engagement_2014, aes(x= factor(level_new), fill = factor(q22a_new))) + geom_bar(position=position_dodge())
kruskal.test(q22a_new ~ level_new, data = level_engagement_2014)

# classification level with commitment
level_commitment_2014 <- select(data_2020, level_new, q22a_new)
ggplot(level_commitment_2014, aes(x= factor(level_new), fill = factor(q22a_new))) + geom_bar(position=position_dodge())
kruskal.test(q22a_new ~ level_new, data = level_commitment_2014)

# classification level with SES talent development
level_SEStd_2014 <- select(data_2014, level_new, q21e_new)
ggplot(level_SEStd_2014, aes(x= factor(level_new), fill = factor(q21e_new))) + geom_bar(position=position_dodge())
kruskal.test(q21e_new ~ level_new, data = level_SEStd_2014)

# classification level with SES strategy direction
level_SESsd_2014 <- select(data_2014, level_new, q21g_new)
ggplot(level_SESsd_2014, aes(x= factor(level_new), fill = factor(q21g_new))) + geom_bar(position=position_dodge())
kruskal.test(q21g_new ~ level_new, data = level_SESsd_2014)

# classification level on work freedom
level_workfreedom_2014 <- select(data_2014, level_new, q36b_new)
ggplot(level_workfreedom_2014, aes(x= factor(level_new), fill = factor(q36b_new))) + geom_bar(position=position_dodge())
kruskal.test(q36b_new ~ level_new, data = level_workfreedom_2014)

# classification level on duties
level_duties_2014 <- select(data_2014, level_new, q36f_new)
ggplot(level_duties_2014, aes(x= factor(level_new), fill = factor(q36f_new))) + geom_bar(position=position_dodge())
kruskal.test(q36f_new ~ level_new, data = level_duties_2014)


# AGE #######################################################################################################
age_salary  <- select(data_2014, age_new, q18f_new)
age_salary$age_new <- as.factor(age_salary$age_new)
ggplot(age_salary, aes(x= factor(age_new), fill = factor(q18f_new))) + geom_bar(position=position_dodge())
kruskal.test(q18f_new ~ age_new, data = age_salary)

age_nonsalary  <- select(data_2014, age_new, q18g_new)
age_nonsalary$age_new <- as.factor(age_nonsalary$age_new)
ggplot(age_nonsalary, aes(x= factor(age_new), fill = factor(q18g_new))) + geom_bar(position=position_dodge())
kruskal.test(q18g_new ~ age_new, data = age_nonsalary)

#age on collaboration
age_cooperation  <- select(data_2014, age_new, q19b_new)
age_cooperation$age_new <- as.factor(age_cooperation$age_new)
ggplot(age_cooperation, aes(x= factor(age_new), fill = factor(q19b_new))) + geom_bar(position=position_dodge())
kruskal.test(q19b_new ~ age_new, data = age_cooperation)
# age and time pressure
age_timepres_2014 <- select(data_2014, age_new, q36a_new)
age_timepres_2014$age_new <- as.factor(age_timepres_2014$age_new)
ggplot(age_timepres_2014, aes(x= factor(age_new), fill = factor(q36a_new))) + geom_bar(position=position_dodge())
kruskal.test(q36a_new ~ age_new, data = age_timepres_2014)
#age on relationship
age_relationship_2014 <- select(data_2014, age_new, q36e_new)
age_relationship_2014$age_new <- as.factor(age_relationship_2014$age_new)
ggplot(age_relationship_2014, aes(x= factor(age_new), fill = factor(q36e_new))) + geom_bar(position=position_dodge())
kruskal.test(q36e_new ~ age_new, data = age_relationship_2014)
#####################################################
# GENDER on salary
gender_remuneration_2014 <- select(data_2014, gender_new, q18f_new)
ggplot(gender_remuneration_2014, aes(x= factor(gender_new), fill = factor(q18f_new))) + geom_bar(position=position_dodge())
kruskal.test(q18f_new ~ gender_new, data = gender_remuneration_2014)

# gender on non salary
gender_nonremuneration_2014 <- select(data_2014, gender_new, q18g_new)
ggplot(gender_nonremuneration_2014, aes(x= factor(gender_new), fill = factor(q18g_new))) + geom_bar(position=position_dodge())
kruskal.test(q18g_new ~ gender_new, data = gender_nonremuneration_2014)

# gender with coperation
gender_cooperation_2014 <- select(data_2014, gender_new, q19b_new)
gender_cooperation_2014$level_new <- as.factor(level_cooperation_2014$level_new)
ggplot(gender_cooperation_2014, aes(x= factor(gender_new), fill = factor(q19b_new))) + geom_bar(position=position_dodge())
kruskal.test(q19b_new ~ gender_new, data = gender_cooperation_2014)

#############################################################################################

level_remuneration_2020 <- select(data_2020, level_new, q17d_new)
ggplot(level_remuneration_2020, aes(x= factor(level_new), fill = factor(q17d_new))) + geom_bar(position=position_dodge())
kruskal.test(q17d_new ~ level_new, data = level_remuneration_2020)


# classification level on non salary
level_nonremuneration_2020 <- select(data_2020, level_new, q17e_new)
ggplot(level_nonremuneration_2020, aes(x= factor(level_new), fill = factor(q17e_new))) + geom_bar(position=position_dodge())
kruskal.test(q17e_new ~ level_new, data = level_nonremuneration_2020)


# classification level with JOB recognition
level_recognition_2020 <- select(data_2020, level_new, q17c_new)
ggplot(level_recognition_2020, aes(x= factor(level_new), fill = factor(q17c_new))) + geom_bar(position=position_dodge())
kruskal.test(q17c_new ~ level_new, data = level_recognition_2020)

# classification level with coperation
level_cooperation_2020 <- select(data_2020, level_new, q18b_new)
ggplot(level_cooperation_2020, aes(x= factor(level_new), fill = factor(q18b_new))) + geom_bar(position=position_dodge())
kruskal.test(q18b_new ~ level_new, data = level_cooperation_2020)

# classification level with attachment
level_engagement_2020 <- select(data_2020, level_new, q23g_new)
ggplot(level_engagement_2020, aes(x= factor(level_new), fill = factor(q23g_new))) + geom_bar(position=position_dodge())
kruskal.test(q23g_new ~ level_new, data = level_engagement_2020)

# classification level with SES talent development
level_SEStd_2020 <- select(data_2020, level_new, q21c_new)
ggplot(level_SEStd_2020, aes(x= factor(level_new), fill = factor(q21c_new))) + geom_bar(position=position_dodge())
kruskal.test(q21c_new ~ level_new, data = level_SEStd_2020)

# classification level with SES strategy direction
level_SESsd_2020 <- select(data_2020, level_new, q21d_new)
ggplot(level_SESsd_2020, aes(x= factor(level_new), fill = factor(q21d_new))) + geom_bar(position=position_dodge())
kruskal.test(q21d_new ~ level_new, data = level_SESsd_2020)

# classification level on work freedom
level_workfreedom_2020 <- select(data_2020, level_new, q47b_new)
ggplot(level_workfreedom_2020, aes(x= factor(level_new), fill = factor(q47b_new))) + geom_bar(position=position_dodge())
kruskal.test(q47b_new ~ level_new, data = level_workfreedom_2020)

# classification level on duties
level_duties_2020 <- select(data_2020, level_new, q47e_new)
ggplot(level_duties_2020, aes(x= factor(level_new), fill = factor(q47e_new))) + geom_bar(position=position_dodge())
kruskal.test(q47e_new ~ level_new, data = level_duties_2020)

# so sánh q30.36 of 2020 alone for covid
level_q30_2020 <- select(data_2020, level_new, q30_new)
ggplot(level_duties_2020, aes(x= factor(level_new), fill = factor(q47e_new))) + geom_bar(position=position_dodge())
kruskal.test(q47e_new ~ level_new, data = level_duties_2020)

##########################################################################################################################

# level on productivity during covid
level_productivity_2020 <- select(data_2020_final_v1, level_new, q30_new)
ggplot(level_productivity_2020, aes(x= factor(level_new), fill = factor(q30_new))) + geom_bar(position=position_dodge())
kruskal.test(q30_new ~ level_new, data = level_productivity_2020)

# gender on productivity during covid

gender_productivity_2020 <- select(data_2020_final_v1, gender_new, q30_new)
ggplot(gender_productivity_2020, aes(x= factor(gender_new), fill = factor(q30_new))) + geom_bar(position=position_dodge())
kruskal.test(q30_new ~ gender_new, data = gender_productivity_2020)

# age on productivity during covid

age_productivity_2020 <- select(data_2020_final_v1, age_new, q30_new)
ggplot(age_productivity_2020, aes(x= factor(age_new), fill = factor(q30_new))) + geom_bar(position=position_dodge())
kruskal.test(q30_new ~ age_new, data = age_productivity_2020)

# discriminated group on productivity during covid

unique(data_2020_final_v1$q30_new)

q58_new = as.factor(data_2020_final_v1$q58_new)

dis_pro = data_2020_final_v1%>% group_by(q58_new,q30_new) %>% count()

ggplot(t, aes(x= factor(q58_new), fill = factor(q30_new))) + geom_bar(position=position_dodge())

t = data_2020_final_v1%>% select(q58_new,q30_new) 

wilcox.test(jitter(as.numeric(t$q30_new, 5)) ~ q58_new, alternative = "two.sided",data = test, paired= FALSE)


# discriminated group (race) on well being during covid 19 & job satisfaction
q60.2_new = as.factor(data_2020_final_v1$q60.2_new)

race_wellbeing_covid= data_2020_final_v1%>% group_by(q60.2_new,q49_new) %>% count()
t1  = data_2020_final_v1%>% select(q60.2_new,q49_new) 

ggplot(t1, aes(x= factor(q60.2_new), fill = factor(q49_new))) + geom_bar(position=position_dodge())

wilcox.test(jitter(as.numeric(t1$q49_new, 5)) ~ q60.2_new, alternative = "two.sided",data = t1, paired= FALSE)

wilcox.test(jitter(as.numeric(t1$q49_new, 5)) ~ q60.2_new, alternative = "greater",data = t1, paired= FALSE)
#W = 113159242, p-value = 1

wilcox.test(jitter(as.numeric(t1$q49_new, 5)) ~ q60.2_new, alternative = "less",data = t1, paired= FALSE)
levels(q60.2_new)

# [1] "0" "1" --> Non-race discriminated people has lower mean ranks than race discriminated one

disability_wellbeing_covid = data_2020_final_v1%>% group_by(q60.3_new,q49_new) %>% count()
t2  = data_2020_final_v1%>% select(q60.3_new,q49_new) 

ggplot(t2, aes(x= factor(q60.3_new), fill = factor(q49_new))) + 
  geom_bar(position=position_dodge()) # PP k cùng shape

# discriminated group (gender) on well being during covid 19 & job satisfaction
q60.1_new = as.factor(data_2020_final_v1$q60.1_new)
gender_wellbeing_covid = data_2020_final_v1%>% group_by(q60.1_new,q49_new) %>% count()
t3  = data_2020_final_v1%>% select(q60.1_new,q49_new) 

ggplot(t3, aes(x= factor(q60.1_new), fill = factor(q49_new))) + 
  geom_bar(position=position_dodge()) 

wilcox.test(jitter(as.numeric(t3$q49_new, 5)) ~ q60.1_new, alternative = "two.sided",data = t3, paired= FALSE)
#W = 161939729, p-value < 2.2e-16
wilcox.test(jitter(as.numeric(t3$q49_new, 5)) ~ q60.1_new, alternative = "greater",data = t3, paired= FALSE)
#W = 113159242, p-value = 1
wilcox.test(jitter(as.numeric(t3$q49_new, 5)) ~ q60.1_new, alternative = "less", data = t3, paired= FALSE)
#W = 161716892, p-value < 2.2e-16

levels(q60.1_new)

# discriminated group on job satisfaction

# SKILL & CAPABILITIES 
# level vs 40.1 prof data skill
q40.1_new = as.factor(data_2020_final_v1$q40.1_new)
level_prof_data = data_2020_final_v1%>% group_by(q40.1_new,level_new) %>% count()
t4  = data_2020_final_v1%>% select(q40.1_new,level_new) 

ggplot(t4, aes(x= factor(q40.1_new), fill = factor(level_new))) + 
  geom_bar(position=position_dodge()) 

wilcox.test(jitter(as.numeric(t4$level_new, 3)) ~ q40.1_new, alternative = "two.sided",data = t4, paired= FALSE)
# W = 442555796, p-value < 2.2e-16

# wilcox.test(jitter(as.numeric(t4$q40.1_new, 2)) ~ level_new, alternative = "two.sided",data = t4, paired= FALSE)

wilcox.test(jitter(as.numeric(t4$level_new, 3)) ~ q40.1_new, alternative = "greater",data = t4, paired= FALSE)
#W = 442407569, p-value = 1
wilcox.test(jitter(as.numeric(t4$level_new, 3)) ~ q40.1_new, alternative = "less", data = t4, paired= FALSE)
##W = 161716892, p-value < 2.2e-16

levels(q40.1_new)
#[1] "0" "1": nhom tra loi thieu prof data skill co mean ranks thap hon nhóm tl là KHONG thieu prof data skill

