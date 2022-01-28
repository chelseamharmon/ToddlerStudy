# ToddlerStudy
Study examining Classical Conditioning Reward Learning in the presence versus absence of a parent. Collaboration with Barnard Toddler Center 

See figures file: figures01.20.2022.pptx

The goal of this study was to understand how toddlers learn about rewards in different social contexts (i.e. sitting with their parent or without them). In study 1 we used cute, cuddly animal photos as our rewarding stimuli and asked if toddlers learned an association with a neutral shape (compared to a shape with no reward pairing) better with their parent sitting next to them or a familiar teacher sitting next to them (SEE FIGURE 1 FOR STUDY DESIGN). We found an age by social context interaction (see Figure 2 for result), such that older toddlers were more likely to prefer the neutral shape paired with the reward when learning took place next to their parent compared to with a familiar teacher. However, older toddlers were more likely to be paired with a familiar teacher (SEE FIGURE 3 FOR RESULT) which may have confounded our primary finding. Given these ambiguious results, we developed study 2 to ask if improving the study paradigm increased learning with younger toddlers, who showed no learning at all (across both social contexts) in study 1. We improved the experiment by using a highchair, a warm-up activity, and a more interesting (and thus more saliently rewarding) stimulus, video clips of a popular childrens show (Paw Patrol)(SEE FIGURE 4 FOR STUDY DESIGN). We found in study 2 that the order of learning (whether toddlers learned first with their parent in the room or first with their parent out of the room) determined learning outcomes such that toddlers who learned with their parents out of the room first demonstrated a statistically significant disliking of the neutral shape paired with the reward (SEE FIGURE 5 FOR RESULTS). We conclude that social context, in the form of parental presence, changes how toddlers learn reward associations in a new environment. Older toddlers are more likely to show learned preference for rewards when their parent is present with them and younger toddlers are sensitive to parental absence which can flip preference for reward associations to aversion (fear) of reward associations. These findings have implications for various learning environments toddlers encounter such as early educational settings (e.g., preschool, child care), the doctor's office, and the home.

Data anlaysis pipeline for paper to follow (in R)

title: "Social Modulation of Reward Learning in Toddlerhood" 
author: "Chelsea Harmon, Emma Routhier, Hannah Dunn, Sophia Golden, Sarah Marcotte, Laura MacMullin, Tovah Klein Nim Tottenham" 
date: "July 1, 2020" 
output: html_document:

```.r
#```{r, echo=F, include=F, eval=F, warning=F}
knitr::opts_chunk$set(echo = TRUE)
#library(papaja)
require(ggplot2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plyr)
library(psych)
library(rstanarm)
library(brms)
library(nlme)
library(grid)
library(gmodels)
library(plotrix) 
library(effects)
library(lme4)
library(lmerTest)
library(car)
library(rstanarm)
library(boot)
library(ggsignif)
library(knitr)
library(kableExtra)
library(car)



mytheme = theme_minimal(base_size =20) +
    theme(legend.position = 'right',
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "gray98", color = NA),
          #strip.text.x = element_blank(),
          #panel.margin = unit(c(.1, .1, .1, .1), "cm"),
          axis.line = element_line(colour = "grey50"), 
          #panel.border = element_rect(color="black", fill=NA),
          text = element_text(family="Helvetica", size=12),
          axis.text.x = element_text(size=20, margin=unit(c(2,0,0,0), "mm")), #changed from size=8
          axis.text.y = element_text(size=20, margin=unit(c(0,2,0,0), "mm")), #changed from size=8
          axis.ticks = element_line(size = .3),
          axis.ticks.length=unit(-1, "mm"))+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
      theme(plot.margin = unit(c(1,.1,.1,.1), "cm"))
theme_set(mytheme)

#Reading study data file
studyData <- read.csv("Toddler_HT_DataEntryMaster_01.17.2019PlusDemographics.csv", stringsAsFactors = FALSE)
studyData$subject_number <- 1:145 #Numbering participants
studyData$study <- ifelse(studyData$subject_number< 98, "study1", "study2") #seperating data file into Study 1 and Study 2 

#Adding demographic data
demographs1 <- read.csv("Y-Maze_Subject_Demographics2018_Ordered.csv")
demographs2 <- read.csv("Y_Maze_Study_2019_Demographics_Ordered.csv")

demographs <- rbind(demographs1[1:5], demographs2[1:5])
Ethnicity <- rbind(demographs1[7], demographs2[7])
parent1Ed <- rbind(demographs1[9], demographs2[12])
parent2Ed <- rbind(demographs1[10], demographs2[13])
demographs <- cbind(demographs, Ethnicity, parent1Ed, parent2Ed)

studyData <- as.data.frame(studyData)
study1 <- studyData[which(studyData$study=="study1"),]
#min(study1$Age_Months)
#max(study1$Age_Months)

#Mean Centering Age and Gender for models 
studyData$Age_MonthsMeanCentered <- studyData$Age_Months-mean(studyData$Age_Months) #This also works the same
studyData$GENDERMeanCentered <- studyData$GENDER - mean(studyData$GENDER)

study2 <- studyData[which(studyData$study=="study2"),]
#min(study2$Age_Months)
#max(study2$Age_Months)
#describe(study2$Age_Months)


study2$Age_MonthsMeanCentered <- study2$Age_Months-mean(study2$Age_Months)
study2$GENDERMeanCentered <- study2$GENDER - mean(study2$GENDER)
study1_meanAge <-mean(study1$Age_Months)
#sd(study1$Age_Months)
#sd(study2$Age_Months)
studyData$Gender <- ifelse(studyData$GENDER==0, "Male", "Female")

#For reporting demographic data
age_demo <- studyData %>% #wideNewNonNASubjects
  group_by(study) %>%
  summarise_at(vars(Age_Months), funs(mean(., na.rm=TRUE), min(., na.rm=T), max(.,na.rm=T))) 
  #summarise_at(vars(Age_Months), funs(mean(., na.rm=TRUE), sd(., na.rm=T),se=sd/sqrt(n())))
age_demo <- as.data.frame(age_demo)

#genderCounts<- (as.data.frame(count(studyData$GENDER)[2],  c( "M", "F", "NA")))
#genderCounts <- as.data.frame(count(studyData$GENDER)[2],  c( "M", "F", "NA")) %>% 
#  dplyr::rename_all(funs(str_replace(., "Gender", "Count")))
#(genderCounts)
#names(genderCounts)[c(1,2)] <- c("Gender", "Count")


gen_demo <- studyData %>% 
  group_by(study) %>%
  dplyr::count(Gender)

gen_demo<- spread(gen_demo,Gender, n)
demo <- merge(age_demo, gen_demo)
age_demo$years <- (age_demo$mean)/12
age_demo$min_years  <- (age_demo$min)/12
age_demo$max_years <-  (age_demo$max)/12

demo$mean <- c("31.8", "29.6") #2.6, 2.5
demo$min <- c("24", "23")  #2, 1.9
demo$max <- c("43", "37") #3.6, 3.1
#age_demo$mean <- paste(c(age_demo$mean, age_demo$years))



#names(flexdat)[c(5,6)]<-c("ROI","flex")

#names(demo)[1:4] <- c("study", "", "", "")

#gender_dem <- studyData %>%
#  group_by(study) %>%
#  tally(vars(GENDER))

#Parent gender
table(study1$GENDER_CAREGIVER)

table(study2$GENDER_CAREGIVER)
#options(knitr.kable.NA = '-') 
studyDataDems <- merge(studyData, demographs, by="SUB_ID")
#unique(studyDataDems$EthnicityCoded)

studyDataDems$EthnicityCoded <- dplyr::recode(studyDataDems$Ethnicity, "Caucasian" ="White/European-American", 
              "Asian-American/Caucasian"="Asian-American,White/European-American", #Can we assume every Asian is Asian american? 
              "Hispanic"="Hispanic/Latinx", 
              "Korean"="Asian-American", 
              "Latino"="Hispanic/Latinx", 
              "Chinese"="Asian-American", 
              "African-American"="Black/African-American",
              "Indian/Jewish/Italian"="Asian-American,White/European-American",  #should this be Asian-American?
              "Indian/Caucasian"="Asian-American,White/European-American", #should this be Asian-American?
             "Mixed Race (Caucasian/Black/Southeast Asian)"="White/European-American,Black/African-American,Asian-American", 
              "Asian-Indian/Caucasian"="Asian-American,White/European-American",  #should this be Asian-American?
              "White/Mexican"="White/European-American,Hispanic/Latinx", 
              "Afro-Puerto Rican/Haitian-American"="Black/African-American", 
              "Irish and Greek-American"="White/European-American", 
              "Arab American"="White/European-American", 
              "Asian/Caucasian"="Asian-American,White/European-American", #should this be Asian-American?
              "Portuguese"="White/European-American", #Does not indiciate american 
              "Navajo Native American/White"="Native American,White/European-American", #Do we want a Native American Category?
              "Indian and Caucasian"="Asian-American,White/European-American", #should this be Asian-American?
              "Swedish-American"="White/European-American", 
              "Afro-Latina/Caucasian/Native American"="Black/African-American,White/European-American,Native American,Hispanic/Latinx", #right? 
              "Italian/Taiwanese"="White/European-American,Asian-American", 
              "Indian/South-Asian"="Asian-American", 
              "Caucasian/African-American"="White/European-American,Black/African-American", 
              "Black"="Black/African-American",
              "Asian/Korean"="Asian-American", 
              "Latino/Arab"="White/European-American,Hispanic/Latinx", 
              "European/Afro-Caribbean"="White/European-American,Black/African-American", 
              "Hispanic/Caucasian"="Hispanic/Latinx,White/European-American", 
              "Cauasian"="White/European-American", 
              "Caucasian/Latin"="White/European-American,Hispanic/Latinx",
              "Korean Bangladeshi, Scottish & English"="Asian-American,White/European-American", 
              "African American/Puerto Rican, Irish/zech"="Black/African-American,Hispanic/Latinx,White/European-American", #Should this be "American"
              "European/Afro-Caribbean "="White/European-American,Black/African-American", #Should this be "American"
              "Japanese/Caucasian"="Asian-American,White/European-American", #should this be Asian-American 
              "Pakistani"="White/European-American", #Should this be "American"
              "Black "="Black/African-American", #should this be "American"
              "Black American "="Black/African-American", 
              "White"="White/European-American", 
              "White/Hispanic"="White/European-American,Hispanic/Latinx",
              "Hungarian/Chinese"="White/European-American,Asian-American", #Should this be "American"
              "African American"="Black/African-American",
              "Indian (South Asian)"="Asian-American", #should this be "American"
              "Latino/Hispanic"="Hispanic/Latinx",
              "White/South Asian"="White/European-American,Asian-American", #Should this be "American"
              "Muslim"="White/European-American",
              "White & South Asian"="White/European-American,Asian-American", #should this be "American"
              "mixed Afro-Latino, Native American, mixed Western European"="Black/African-American,Native American,White/European-American",
              "Caucasian/Mixed"="White/European-American", 
             "Caucasian/African American"="White/European-American,Black/African-American", 
             "Asian"="Asian-American", 
             "Polish & Irish"="White/European-American",
             "Indian, Puerto Rican, English, Polish, Austrian"="White/European-American,Hispanic/Latinx,Asian-American"
             )

#studyDataDems_sep <- (separate(studyDataDems, EthnicityCoded, c("Ethnicity.a", "Ethnicity.b", "Ethnicity.c", "Ethnicity.d")))

#Important: Participants can endorse more than 1 category so % may be greater than 100 

studyDataDems$EthnicityCoded <- as.character(studyDataDems$EthnicityCoded)

studyDataDems_sep<- studyDataDems %>% 
  mutate(EthnicityCoded = strsplit(as.character(EthnicityCoded), ",")) %>% 
    unnest(EthnicityCoded)
ethno_demo <- studyDataDems_sep %>% 
  group_by(study) %>%
  dplyr::count(EthnicityCoded)

ethno_demo<- data.frame(ethno_demo, perc=c((ethno_demo$n[ethno_demo$study=="study1"]/96)*100, (ethno_demo$n[ethno_demo$study=="study2"]/47)*100))

ethno_demo1 <- ethno_demo[-3]

ethno_spread <- spread(ethno_demo1, EthnicityCoded, perc, drop=TRUE)

Nums <- count(studyDataDems$study)
names(Nums)[1] <-"study"
Nums$study <- as.character(Nums$study)

demo1 <- merge(demo, ethno_spread)

demo2 <- merge(Nums, demo1)

ethno_demo
#unique(studyDataDems$Parent.1.Highest.Level.of.Education)

studyDataDems$Parent.1.Highest.Level.of.Education.Coded <- car::recode(studyDataDems$Parent.1.Highest.Level.of.Education, "'Highschool'=1; 'Some College'=2;  'Some college'=2; 'BA'=3; 'BS'=3; 'BSM'=3; 'BA (pursuing PhD)'=3; 'CFA'=3; 'MA'=4; 'MS'=4; 'MFA'=4; 'MBA'=4; 'MPA'=4; 'MSW'=4; 'MSW '=4; 'Masters'=4; 'MEng'=4; 'Mphil'=4; 'MA, HSW'=4; '2 Masters'=4; 'Master/PhD in progress'=4; 'PHD, in progress'=4;'MSN'=4; 'Psychoanalytic candidate'=4; 'PhD'=5; 'MD'=5; 'JD'=5; 'DMA'=5; 'JD, MA'=5; 'DMD'=5; 'MBA, JD'=5;  'JD, LLM'=5;  'Md, PHD'=5; else='NA'")

studyDataDems$Parent.2.Highest.Level.of.Education.Coded <- car::recode(studyDataDems$Parent.2.Highest.Level.of.Education, "'Highschool'=1; 'High school'=1; 'Some College'=2; 'Some college'=2;'some college'=2;'2 yrs college'=2; 'AAS'=2;  'BS, in progress'=2; 'BA'=3; 'BS'=3; 'BS '=3;'BComm'=3; 'BS+Exec'=3; 'BS, CFA'=3; 'BSE'=3; 'BM'=3;'MA'=4; 'MS'=4; 'MArch'=4; 'MPA'=4;'Masters'=4; 'MA (Pursuing PhD)'=4;  'MBA'=4; 'MFA'=4;'MD'=5; 'JD'=5; 'DMA'=5; 'PhD'=5; 'JD, MA'=5; 'DPT'=5; 'MD, PHD'=5; 'JD, MBA'=5; else='NA'")


studyDataDems$Parent.1.Highest.Level.of.Education.Coded.Numeric <- (as.numeric(as.character(studyDataDems$Parent.1.Highest.Level.of.Education.Coded), stringAsFactor=F))


studyDataDems$Parent.2.Highest.Level.of.Education.Coded.Numeric <- (as.numeric(as.character(studyDataDems$Parent.2.Highest.Level.of.Education.Coded), stringAsFactor=F))

      
studyDataDemsStudy1 <- studyDataDems[which(studyDataDems$study=="study1"),]
studyDataDemsStudy2 <- studyDataDems[which(studyDataDems$study=="study2"),]

as.numeric(studyDataDemsStudy1$Parent.1.Highest.Level.of.Education.Coded, stringAsFactor=T)
(as.numeric(as.character(studyDataDemsStudy1$Parent.1.Highest.Level.of.Education.Coded), stringAsFactor=F))


edu <- data.frame("study"= c("study1", "study2"), "Parent.1.Mode"=c("Master's Degree", "Master's Degree"), "Parent.1.Range"=c("Some College - Professional Degree", "Some College - Professional Degree"), "Parent.2.Mode"=c("Bachelor's Degree", "Bachelor's Degree"), "Parent.2.Range"=c("High School - Professional Degree", "High School - Professional Degree"))

#Would this better be depicted as broken down percentages ; currently it is not very informative 

demo3
demo3 <- merge(demo2, edu)

demo3Study1 <- demo3[which(demo3$study=="study1"),]


#demo3$study <- ifelse(demo$study=="study1", "Study 1", "Study 2")
demo3Study1$study <- "Study 1"
names(demo3Study1)[1:3] <- c("", "", "")
names(demo3Study1)[13:16] <- c("Parent 1 Mode", "Range", "Parent 2 Mode", "Range")


demo3Study1 %>%
  kable(digits=2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width=F, position="center") %>%
  add_header_above(c("", "N", "Mean Age in Months", "Range"=2, "Gender"=2, "Race/Ethnicity (percentage)"=5, "Parental Education"=4))

#We didn't ask primary language in second study - most children spoke english, I think 1 had trouble in the frist study understanding but may not have had complete data if I remember correctly. 84.5%had english as primary language 6.2% Spoke english and another language & 9.3% had another language as their primary language 

#Here is the information you requested about the percentage of families who received aid/sliding scale tuition for these years: 
#2016-17: 38%
#2017-18: 42%
#2018-19: 41% 


#study1 average= 
(38+42)/2

#study2= 
41
Table 1. **Demographics.** Note: Race/Ethnicity totals exceed 100% to allow for participants to endorse more than one category. 

Figure 2. **Conditioning and Testing Paradigm.** Figure 2A. Conditioning phase. Children were presented with shapes on a computer screen. One shape was reinforced with the US 50% of the time, shapes were randomized between participants. 
Figure 2B. Testing phase. After conditioning, children were then brought to a play tent where a teacher pointed to both of the doors and each child was told behind each shape was the same prize (e.g. a sticker/small toy). The child was prompted to choose which door they wanted to retrieve his/her prize from. Each child completed 5 randomized trials before returning to the computer to complete the next condition (parent or experimenter). Order of social context was randomized between participants. 
 

#Questionnaire data - Study 1 
```.r
#```{r, echo=F, include=F, eval=F, warning=F}

quest <- read.csv("questionnaire_dataStudy1_8.8.20.csv")
length(quest$Participant)
describe(quest$Involvement)
describe(quest$PositiveParenting)
describe(quest$InconsistentDiscipline)
describe(quest$HarshPunishment)

#questNoNA <- quest %>% drop_na(Involvement) #has to be done at the questionnaire level 
quest$Involvement < 35
describe(quest$PositiveParenting)
describe(quest$InconsistentDiscipline)
describe(quest$HarshPunishment)

#Involvement 10 items M = 40.79; SD = 4.39 ; problem cutoff 35
#Poor Supervision 10 items M = 1.88; S.D. = 2.70
#Positive Parenting 6 items M = 26.05; S.D. = 2.64 # positive parenting is a little bit lower in our sample ??????????
#Inconsistent Discipline 6 items M = 13.83; S.D. = 3.41
#Punishment 3 items M = 5.62; S.D. 1.61

#Means (s.d.)
describe(quest$ActivityLevel.Energy) #24 months = 4.90 (.65); 30 months = 	4.94 (.72), 36 months=	4.75 (.77)
describe(quest$AttentionalFocusing) #4.38 (.71)	4.61 (.82)	4.76 (.79)
describe(quest$AttentionalShifting) #4.59 (.68)	4.72 (.65)	4.79 (.62)	
describe(quest$Cuddliness) #4.94 (.78)	5.02 (.79)	5.21 (.82)
describe(quest$Discomfort) #24 months = 2.26 (.90); 30 months = 2.55 (1.02); 36 months	2.73 (1.10)
describe(quest$Fear) #24 months = 2.33 (.87); 30 months =	2.52 (.96); 36 months =	2.80 (.99)
describe(quest$Frustration) #3.56 (.77)	3.66 (.88)	3.65 (.93)
describe(quest$HighIntensityPleasure) #4.95 (.92)	5.14 (.87)	5.07 (1.03)
describe(quest$Impulsivity) #5.02 (.75)	4.93 (.72)	4.78 (.69)
describe(quest$InhibitoryControl) #3.86 (.97)	4.08 (.94)	4.20 (.99)
describe(quest$MotorActivation) #24 months = 2.08 (.76); 30 months =	2.12 (.90); 36 months =	2.03 (.75)
describe(quest$PerceptualSensitivity) #3.94 (1.04)	4.14 (1.05)	4.26 (1.19)
describe(quest$PositiveAnticipation) #4.86 (.93)	5.15 (.81)	5.17 (.77)
describe(quest$LowIntenistyPleasure) #4.95 (.76)	4.88 (.70)	4.89 (.71)
describe(quest$Sadness) #2.65 (.91)	2.75 (.90)	2.82 (.92)
describe(quest$Shyness) #3.20 (.91)	3.21 (.95)	3.52 (.85)
describe(quest$Sociability) #5.57 (.92)	5.74 (.85)	5.81 (.87)
describe(quest$Soothabilty) # 5.54 (.78)	5.46 (.74)	5.34 (.76)

#quest$Soothability

data <- read.csv("Toddler_HT_DataEntryMaster_01.17.2019PlusDemographics.csv", stringsAsFactors = FALSE)
data$subject_number <- 1:145
data$study <- ifelse(data$subject_number< 98, "study1", "study2")

demo <- read.csv("Y_Maze_Study_2019_Demographics.csv")
data <- as.data.frame(data)

study1 <- data[which(data$study=="study1"),]

min(study1$Age_Months)
mean(study1$Age_Months)
max(study1$Age_Months)


data$Age_MonthsMeanCentered <- data$Age_Months-mean(data$Age_Months) #This also works the same
data$GENDERMeanCentered <- data$GENDER - mean(data$GENDER)


#Study 2 
study2 <- data[which(data$study=="study2"),]
min(study2$Age_Months)
mean(study2$Age_Months)
max(study2$Age_Months)
describe(study2$Age_Months)


study2$Age_MonthsMeanCentered <- study2$Age_Months-mean(study2$Age_Months)
study2$GENDERMeanCentered <- study2$GENDER - mean(study2$GENDER)

#unique(study1$SUB_ID)
study2$ExperimenterPercentScore <- study2$EXPERIMENTER_TOTALSCORE/5
study2$ParentPercentScore <- study2$PARENT_TOTALSCORE/5
meanDataLong <- read.csv("MeanDataLong.csv")

#Turning Data into long version 
study1_long <- gather(study1, key="Trial", value="Score", COND1_1:COND2_5)


study1_long <- dplyr::mutate(study1_long,
                         type = case_when(
                           CONDITION == 0 & grepl('COND1', Trial) ~ 'E',
                           CONDITION == 0 & grepl('COND2', Trial) ~ 'P',
                           CONDITION == 1 & grepl('COND1', Trial) ~ 'P',
                           CONDITION == 1 & grepl('COND2', Trial) ~ 'E'
                         )) 

study1_long <- dplyr::mutate(study1_long,
                         TrialNumber = case_when(
                           grepl('_1', Trial) ~ 1,
                           grepl('_2', Trial) ~ 2,
                           grepl('_3', Trial) ~ 3,
                           grepl('_4', Trial) ~ 4,
                           grepl('_5', Trial) ~ 5
                           #grepl('_6', Trial) ~ 1,
                           #grepl('_7', Trial) ~ 2,
                           #grepl('_8', Trial) ~ 3,
                           #grepl('_9', Trial) ~ 4,
                           #grepl('_10', Trial) ~ 5
                         )) 
#                         )) %>% select(CONDITION, Trial, type, Score)

study1_long <- study1_long %>% drop_na(Score)

study1_long$GENDERMeanCentered <- study1_long$GENDER - mean(study1_long$GENDER)
study1_long$GENDERCoded <- ifelse(study1_long$GENDER == 0, -0.5, 0.5) 

study1_long$Age_MonthsMeanCentered <- study1_long$Age_Months-mean(study1_long$Age_Months) #This also works the same 

#length(study1_long$SUB_ID)/10
#table(study1_long$SUB_ID)

study1_exclude_subs <- (study1_long$SUB_ID) %in% c("HT005", "HT067", "HT068", "HT071", "HT085", "HT089", "HT094", "HT096") 
study1_longNoNA <- study1_long[!study1_exclude_subs,]

study1_exclude_subs_wide <- (study1$SUB_ID) %in% c("HT005", "HT067", "HT068", "HT071", "HT085", "HT089", "HT094", "HT096") 

study1_NoNA <- study1[!study1_exclude_subs_wide,]

study1_means_long <- gather(study1, key="CONDITION", value="Score", PARENT_TOTALSCORE:EXPERIMENTER_TOTALSCORE)
study1_means_long$type <- ifelse(study1_means_long$CONDITION=="PARENT_TOTALSCORE", "P", "E")
study1_means_long$Age_MonthsMeanCentered <- study1_means_long$Age_Months-mean(study1_means_long$Age_Months) 

study1_longNoNA$Type <- ifelse(study1_longNoNA$type=="P", 0.5, -0.5)


study1_m1.5 <- glmer(Score~Type + 
                GENDERCoded + 
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*Type + 
                (Type|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m1.5)
coef(summary(study1_m1.5))


#THESE ARE INCORRECT BECUASE INTERCEPT WAS NOT ADDED AND SHOULD MULTIPLY ESTIMATE FOR TYPE (SOCIAL CONTEXT) BY -0.5 AND 0.5 APPROPRIATELY - fixed later in the script 
#inv.logit(0.16213989) #Type E =-0.5 (not adding to intercept)
#inv.logit(0.16213989+0.13664424) #Type P 
#inv.logit(0.01658546) 
#inv.logit(0.08849817) #interaction for type E

#inv.logit(0.08849817 + 0.16213989) #for the mean age,the effect of Condition E is E coef + interaction*1
#inv.logit(0.16213989+0.13664424 + 0.16213989*(0.16213989+0.13664424)) #for the mean age,the effect of Condition P is P coef + intercept + interaction*1*ConditionP

#inv.logit(0.16213989*(0.16213989+0.13664424)) #interaction term for condition P 

#inv.logit(-0.11347801) #gender


#This is the correct way of doing it 
inv.logit(0.13664424)
inv.logit(0.13664424 + 0.16213989*0.5)
inv.logit(0.13664424 + 0.16213989*-0.5)

inv.logit(-1.08)
inv.logit(1.08)

#Testing for rating preferences 
t.test(study1$ParentRatingPreference, study1$ExperimenterRatingPreference, paired=TRUE, na.rm=T)

count(study2$ParentRatingPreference)
count(study2$ExperimenterRatingPreference)
mean(study2$ExperimenterRatingPreference, na.rm=T)
t.test(study2$ParentRatingPreference, study2$ExperimenterRatingPreference, paired=TRUE, na.rm=T)


count(study1$ParentRatingPreference)
count(study1$ExperimenterRatingPreference)

36+55 #Parent rating preference
32+53 #Experimenter rating preference

36/91
55/91

32/85
53/86


#study 2
24+23 = 47 #partinng ratinng 
21+21=42 #experimenter rating 

23/47 #Parent CS+ 
24/47 #Parent CS- 

21/42 #Parent CS+
21/42 #Experimenter CS-
#plug in ages to get prediction for E an P position 
#effect_Type
#Testing recall with glmer model Study 1


data_recall_long <- gather(data, key = "socialContext", value="RecallScore", ParentRatingPreference:ExperimenterRatingPreference)
head(data_recall_long)

#study1_recall_long$GENDERCoded <- ifelse(study1_recall_long$GENDER == 0, -0.5, 0.5) 
study1_recall <- data_recall_long[which(data_recall_long$study=="study1"),]
#View(study1_recall[c("SUB_ID","socialContext", "RecallScore")])

study1_recall_exclude_subs <- (study1_recall$SUB_ID) %in% c("HT005", "HT067", "HT068", "HT071", "HT085", "HT089", "HT094", "HT096","HT063", "HT066", "HT015", "HT056", "HT061", "HT074", "HT081", "HT092") #these subject are missing one or more recall test HT005, HT063, HT066, HT085, HT094, HT096, HT015, HT056, HT061, HT068, HT071, HT074, HT081, HT089, HT092 these are missing one or more condition "HT005", "HT067", "HT068", "HT071", "HT085", "HT089", "HT094", "HT096"
study1_recall_noNA <- study1_recall[!study1_recall_exclude_subs,]

head(study1_recall_noNA)

study1_recall_noNA$socialContext <- ifelse(study1_recall_noNA$socialContext=="ParentRatingPreference", 0.5, -0.5)


m_recall1 <- glmer(RecallScore ~ socialContext + 
                     GENDERMeanCentered + 
                     Age_MonthsMeanCentered + 
                     (socialContext | SUB_ID), 
                   data=study1_recall_noNA, family=binomial)

summary(m_recall1)


#ggplot(data=study1_recall_noNA, aes(socialContext, RecallScore, group=SUB_ID, colour=SUB_ID)) + 
#  geom_jitter() #+
  #geom_line(aes(socialContext, RecallScore, group=SUB_ID))


inv.logit(0.39351) #baseline preference for CS+ 

inv.logit(0.39351+-0.16272*0.5) #Parent present 
inv.logit(0.39351+-0.16272*-0.5) #Parent Absent

#Converting binomial to B value
#Getting Estimate for parent for average gender and age child 
#invlogit(EstimateIntercept + EstimateSocialContextParentRatingPreference + EstimateGENDERMeanCentered*0 + Age_MonthsMeanCentered*0) 

#Getting Estimate for teacher for average gender and age child 
#invlogit(EstimateIntercept + EstimateSocialContextParentRatingPreference*0 + EstimateGENDERMeanCentered*0 + Age_MonthsMeanCentered*0) 

?isSingular
#Testing recall with glmer model Study 2


study2_recall <- data_recall_long[which(data_recall_long$study=="study2"),]
(study2_recall[c("SUB_ID","socialContext", "RecallScore")])


study2_recall_exclude_subs <- (study2_recall$SUB_ID) %in% c("HT111", "HT123", "HT128", "HT131", "HT100", "HT117", "HT119")  #these subject are missing one or more recall test "HT100", "HT111", "HT117", "HT119", "HT128", "HT131" these are missing one or more condition "HT111", "HT123", "HT128", "HT131"
study2_recall_NoNA <- study2_recall[!study2_recall_exclude_subs,]

m_recall2 <- glmer(RecallScore ~ socialContext + GENDERMeanCentered + Age_MonthsMeanCentered + (socialContext | SUB_ID), data=study2_recall_NoNA, family=binomial)

summary(m_recall2)
inv.logit(0.002437) #intercept which is the Experimenter Rating PReference for CS+ Condition 
inv.logit(0.002437 + -0.008114) #intercept plus estimate for parent rating preference which gives us parental presence estiimate prediction 
#prop.test(c(study2$ParentRatingPreference,study2$ExperimenterRatingPreference),c(n1,n2),alternative)

inv.logit(0)
inv.logit(0 + -0.008114)
#Testing with P as 0 value 
study1_longNoNA$TypeInv <- ifelse(study1_longNoNA$type=="E", 0.5, -0.5)


study1_m1.6 <- glmer(Score~TypeInv + 
                GENDERCoded + 
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*TypeInv + 
                (TypeInv|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m1.6)

coef(summary(study1_m1.5))
coef(summary(study1_m1.6))



study1_m1.7 <- glmer(Score~type + 
                GENDERCoded + 
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*type + 
                (type|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m1.7)
coef(summary(study1_m1.7))
coef(summary(study1_m1.5))
coef(summary(study1_m1.6))

inv.logit(0.05557589) #intercept for type P

inv.logit(0.16213989) #Type P =-0.5 (not adding to intercept)
inv.logit(0.16213989+0.13664424) #Type P 
inv.logit(0.08849817) 



inv.logit(-0.16213989) #Type P =-0.5 (not adding to intercept)
inv.logit(-0.16213989+0.13664424) #Type P 
inv.logit(-0.08849817) 

study1_m1.6

effect_TypeP <- as.data.frame(effect('TypeInv:Age_MonthsMeanCentered', 
                                    study1_m1.6, 
                                    xlevels=list(TypeInv= c(-0.5, 0.5), 
                                                 Age_MonthsMeanCentered=
                                                   seq(min(data$Age_MonthsMeanCentered, na.rm=T),
                                                       max(data$Age_MonthsMeanCentered, na.rm=T), .1))))
effect_Type$type <- ifelse(effect_Type$Type==-0.5, "P", "E")


fitLmePlot <- ggplot(data=effect_TypeP, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
    scale_color_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Teachher","Parent"), values = c("seagreen2","blue")) +
    labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 1') +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
fitLmePlot

#checking predictions (they are the same with +5 -5 Age but not when mean centered, I'm probably not adding correctly when interpreting the coefficients)
#View(effect_TypeP)



#Models - 
study1_longNoNA$CONDITION_Coded <- ifelse(study1_longNoNA$CONDITION==0, -0.5, 0.5)
study1_longNoNA$Gender_Parent_Coded <- ifelse(study1_longNoNA$GENDER_CAREGIVER==0, -0.5, 0.5)
study1_longNoNA$InExperimenterClassCoded <- ifelse(study1_longNoNA$InExperimenterClass==0, -0.5, 0.5)
table(study1_longNoNA$GENDER_CAREGIVER)

study1_m2 <- glmer(Score~Type + 
                GENDERCoded + 
                CONDITION_Coded +
                Gender_Parent_Coded +
                InExperimenterClassCoded +
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*Type + 
                (Type|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m2)
coef(summary(study1_m2))
inv.logit(coef(summary(study1_m2)))


study1_m2.7 <- glmer(Score~type + 
                GENDERCoded + 
                CONDITION_Coded +
                Gender_Parent_Coded +
                InExperimenterClassCoded +
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*type + 
                (type|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m2.7)
coef(summary(study1_m2.7))
inv.logit(coef(summary(study1_m2.7)))


study1_m2.8 <- glmer(Score~type + 
                GENDERCoded + 
                CONDITION_Coded +
                Gender_Parent_Coded +
                #InExperimenterClassCoded +
                Age_MonthsMeanCentered + 
                Age_MonthsMeanCentered*type + 
                (type|SUB_ID), 
              data=study1_longNoNA, family=binomial)
summary(study1_m2.8)
coef(summary(study1_m2.8))
inv.logit(coef(summary(study1_m2.8)))
Interrogating inv logit transform

describe(study1_longNoNA$Age_Months)
describe(study1_longNoNA$Age_MonthsMeanCentered)

#no interactino term 
#invlogit(0.13664 + 0.5*0.16214 + 0.01659*0 + -0.11348*0)
#invlogit(0.13664 + -0.5*0.16214 + 0.01659*0 + -0.11348*0)


#Interaction included 
invlogit(0.13664 + 0.5*0.16214 + 0.01659*0 + -0.11348*0 + 0.08850*0*0.5)
invlogit(0.13664 + -0.5*0.16214 + 0.01659*0 + -0.11348*0 + 0.08850*0*-0.5)


describe(study1_longNoNA$Age_Months)
describe(study1_longNoNA$Age_MonthsMeanCentered)


#Age SD Centered (-1SD)
#Parent
invlogit(0.13664 + 0.5*0.16214 + 0.01659*-4.03 + -0.11348*0 + 0.08850*-4.03*0.5)
#Experimenter
invlogit(0.13664 + -0.5*0.16214 + 0.01659*-4.03 + -0.11348*0 + 0.08850*-4.03*-0.5)

#Age SD Centered (+1SD)
#Parent
invlogit(0.13664 + 0.5*0.16214 + 0.01659*4.03 + -0.11348*0 + 0.08850*4.03*0.5)
#Experimenter
invlogit(0.13664 + -0.5*0.16214 + 0.01659*4.03 + -0.11348*0+ 0.08850*4.03*-0.5)



#Youngest Age prediction -8.04 (24 months)
#Parent
invlogit(0.13664 + 0.5*0.16214 + 0.01659*-8.04 + -0.11348*0 + 0.08850*-8.04*0.5)
#Experimenter
invlogit(0.13664 + -0.5*0.16214 + 0.01659*-8.04 + -0.11348*0 + 0.08850*-8.04*-0.5)

#Oldest SD Centered +10.96 (43 months)
#Parent
invlogit(0.13664 + 0.5*0.16214 + 0.01659*10.96 + -0.11348*0 + 0.08850*10.96*0.5)
#Experimenter
invlogit(0.13664 + -0.5*0.16214 + 0.01659*10.96 + -0.11348*0+ 0.08850*10.96*-0.5)
$$P(CS+ choice) = Parent Context + Age + Sex + Parent Context*Age + (Parent Context|Subject)$$

effect_Type <- as.data.frame(effect('Type:Age_MonthsMeanCentered', 
                                    study1_m1.5, 
                                    xlevels=list(Type= c(-0.5, 0.5), 
                                                 Age_MonthsMeanCentered=
                                                   seq(min(data$Age_MonthsMeanCentered, na.rm=T),
                                                       max(data$Age_MonthsMeanCentered, na.rm=T), .1))))

?effect
effect_Type$type <- ifelse(effect_Type$Type==-0.5, "E", "P")

#Plotting 
fitLmePlot <- ggplot(data=effect_Type, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
    scale_color_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Teachher","Parent"), values = c("seagreen2","blue")) +
    labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 1') +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
#fitLmePlot

fitLmePlotRawData <- ggplot(data=effect_Type, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_jitter(data=meanDataLong, aes(x=Age_MonthsMeanCentered, y=PercentChoice), alpha=0.5) +
  scale_color_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue")) +
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 1') +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))

#fitLmePlotRawData

fitLmePlotRawData <- ggplot(data=effect_Type, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) +
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_jitter(data=meanDataLong, aes(x=Age_MonthsMeanCentered, y=PercentChoice), alpha=0.5) +
  scale_color_manual(labels=c( "E","P"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "E","P"), values = c("seagreen2","blue")) +
  facet_wrap(~type, ncol=1)
#fitLmePlotRawData


fitLmePlotRawData <- ggplot(data=effect_Type, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_jitter(data=meanDataLong, aes(x=Age_MonthsMeanCentered, y=PercentChoice), alpha=0.5) +
  scale_color_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue")) + 
  facet_wrap(~type, labeller = labels) +
  labs(x = 'Age (mean centered)', y = 'Proportion of CS+ choices', title = 'Study 1') + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))
#fitLmePlotRawData


MeanCenteredAgePlot <- ggplot(data=effect_Type, aes(x=Age_MonthsMeanCentered, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_jitter(data=meanDataLong, aes(x=Age_MonthsMeanCentered, y=PercentChoice), alpha=0.5) +
  scale_color_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Teacher","Parent"), values = c("seagreen2","blue")) + 
  facet_wrap(~type, labeller = labels) +
  labs(x = 'Age (mean centered)', y = 'Proportion of CS+ choices', title = 'Study 1') + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))
#MeanCenteredAgePlot

effect_Type$Age_Months <- effect_Type$Age_MonthsMeanCentered+(mean(meanDataLong$Age_Months))


head(effect_Type$type)
teacher_labels <-(c( "Teacher","Parent"))
names(teacher_labels)  <-(c( "Teacher","Parent"))


teacher_labels <- as_labeller(c('E' = "Parent Absent", 'P' = "Parent Present"))


ggplot(data=effect_Type, aes(x=Age_Months, y=fit, group=type, colour=type)) +  
  geom_line() +
  facet_wrap(~type)

nojitter <- ggplot(data=effect_Type, aes(x=Age_Months, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_point(data=meanDataLong, aes(x=Age_Months, y=PercentChoice), alpha=0.5) +
  scale_color_manual(labels=c( "Parent Absent","Parent Present"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Parent Absent","Parent Present"), values = c("seagreen2","blue")) + 
  facet_wrap(~type, labeller = teacher_labels) + 
  labs(x = 'Age (months)', y = 'Proportion of CS+ choices', title = 'Study 1') + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_classic()
nojitter

AgeAxisChanged <- ggplot(data=effect_Type, aes(x=Age_Months, y=fit, group=type, colour=type)) +  
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  geom_hline(yintercept = .5, linetype="dotted") +
  geom_jitter(data=meanDataLong, aes(x=Age_Months, y=PercentChoice), alpha=0.5, jitter=0.1) +
  scale_color_manual(labels=c( "Parent Absent","Parent Present"), values = c("seagreen2","blue"))+
  scale_fill_manual(labels=c( "Parent Absent","Parent Present"), values = c("seagreen2","blue")) + 
  facet_wrap(~type, labeller = teacher_labels) + 
  labs(x = 'Age (months)', y = 'Proportion of CS+ choices', title = 'Study 1') + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_classic()

teacher_labels
AgeAxisChanged


#plug in ages to get prediction for E an P position 
View(effect_Type)
effect_Type$Age_MonthsMeanCentered==-5
study1_NoNA$ExperimenterClass <- study1_NoNA$InExperimenterClass

#t.test(study1_NoNA$Age_Months, study1_NoNA$ExperimenterClass)

mean_age_inclass <- study1_NoNA %>% 
  group_by(InExperimenterClass,) %>%
  summarise_at(vars(Age_Months), funs(mean(., na.rm=TRUE), sd(., na.rm=T), se=sd/sqrt(n())))
mean_age_inclass<-as.data.frame(mean_age_inclass)
mean_age_inclass <- mean_age_inclass %>% drop_na() #Note there is 1 NA value @ age 28 months 

mean_age_inclass$InExperimenterClass <- as.factor(mean_age_inclass$InExperimenterClass)
study1_NoNA$InExperimenterClass <- as.factor(study1_NoNA$InExperimenterClass)

mean_age_inclass$InExperimenterClass <- ifelse(mean_age_inclass$InExperimenterClass == 0, "Unfamiliar Teacher", "Familar Teacher") 
study1_NoNA$InExperimenterClass <- ifelse(study1_NoNA$InExperimenterClass == 0, "Unfamiliar Teacher", "Familar Teacher") 


ggplot(mean_age_inclass, 
  aes(InExperimenterClass, mean, group=InExperimenterClass, colour=InExperimenterClass)) + 
  #scale_color_brewer(palette = 'Set1') +
  #scale_fill_brewer(palette = 'Set1') +
  scale_color_manual(labels=c("Familiar Teacher","Unfamiliar Teacher"), values = c("red","purple"))+
  theme_classic() +
  geom_point() + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +  
  theme(text = element_text(size=14)) +
  geom_jitter(data=study1_NoNA, height=0, width=0.2, alpha=0.4, aes(InExperimenterClass, Age_Months))+
 labs(x = '', y = 'Age (months)', title = '') 

count(study1_NoNA$InExperimenterClass)
mutate(study1_NoNA$Age_Months, study1_NoNA$InExperimenterClass)
t.test(study1_NoNA$Age_Months~study1_NoNA$InExperimenterClass)

study1_NoNA %>% #wideNewNonNASubjects
  group_by(InExperimenterClass) %>%
  summarise_at(vars(Age_Months), funs(mean(., na.rm=TRUE), min(., na.rm=T), max(.,na.rm=T))) 
Figure 2. Significant Effect of Teacher Familiarity and Age. Significant Teacher Familiarity X Age difference such that older children are more likely to be paired with a teacher in their class. Plotted are the means and standard error bars for each context (Familiar vs. Unfamiliar Teacher) on the x-axis and age on the y-axis, with individual subject data jittered.


demo3Study2 <- demo3[which(demo3$study=="study2"),]

demo3Study2$study <- "Study 2"
names(demo3Study2)[1:3] <- c("", "", "")
names(demo3Study2)[13:16] <- c("Parent 1 Mode", "Range", "Parent 2 Mode", "Range")

dem2 <- demo3Study2[2:14]
dem2
demo3Study2 %>%
  kable(digits=2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width=F, position="center") %>%
  add_header_above(c("","", "N", "Mean Age in Months", "Range"=2, "Gender"=2, "Race/Ethnicity (percentage)"=5, "Parental Education"=4))


Table 2. Study 2 Demographics. Note: Race/Ethnicity totals exceed 100% to allow for participants to endorse more than one category.

Figure 4. Study 2 Conditioning and Testing Paradigm. Figure 4A. Conditioning phase. Children were presented with shapes on a computer screen for 500ms before a cartoon clip or gif played for 3000ms overlaid on the shape, then the shape presented on the screen for another 1000ms before a fixation cross indicated the start of a new trial. One shape was reinforced with the US 100% of the time, shapes were randomized between participants. CS+ shapes showed a clip from the popular TV show paw patrol and the CS- shapes showed a gif stick figure that made a movement such as a hand wave. Figure 4B. Testing phase. Children were then presented with two wooden boxes with the shapes on top while sitting in their chair. They were asked to choose which box they wanted their sticker prize from. Which side the CS+ shape was on was randomized for each trial.

#quest <- read.csv("questionnaire_data4.29.18.csv")
quest <- read.csv("questionnaire_dataStudy2_8.8.20.csv")
length(quest$Participant)
describe(quest$Involvement)
describe(quest$PositiveParenting)
describe(quest$InconsistentDiscipline)

#questNoNA <- quest %>% drop_na(Involvement) #has to be done at the questionnaire level 
quest$Involvement < 35
describe(quest$PositiveParenting)
describe(quest$InconsistentDiscipline)

#Involvement 10 items M = 40.79; SD = 4.39 ; problem cutoff 35
#Poor Supervision 10 items M = 1.88; S.D. = 2.70
#Positive Parenting 6 items M = 26.05; S.D. = 2.64 # positive parenting is a little bit lower in our sample ??????????
#Inconsistent Discipline 6 items M = 13.83; S.D. = 3.41
#Punishment 3 items M = 5.62; S.D. 1.61



#Means (s.d.)
describe(quest$AttentionalFocusing) #4.38 (.71)	4.61 (.82)	4.76 (.79)
describe(quest$AttentionalShifting) #4.59 (.68)	4.72 (.65)	4.79 (.62)	
describe(quest$Fear) #24 months = 2.33 (.87); 30 months =	2.52 (.96); 36 months =	2.80 (.99)
describe(quest$Frustration) #3.56 (.77)	3.66 (.88)	3.65 (.93)
describe(quest$HighIntensityPleasure) #4.95 (.92)	5.14 (.87)	5.07 (1.03)
describe(quest$Impulsivity) #5.02 (.75)	4.93 (.72)	4.78 (.69)
describe(quest$InhibitoryControl) #3.86 (.97)	4.08 (.94)	4.20 (.99)
describe(quest$PerceptualSensitivity) #3.94 (1.04)	4.14 (1.05)	4.26 (1.19)
describe(quest$LowIntenistyPleasure) #4.95 (.76)	4.88 (.70)	4.89 (.71)
describe(quest$Shyness) #3.20 (.91)	3.21 (.95)	3.52 (.85)
describe(quest$Sociability) #5.57 (.92)	5.74 (.85)	5.81 (.87)
describe(quest$Soothability) # 5.54 (.78)	5.46 (.74)	5.34 (.76)



#Results

#Loading Data
study2_long <- gather(study2, key="Trial", value="Score", COND1_1:COND2_5)


study2_long <- dplyr::mutate(study2_long,
                         type = case_when(
                           CONDITION == 0 & grepl('COND1', Trial) ~ 'E',
                           CONDITION == 0 & grepl('COND2', Trial) ~ 'P',
                           CONDITION == 1 & grepl('COND1', Trial) ~ 'P',
                           CONDITION == 1 & grepl('COND2', Trial) ~ 'E'
                         )) 

study2_long <- dplyr::mutate(study2_long,
                         TrialNumber = case_when(
                           grepl('_1', Trial) ~ 1,
                           grepl('_2', Trial) ~ 2,
                           grepl('_3', Trial) ~ 3,
                           grepl('_4', Trial) ~ 4,
                           grepl('_5', Trial) ~ 5
                           #grepl('_6', Trial) ~ 1,
                           #grepl('_7', Trial) ~ 2,
                           #grepl('_8', Trial) ~ 3,
                           #grepl('_9', Trial) ~ 4,
                           #grepl('_10', Trial) ~ 5
                         )) 
#                         )) %>% select(CONDITION, Trial, type, Score)

library(tidyr)
study2_long <- study2_long %>% drop_na(Score)

study2_long$GENDERMeanCentered <- study2_long$GENDER - mean(study2_long$GENDER)
study2_long$GENDERCoded <- ifelse(study2_long$GENDER == 0, -0.5, 0.5) 

study2_long$Age_MonthsMeanCentered <- study2_long$Age_Months-mean(study2_long$Age_Months) #This also works the same 

#length(study2_long$SUB_ID)/10
#table(study2_long$SUB_ID)

study2_exclude_subs <- (study2_long$SUB_ID) %in% c("HT111", "HT123", "HT128", "HT131") 
study2_longNoNA <- study2_long[!study2_exclude_subs,]


#Analyses
study2_longNoNA$Type <- ifelse(study2_longNoNA$type=="P", 0.5, -0.5)


study2_m1.51 <- glmer(Score~Type + 
                     Age_MonthsMeanCentered + 
                     GENDERCoded +
                     Age_MonthsMeanCentered*Type +
                 (Type|SUB_ID), 
               data=study2_longNoNA, family=binomial)
summary(study2_m1.51)

#inv.logit(-0.09697)
#inv.logit(0.21683)
#Convering to B rather and logit units - for percentage prediction purposes  

#intercept + Type*Parent + MeanAge + MeanSex + Interaction of Age and Type 
invlogit(-0.21424 + 0.21683*0.5 + 0.02357*0 + 0.09938*0 + -0.09697*0*0.5)

#intercept + Type*ParentAbsent + MeanAge + MeanSex + Interaction of Age and Type 
invlogit(-0.21424 + 0.21683*-0.5 + 0.02357*0 + 0.09938*0 + -0.09697*0*0.5)

#Ask paul about intercept interpretation here, it is negative meaning the baseline probability of choosing CS + is negative (bias toward CS - ) regardless of social context (type) age (mean ventered) and gender (mean centere) If 0 is CS- and 1 is CS+ 


#Random effects - BETWEEN / ACROSS SUBJECT SLOPES 
#intercept is regardless of condition between subject preference for CS+ 

#for SUB_ID indicates within subject variability across conditions 
#and for between subject type 
#Create a bw and within version of the variable 

library(bmlm)
centered <- isolate(study2_longNoNA, by="SUB_ID", value="Score", which="both")
head(centered)
describe(centered$Score_cb)
describe(centered$Score_cw)
#names(study2_longNoNA)

study2_longNoNA$CONDITION_Coded <- ifelse(study2_longNoNA$CONDITION==0, -0.5, 0.5)
study2_longNoNA$Gender_Parent_Coded <- ifelse(study2_longNoNA$GENDER_CAREGIVER==0, -0.5, 0.5)
study2_longNoNA$InExperimenterClassCoded <- ifelse(study2_longNoNA$InExperimenterClass==0, -0.5, 0.5)
table(study2_longNoNA$GENDER_CAREGIVER)

#study2_longNoNA

table(study2_longNoNA$Gender_Parent_Coded)/10
table(study2_longNoNA$InExperimenterClass)/10
#Adding all the variables to study 1finding  - we get sig gender of parent 
study2_m3 <- glmer(Score~Type + 
                 GENDERCoded + 
                 Age_MonthsMeanCentered + 
#                 Age_MonthsMeanCentered*Type + 
                 Gender_Parent_Coded + 
                 #InExperimenterClassCoded +
                 CONDITION_Coded+
                 (Type|SUB_ID), 
               data=study2_longNoNA, family=binomial)

summary(study2_m3)
inv.logit(0.74503) #sex of parent
inv.logit(0.05705) #main effect of order



study2_m4.5 <- glmer(Score~Type + 
                     CONDITION+
                       Type*CONDITION+
                       Age_MonthsMeanCentered + 
                       GENDERCoded +
                 (Type|SUB_ID), 
               data=study2_longNoNA, family=binomial)
summary(study2_m4.5)
inv.logit(-0.96512) #CONDNITION by Type interaction <- social context order interacts with social context effects 


study2_m4.1 <- glmer(Score~Type + 
                     CONDITION+
                       Type*CONDITION+
                       Age_MonthsMeanCentered + 
                       Gender_Parent_Coded + 
                       InExperimenterClassCoded +
                       GENDERCoded +
                 (Type|SUB_ID), 
               data=study2_longNoNA, family=binomial)
summary(study2_m4.1)
inv.logit(0.91993)


study2_m5.1 <- glmer(Score~Type + 
                     CONDITION+
                       Type*CONDITION+
                       Age_MonthsMeanCentered + 
                       #Gender_Parent_Coded + 
                       #InExperimenterClassCoded +
                       GENDERCoded +
                 (Type|SUB_ID), 
               data=study2_longNoNA, family=binomial)
summary(study2_m5.1)

#Parent hten Experimenter, parent absent first 
#parent present, in parent absent first (second)
invlogit(-0.25972 + 0.5*0.69445 + 0.09804*0 + 0.02044*0 + 0.10082*0 + -0.96512*0*0.5)
#parent absent in parent absent first (first)
invlogit(-0.25972 + -0.5*0.69445 + 0.09804*0 + 0.02044*0 + 0.10082*0 + -0.96512*0*-0.5)

#Parent then Experimenter , parent present first 
#parent present in parent present first (first)
invlogit(-0.25972 + 0.5*0.69445 + 0.09804*1 + 0.02044*0 + 0.10082*0 + -0.96512*1*0.5)
#parent absent in parent present first (second)
invlogit(-0.25972 + -0.5*0.69445 + 0.09804*1 + 0.02044*0 + 0.10082*0 + -0.96512*1*-0.5)


count(study2_longNoNA$CONDITION)

effect_Type <- as.data.frame(effect('Type:CONDITION', 
                                    study2_m4.5, 
                                    xlevels=list(Type= c(-0.5, 0.5), 
                                                 CONDITION=c(0,1))))
effect_Type$type <- ifelse(effect_Type$Type==-0.5, "E", "P")

labels <- as_labeller(c("E" = "Teacher", "P" = "Parent"))


labels <- as_labeller(c('0' = "Order: Parent Absent First", '1' = "Order: Parent Present First"))


effect_Type$type_name <- ifelse(effect_Type$type=="E", "Teacher", "Parent")
effect_Type$Condition <- ifelse(effect_Type$CONDITION==0, "Order: Parent Absent First", "Order: Parent Present First")

study2_longNoNA$Condition <- ifelse(study2_longNoNA$CONDITION==0, "Order: Parent Absent First", "Order: Parent Present First")
#View(study2_longNoNA)

meanData <- study2_longNoNA %>% #wideNewNonNASubjects
  group_by(SUB_ID, type, Condition) %>%
  summarise_at(vars(Score), funs(mean(., na.rm=TRUE)))
meanData <- as.data.frame(meanData)
 
meanData$type_name <- ifelse(meanData$type=="E", "Teacher", "Parent")

plot1 <- ggplot(data=effect_Type, aes(x=type, y=fit, group=type_name, colour=type_name)) +  
  geom_point() +
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  scale_color_manual(values = c("seagreen2","blue"))+
  scale_fill_manual(values = c("seagreen2","blue"))+
  geom_errorbar(aes(ymin=lower,ymax=upper, colour=type_name),width=.2) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  geom_jitter(data=meanData, aes(x=type, y=Score, colour=type_name), alpha=0.4, height=0.1, width=0.05) +
  theme(legend.title = element_blank()) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  facet_wrap(~Condition) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
 geom_hline(yintercept = .5, linetype="dotted")+ 
  ylim(0,1)

plot2 <- ggplot(data=effect_Type, aes(x=type, y=fit, group=type_name, colour=type_name)) +  
  geom_point() +
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  scale_color_manual(values = c("seagreen2","blue"))+
  scale_fill_manual(values = c("seagreen2","blue"))+
  geom_errorbar(aes(ymin=lower,ymax=upper, colour=type_name),width=.2) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  geom_jitter(data=meanData, aes(x=type, y=Score, colour=type_name), alpha=0.4, height=0.1, width=0.1) +
  theme(legend.title = element_blank()) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  facet_wrap(~Condition) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
 geom_hline(yintercept = .5, linetype="dotted")+ 
  ylim(0,1)




order <- c(1,2,4,3)

labels <- as_labeller(c("E" = "Experimenter", "P" = "Parent"))


meanData <- dplyr::mutate(meanData, 
                          order=case_when(
                            Condition == "Order: Parent Absent First" & type=="E" ~ "First", 
                            Condition == "Order: Parent Absent First" & type=="P" ~ "Second", 
                            Condition == "Order: Parent Present First" & type=="E" ~ "Second",
                            Condition == "Order: Parent Present First" & type=="P" ~ "First" 

                          ))

effect_Type$order <- as.numeric(c(1,2,4,3))

effect_Type<-effect_Type %>%
  arrange(order)

effect_Type
effect_Type$type_order <- c("First", "Second", "First", "Second")
meanData$type_order <- factor(meanData$type, levels=unique(meanData$type))


xlabsNames <- as.factor(c("Teacher Present", "Parent Present", "Parent Present", "Teacher Present"))

labels <- as_labeller(c("First" = "", "Second" = ""))

plot3 <-  ggplot(data=effect_Type, aes(x=type_order, y=fit, group=type_name, colour=type_name)) +  
  geom_point() +
  scale_color_manual(values = c("blue","seagreen2")) + 
  scale_fill_manual(values = c("blue","seagreen2"))+
  geom_errorbar(aes(ymin=lower,ymax=upper, colour=type_name),width=.2) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  geom_jitter(data=meanData, aes(x=order, y=Score, colour=type_name), alpha=0.4, height=0.1, width=0.2) +
  labs(x = 'Order of Conditioning', y = 'Proportion of CS+ Choices', title = 'Study 2') +
  facet_wrap(~Condition) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  #theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank()) #GETS RID OF X TICKS
 geom_hline(yintercept = .5, linetype="dotted") + 
  scale_x_discrete(labels = c("Parent Absent", "Parent Present")) +
#automatically orders
  #scale_x_discrete(labels=c(1,3,2,4)) +
  ylim(0,1.2) + 
  theme_classic()

#xlabsNames

effect_Type
plot3



#Failed attempt at changing labels to "Teacher Present", "Parent Present", "Parent Present", "Teacher Present"

#order <- c(1,2,4,3)

#labels <- as_labeller(c("E" = "Experimenter", "P" = "Parent"))


#meanData <- dplyr::mutate(meanData, 
#                          order=case_when(
#                            Condition == "Experimenter First" & type=="E" ~ "Teacher Present", #                             Condition == "Experimenter First" & type=="P" ~ "Parent Present",
#                            Condition == "Parent First" & type=="E" ~ "Teacher Present",
#                            Condition == "Parent First" & type=="P" ~ "Parent Present" 
#
#                          ))

meanData <- dplyr::mutate(meanData, 
                          order=case_when(
                            Condition == "Teacher First" & type=="E" ~ 1,      
                            Condition == "Teacher First" & type=="P" ~ 2,
                            Condition == "Parent First" & type=="P" ~ 1, 
                            Condition == "Parent First" & type=="E" ~ 2
                          ))


effect_Type$type_order <- as.numeric(c(2,1,1,2))



#effect_Type<-effect_Type %>%
#  arrange(order)
order <- c(1,2,4,3)

effect_Type$type_name <- c("Teacher Present", "Parent Present", "Parent Present", "Teacher Present")
library(tidyverse)
effect_Type <- effect_Type %>%
  mutate(type_name = fct_reorder(type_name, fit)) 

#effect_Type
#meanData$order

xlabsNames <- c("Teacher Present", "Parent Present", "Parent Present", "Teacher Present")
effect_Type$type_name <- as.factor(effect_Type$type_name)

ggplot(data=effect_Type, aes(x=type, y=fit, group=type, colour=type)) +  
  geom_point() +
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  scale_color_manual(values = c("seagreen2","blue"), name="Social Context")+
  scale_fill_manual(values = c("seagreen2","blue"))+
  geom_errorbar(aes(ymin=lower,ymax=upper, colour=type),width=.2) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  geom_jitter(data=meanData, 
              aes(x=order, y=Score, colour=type), alpha=0.4, height=0.1, width=0.2) +
  #theme(legend.title = element_text("Social Context")) +
  #theme(legend.title = element_blank(), axis.text.x=element_blank()) + 
  labs(x = 'Order of Conditioning', y = 'Proportion of CS+ Choices', title = 'Study 2') +
  facet_wrap(~Condition) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept = .5, linetype="dotted") + 
  ylim(0,1) + 
  scale_x_discrete(labels = effect_Type$type_name)

  scale_x_discrete(labels= xlabsNames) 


#effect_Type$type_ordered <- factor(effect_Type$type_name, levels=effect_Type$type_order[order(effect_Type$type_order)])

effect_Type$type_ordered

meanData$type_order <- factor(meanData$type, levels=unique(meanData$type))
#meanData$type_order <- dplyr::recode(meanData$order, 
#                                     "1"="Teacher Present", 
#                                     "2"="Parent Present",
#                                     "3"="Teacher Present", 
#                                     "4"="Parent Present")


#labels <- as_labeller(c( 1= "Parent Present", 2 = ""))
plot3 <-  ggplot(data=effect_Type, aes(x=type_order, y=fit, group=type_name, colour=type_name)) +  
  geom_point() +
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
  scale_color_manual(values = c("seagreen2","blue"), name="Social Context")+
  scale_fill_manual(values = c("seagreen2","blue"))+
  geom_errorbar(aes(ymin=lower,ymax=upper, colour=type_name),width=.2) + 
  labs(x = 'Age (mean centered)', y = 'P(choose CS+)', title = 'Study 2') +
  geom_jitter(data=meanData, aes(x=order, y=Score, colour=type_name), alpha=0.4, height=0.1, width=0.2) +
  #theme(legend.title = element_text("Social Context")) +
  #theme(legend.title = element_blank(), axis.text.x=element_blank()) + 
  labs(x = 'Order of Conditioning', y = 'Proportion of CS+ Choices', title = 'Study 2') +
  facet_wrap(~Condition) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
 geom_hline(yintercept = .5, linetype="dotted") + 
  ylim(0,1)

plot3

Figure 5. Significant Order Effects. Significant social context X order interaction β=0.28, such that children who underwent the teacher context first, demonstrate a negative bias toward the CS- compared to all other conditions which hover closer to the chance (50%) value. On the y-axis is the probability of choosing the CS+ shape and the x-axis shows social context separated into groups, children who completed the familiar teacher context first and those that completed the parent context first. Mean and confidence interval error bars are model fits and individual subjects’ raw data (percentage of the CS+ chosen across 5 trials) is plotted as individual points.

#Post hoc tests changing mean centered age


#Center at Mean +1 SD (or -) to conserve continuious variable
#change center of age - and look at differnce between groups 
#Present effects both ways - simple slope (x at different levels of m(age) and the other way and difference between m at difference x - main effect of age at difference group. )

#repeated measures versus multi level model - 
#Can't do repeated measure anova with binary outcomes - (Affects models - aov4 -uses same syntax as multilevel ) - aov more primitive 
describe(study1_longNoNA$Age_Months)
#sd = 4.1
 
describe(study1$Age_Months)

study1_longNoNA$Age_YoungCentered <- study1_longNoNA$Age_Months-(mean(study1_longNoNA$Age_Months)-4.03) #plus or minus SD 
mean(study1_longNoNA$Age_Months)-4.03
study1_longNoNA$Age_OldCentered <- study1_longNoNA$Age_Months-(mean(study1_longNoNA$Age_Months)+4.03) #This also works the same 
mean(study1_longNoNA$Age_Months)+4.03


#younger
study1_m1.51 <- glmer(Score~Type + 
                 GENDERMeanCentered + 
                 Age_YoungCentered + 
                 Age_YoungCentered*Type + 
                 (Type|SUB_ID), 
               data=study1_longNoNA, family=binomial)

summary(study1_m1.51)
coef(summary(study1_m1.51))

inv.logit(coef(summary(study1_m1.51)))

inv.logit(-0.18471313) #Type 
inv.logit(-0.18471313+0.07030594) #Type + Intercept


#older
study1_m1.52 <- glmer(Score~Type + 
                 GENDERMeanCentered + 
                 Age_OldCentered + 
                 Age_OldCentered*Type + 
                 (Type|SUB_ID), 
               data=study1_longNoNA, family=binomial)

summary(study1_m1.52)
coef(summary(study1_m1.52))

inv.logit(0.5335641) #Type
inv.logit(0.5335641+0.20398621 ) #Type + intercept


#Inv logit for interpretability 
inv.logit(coef(summary(study1_m1.5))) #When E is coded as -0.5 
inv.logit(coef(summary(study1_m1.6))) #When P is coded as -0.5
inv.logit(coef(summary(study1_m1.7))) #When not dummy coded 

inv.logit(coef(summary(study1_m1.51)))
inv.logit(coef(summary(study1_m1.52)))


#no inv.logit for p-values
(coef(summary(study1_m1.5))) #When E is coded as -0.5 
(coef(summary(study1_m1.6))) #When P is coded as -0.5
(coef(summary(study1_m1.7))) #When not dummy coded 

(coef(summary(study1_m1.51)))
(coef(summary(study1_m1.52)))




#Changing to P coded 
study1_m1.71 <- glmer(Score~type + 
                 GENDERMeanCentered + 
                 Age_YoungCentered + 
                 Age_YoungCentered*type + 
                 (type|SUB_ID), 
               data=study1_longNoNA, family=binomial)

summary(study1_m1.71)
coef(summary(study1_m1.71))

inv.logit(coef(summary(study1_m1.71)))

inv.logit(-0.18471313) #Type 
inv.logit(-0.18471313+0.07030594) #Type + Intercept


#older
study1_m1.72 <- glmer(Score~type + 
                 GENDERMeanCentered + 
                 Age_OldCentered + 
                 Age_OldCentered*type + 
                 (type|SUB_ID), 
               data=study1_longNoNA, family=binomial)


(coef(summary(study1_m1.71)))
(coef(summary(study1_m1.72)))


inv.logit(coef(summary(study1_m1.71)))
inv.logit(coef(summary(study1_m1.72)))
