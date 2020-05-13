library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)

#-------Binding All of the Datasets Together

Kdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "KelseyEffectSizes")

Rdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "RoseEffectSizes")

JJdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "JJEffectSizes")
JJdf = JJdf[,1:28]

Tdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "TaraEffectSizes")

Sdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "SiyuanEffectSizes")
Sdf = Sdf[1:54,]

df = rbind(Kdf, Rdf, JJdf, Tdf, Sdf)

#--------Get rid of the rows that have all NAs in the entries
row.has.na <- apply(df, 1, function(x){all(is.na(x))})
df <- df[!row.has.na,]

#--------Rename the Columns 
df <- dplyr::rename(df, Authors = `Study Title`)
df <- dplyr::rename(df, ES = `Mean Effect Size`)
df <- dplyr::rename(df, lowCI = `CI -`)
df <- dplyr::rename(df, highCI = `CI +`)
df <- dplyr::rename(df, PubYear = `Year Published`)
df <- dplyr::rename(df, PropMales = `Proportion Male Subs`)
df <- dplyr::rename(df, MeanAge = `mean age`)
df <- dplyr::rename(df, SdAge = `std age`)
df <- dplyr::rename(df, participants = `Type of Sample`)
df <- dplyr::rename(df, moralJudge = `Moral Judgment`)
df <- dplyr:: rename(df, design = `Research Design`)
df$StudyID <- paste(word(df$Authors, 1), df$PubYear, rownames(df),sep = '_')
df$lowCI <- as.numeric(df$lowCI)
df$highCI <- as.numeric(df$highCI)
df$ES <- as.numeric(df$ES)
df <-df[!is.na(df$ES),]
df <-df[!is.na(df$lowCI),]

#For now, just deal with absolute value of effect sizes
#First, we have to swap the low and high CIs for just the negative ES
negES <- which(df$ES < 0)
v1 <- df$lowCI[negES]
v2 <- df$highCI[negES]

df$lowCI[negES] <- v2
df$highCI[negES] <- v1

df$ES <- abs(df$ES)
df$lowCI <- abs(df$lowCI)
df$highCI <- abs(df$highCI)

probsigns <- which(df$ES <= df$lowCI)
df[probsigns,] <- transform(df[probsigns,], lowCI = ifelse(abs(ES) < abs(lowCI), -1*lowCI, lowCI))


#-------Creating a new colmun indiacating if the participant sample is a student (1) or a not (0)
df <- df%>% dplyr::mutate(student_bin = ifelse( !grepl('Student|Students|student|students|undergrads|Undergraduates|Undegraduates|Undergrads|undergraduates|studying', participants),0,1))

df <- df%>%dplyr::mutate(student_bin = ifelse((Authors == "Li" & PubYear ==1995 )| (Authors == "Druckman 2008"&PubYear ==2008), NA,df$student_bin ))

#-------Create a new column indcating The type of moral judgement
#df <- df %>% 
#  mutate_all(funs(str_replace(., "Asian Disease Problem", "ADP"))) %>%
#  mutate_all(funs(str_replace(., "Similar to ADP", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "similar to ADP", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "Modified Asian Disease Problem", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like for human like outcomes", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like for human lifes", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like, but smallpox", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like war problem", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "4 types of ADP-like problems", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like human life", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like human life", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "life saving risky scenario", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "Modified ADP (replace disease with war)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "Modified ADP (replace with AIDS victims)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP 600", "ADP")))%>%
#  mutate_all(funs(str_replace(., "Asian Disease", "ADP")))%>%
#  mutate_all(funs(str_replace(., "Fatal Disease Problem", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP (w/ 3-6 kin)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like (Coal Mining Problem 1)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP (Alien Lives)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP 6 relatives", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-like (ADP, civil defense problem, and cancer treatment problem)", "ADPModif")))%>% mutate_all(funs(str_replace(., "Asian Disease & Modified Asian Disease", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "Asian DIsease Problem Unspecified #", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "Asian DIsease problem", "ADP")))%>%
#  mutate_all(funs(str_replace(., "Modified ADP", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP & ADPModif", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "West Nile Virus task (augmented ADP)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP (augmented)", "ADPModif")))%>%
#  mutate_all(funs(str_replace(., "ADP-augmented", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "West Nile Virus task (augmented ADP)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like military example", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like cancer scenario", "ADPModifMedical"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like for medical decision", "ADPModifMedical"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like for lives", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADPModif (replace disease with war)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADPModif (replace with AIDS victims)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP0", "ADP 6000"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like (Coal Mining Problem 1)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP (Alien Lives)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP & Modified Asian Disease", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "Trolley Problem 15", "Trolly Problem"))) %>%
#  mutate_all(funs(str_replace(., "Trolley Problem 5", "Trolly Problem"))) %>%
#  mutate_all(funs(str_replace(., "trolly problems", "Trolly Problem"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like military example", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like military example", "ADPModif")))  %>%
#  mutate_all(funs(str_replace(., "ADP (Alien Lives)", "ADPModif")))  %>%
#  mutate_all(funs(str_replace(., "ADP-like (Coal Mining Problem 1)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP and oil tanker ADP-like problem", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADPModif (replace with AIDS victims)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADPModif (replace disease with war)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP 120", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP-like (Coal Mining Problem 1)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP (Alien Lives)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "West Nile Virus task (augmented ADP)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP (Alien Lives)", "ADPModif"))) %>%
#  mutate_all(funs(str_replace(., "ADP (augmented)", "ADPModif")))


#-------- Moral Judgement: ADP v Modified ADP
dfADP_ADPModif <- df%>% dplyr::filter( grepl('ADPModif|ADP', MoralJudgeFactor))
dfADP_ADPModif <- dfADP_ADPModif%>% dplyr::mutate(moraljudge_bin = ifelse(MoralJudgeFactor =="ADP", 1,0))

save(dfADP_ADPModif, file = "/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/dfADP_ADPModif.RData")

#------- Moral Judgement: ADP & Modified ADP v. Other
df <- df%>% dplyr::mutate(ADPmodif_other_bin = ifelse( grepl('ADPModif|ADP', MoralJudgeFactor),1,0))

#------- Moral Judgement: Original ADP v. All Other
df <- df%>% dplyr::mutate(ADP_other_bin = ifelse( MoralJudgeFactor=='ADP',1,0))

#------- Method: Between v. Within
df[ df == "unclear" ] <- NA
df[ df == "2x2 (between) x 2x3 (within) subjects" ] <- NA
df[ df == "2x2 between - within subjects" ] <- NA

df <- df %>% 
  mutate_all(funs(str_replace(., "2x4 between subjects", "between-subject")))%>%
  mutate_all(funs(str_replace(., "between subjects by frame (then within subjects by need condition)", "between-subjects"))) %>%
  mutate_all(funs(str_replace(., "between subjects by frame", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "2x2x2 between subjects", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "2x2 between subjects", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "between subjects 2 - level", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "between-subject - forced choice", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "between-subject - with no-choice option", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "between subjects", "between-subject"))) %>%
  mutate_all(funs(str_replace(., "between-subjectss", "between-subject"))) 
df[ df == "between-subject (then within-subject by need condition)" ] <- "between-subject" 

df <- df%>%
  mutate_all(funs(str_replace(., "within-subject - with no-chioce option", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within-subjects 2- level (gain/loss)", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within-subjects 2-level", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within-subject - forced option", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within subjects", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within-subject", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "Within subjects", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "within Subjects", "within-subject"))) %>%
  mutate_all(funs(str_replace(., "Within Subjects", "within-subject")))  %>%
  mutate_all(funs(str_replace(., "within-subjects 2- level (gain/loss)", "within-subject"))) 
df[ df == "within-subjects 2- level (gain/loss)" ] <- "within-subject" 

df <- df%>% dplyr::mutate(betweeen_within_bin = ifelse(design =="between-subject", 1,0))

#----- Make Things Numeric Again
df$lowCI <- as.numeric(df$lowCI)
df$highCI <- as.numeric(df$highCI)
df$ES <- as.numeric(df$ES)
df$MeanAge = as.numeric(df$MeanAge)
df$SdAge = as.numeric(df$SdAge)
df$PropMales = as.numeric(df$PropMales)


#------- ES True Direction 

df = df %>% mutate(ESTrue = ifelse((ES <=0 & Consistent ==1), df$ES, ifelse((ES>0 & Consistent==1), -(df$ES),df$ES )))

df = df %>% mutate(lowCITrue = ifelse(ESTrue <0, -(df$highCI), lowCI))
df = df %>% mutate(highCITrue = ifelse(ESTrue <0, -(df$lowCI), highCI))

#-------- lol now switch it back 


df = df %>% mutate(ESTrue = -ESTrue, lowCITrue = -highCITrue, highCITrue = -lowCITrue)



save(df, file = "/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/df.RData")
