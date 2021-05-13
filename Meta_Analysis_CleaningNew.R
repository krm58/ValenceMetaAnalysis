library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)

#-------Binding All of the Datasets Together

Kdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "KelseyEffectSizes")

Rdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "RoseEffectSizes")

JJdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "JJEffectSizes")
JJdf = JJdf[,1:29]

Tdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "TaraEffectSizes")

Sdf <- read_excel("/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/ValenceFramingMetaAnalysis.xlsx", sheet = "SiyuanEffectSizes")

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

#-------- Moral Judgement: ADP v Modified ADP
dfADP_ADPModif <- df%>% dplyr::filter( grepl('ADPModif|ADP', MoralJudgeFactor))
dfADP_ADPModif <- dfADP_ADPModif%>% dplyr::mutate(moraljudge_bin = ifelse(MoralJudgeFactor =="ADP", 1,0))

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
df$Consistent = as.numeric(df$Consistent)


#------- ES True Direction 

df = df %>% mutate(ESTrue = ES)
df = df %>% mutate(ESTrue = ifelse((Consistent ==1), abs(df$ESTrue), df$ES)) %>%
  mutate(ESTrue = ifelse((Consistent==0), -abs(df$ES),df$ES))
                                   
df = df %>% mutate(lowCITrue = lowCI, highCITrue = highCI)

for(i in 1:dim(df)[1]){
  if(df$Consistent[i]==1 & df$highCITrue[i]<0 & df$lowCITrue[i]<0){
    
    k = df$highCITrue[i]
    j = df$lowCITrue[i]
    
    df$highCITrue[i] = abs(j)
    df$lowCITrue[i]=  abs(k)
    
  }
}

for(i in 1:dim(df)[1]){
  if(df$Consistent[i]==1 & isTRUE(!(df$highCITrue[i] > df$ESTrue[i]) ) ){
    
    k = df$highCITrue[i]
    j = df$lowCITrue[i]
    
    df$highCITrue[i] = -(j)
    df$lowCITrue[i]=  -(k)
    
  }
}



for(i in 1:dim(df)[1]){
  if(df$Consistent[i]==0 & df$highCITrue[i]>0 & df$lowCITrue[i]>0){
    
    k = df$highCITrue[i]
    j = df$lowCITrue[i]
    
    df$highCITrue[i] = -(j)
    df$lowCITrue[i]=  -(k)
    
  }
}

for(i in 1:dim(df)[1]){
  if(df$Consistent[i]==0 & isTRUE((df$lowCITrue[i] >= df$ESTrue[i]) ) ){
    
    k = df$highCITrue[i]
    j = df$lowCITrue[i]
    
    df$highCITrue[i] = abs(j)
    df$lowCITrue[i]=  abs(k)
    
  }
}



#-------- lol now switch it back 

df = df %>% mutate( esvar = ((abs((highCI-lowCI)/2)/1.96)^2), esvarTrue = ((abs((highCITrue-lowCITrue)/2)/1.96)^2) )

save(df, file = "/Users/RoseGraves/Documents/Duke- Semester Spring 2020/Philosphy Independent Study/df.RData")
