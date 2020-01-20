#Import libraries and packages
library(readxl)
library(meta)
library(metafor)
library(dplyr)
library(dmetar)
library(stringr)
library(tidyr)
library(ggplot2)

#Load in the data from each person's extraction sheet
kelseydf <- read_excel("C:/Users/Pearson Lab/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceFramingMetaAnalysis_extracted_10-14-19.xlsx",sheet = "KelseyEffectSizes")
rosedf <- read_excel("C:/Users/Pearson Lab/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceFramingMetaAnalysis_extracted_10-14-19.xlsx",sheet = "RoseEffectSizes")
siyuandf <- read_excel("C:/Users/Pearson Lab/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceFramingMetaAnalysis_extracted_10-14-19.xlsx",sheet = "SiyuanEffectSizes")
jjdf <- read_excel("C:/Users/Pearson Lab/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceFramingMetaAnalysis_extracted_10-14-19.xlsx",sheet = "JJEffectSizes")
taradf <- read_excel("C:/Users/Pearson Lab/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceFramingMetaAnalysis_extracted_10-14-19.xlsx",sheet = "TaraEffectSizes")

#Random manual changes to dfs before merging
jjdf <- dplyr::select(jjdf, -c("...24")) #remove the "...24" column

#Now let's concatenate the datasets into one huge dataframe
df <- rbind(kelseydf, rosedf, siyuandf, jjdf, taradf)

#Get rid of rows that have NaN in every column
row.has.na <- apply(df, 1, function(x){all(is.na(x))})
df <- df[!row.has.na,]

#Assign each effect size a unique study identifier

df <- rename(df, Authors = `Study Title`)
df <- rename(df, ES = `Mean Effect Size`)
df <- rename(df, lowCI = `CI -`)
df <- rename(df, highCI = `CI +`)
df <- rename(df, PubYear = `Year Published`)
df <- rename(df, PropMales = `Proportion Male Subs`)
df <- rename(df, MeanAge = `mean age`)
df$StudyID <- paste(word(df$Authors, 1), df$`Year Published`, rownames(df),sep = '_')

#For now, just deal with absolute value of effect sizes
#First, we have to swap the low and high CIs for just the negative ES
df$lowCI <- as.numeric(df$lowCI)
df$highCI <- as.numeric(df$highCI)

negES <- which(df$ES < 0)
v1 <- df$lowCI[negES]
v2 <- df$highCI[negES]

df$lowCI[negES] <- v2
df$highCI[negES] <- v1

df$ES <- abs(df$ES)
df$lowCI <- abs(df$lowCI)
df$highCI <- abs(df$highCI)

df[169,18] <- 0.5718 #Rose manually changed sign
df[169,19] <- 1.1768
df[267,18] <- -0.0852 #JJ's entered low/high CI didn't match r command
df[267,19] <- 1.0557

probsigns <- which(df$ES <= df$lowCI)
df[probsigns,] <- transform(df[probsigns,], lowCI = ifelse(abs(ES) < abs(lowCI), -1*lowCI, lowCI))

#Drop studies that have an effect size that is not a number
df <- df %>% drop_na(ES)
df <- df %>% drop_na(Authors)

#fixed effects model
m_FE <- metagen(ES,
             data=df,
             lower = lowCI,
             upper = highCI,
             studlab=paste(StudyID),
             comb.fixed = TRUE,
             comb.random = FALSE,
             prediction=TRUE,
             sm="SMD")


#random effects model
m_RE <- metagen(ES,
             data=df,
             lower = lowCI,
             upper = highCI,
             studlab=paste(StudyID),
             comb.fixed = FALSE,
             comb.random = TRUE,
             method.tau = "SJ",
             hakn = TRUE,
             prediction=TRUE,
             sm="SMD")

#This needs to be made prettier, but at least it gets the job done
pdf(file='forestplot.pdf',width=20,height=80) 
forest.jama<-forest(m_RE,prediction = FALSE,
                    layout = "JAMA")
dev.off() 


#Determine which studies' effects are outliers: defined as a study's confidence interval being outside
#the confidence interval of the pooled effect

spot.outliers.random<-function(data){
  data<-data
  StudyID<-data$studlab
  lowerci<-data$lower
  upperci<-data$upper
  m.outliers<-data.frame(StudyID,lowerci,upperci)
  te.lower<-data$lower.random
  te.upper<-data$upper.random
  dplyr::filter(m.outliers,upperci < te.lower)
  dplyr::filter(m.outliers,lowerci > te.upper)
}

outliersdf <- spot.outliers.random(data=m_RE)

newdf <- df[!m_RE$studlab %in% c(as.character(outliersdf$StudyID)),]

#make new RE meta analysis model without the outliers
m_RE.nooutliers <- metagen(ES,
                data=newdf,
                lower = lowCI,
                upper = highCI,
                studlab=paste(StudyID),
                comb.fixed = FALSE,
                comb.random = TRUE,
                method.tau = "SJ",
                hakn = TRUE,
                prediction=TRUE,
                sm="SMD")

#Just to have a copy of the effect sizes that are all concatenated by collection author
write.csv(df, "../OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/combineddf.csv")

#Now conduct a subgroup analysis: students versus non-students
#studentWords = c("Student","Students","student","students","undergrads","Undergraduates","Undergrads")
nonstudentdf <- dplyr::filter(df, !grepl('Student|Students|student|students|undergrads|Undergraduates|Undegraduates|Undergrads', `Type of Sample`))
studentdf <- dplyr::filter(df, grepl('Student|Students|student|students|undergrads|Undergraduates|Undegraduates|Undergrads', `Type of Sample`))

df$StudyBoolSubGroup <- as.numeric(df$StudyID %in% studentdf$StudyID) #add subgroup indicator for study
df$StudyBoolSubGroup <- ifelse(df$StudyBoolSubGroup == 1, "Student", "Non-Student")

m_studentsubgroup <- metagen(ES,
                                    data=df,
                                    lower = lowCI,
                                    upper = highCI,
                                    byvar = StudyBoolSubGroup,
                                    studlab=paste(StudyID),
                                    comb.fixed = FALSE,
                                    comb.random = TRUE,
                                    method.tau = "SJ",
                                    hakn = TRUE,
                                    prediction=TRUE,
                                    sm="SMD")

trimdf <- df[!duplicated(df[c("Authors")]),]

ggplot(data=trimdf, aes(trimdf$PubYear)) + geom_histogram() + 
  labs(title="Empirical Papers on Framing Effects") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Year Published", y="Count")

#Now do meta-regression (basically subgroups with a continuous predictor) for publication year (super not significant)
metareg.pubyear <- metareg(m_RE,PubYear)

#What about Proportion Male Subs? STILL NEED TO DEBUG
df$PropMales <- as.numeric(df$PropMales)
metareg.propMales <- metareg(m_RE,PropMales)

#Now use age as a continuous predictor to compare with student/non-student -- STILL NEED TO DEBUG
df$MeanAge <- as.numeric(df$MeanAge)
metareg.meanage <- metareg(m_RE,MeanAge)