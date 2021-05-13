library(readxl)
library(meta)
library(metafor)
library(dplyr)
library(dmetar)
library(stringr)
library(tidyr)
library(ggplot2)

load("C:/Users/krmcd/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceMetaAnalysis-master/df.RData")

#Make histogram of publication years to show time-trending interest on Framing Effects
trimdf <- df[!duplicated(df[c("Authors")]),]
trimdf$PubYear <- as.numeric(trimdf$PubYear)
ggplot(data=trimdf, aes(x=PubYear)) + geom_histogram() + 
  labs(title="Empirical Articles on Framing Effects") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Year of Publication", y="Number of Publications")


#fixed effects model: pooled ES = 0.4535, p-value < 0.0001
m_FE <- metagen(ES,
                data=df,
                lower = lowCI,
                upper = highCI,
                studlab=paste(StudyID),
                comb.fixed = TRUE,
                comb.random = FALSE,
                prediction=TRUE,
                sm="SMD")


#random effects model (the one we will use): pooled ES = 0.4996, p-value < 0.0001
#the pooled effect size here is in line with the magnitude reported in Steiger & Kühberger, 2018!
df$MortalVnonMortal <- as.numeric(df$MortalVnonMortal)
df$MortalVnonMortal <- factor(df$MortalVnonMortal)
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


#SUBGROUP ANALYSIS #1: students versus non-students

studentdf <- df[df[, "student_bin"] == 1,]
nonstudentdf <- df[df[, "student_bin"] == 0,]

#pooled effect-size = student group = 0.5188; non-student group = 0.4491 (test for group differences p-value = 0.0699)
df$student_bin <- factor(df$student_bin)
studentsubgroupdf <- df[!is.na(df$student_bin), ] #get rid of articles where student_bin is NA
m_studentsubgroup <- metagen(ES,
                             data=studentsubgroupdf,
                             lower = lowCI,
                             upper = highCI,
                             byvar = student_bin,
                             studlab=paste(StudyID),
                             comb.fixed = FALSE,
                             comb.random = TRUE,
                             method.tau = "SJ",
                             hakn = TRUE,
                             prediction=TRUE,
                             sm="SMD")

# SUBGROUP ANALYSIS #2: age as continuous regressor: beta = -0.0018, p-value = 0.6339 :(
# Histogram shows we don't have enough variability in MeanAge to quantify framing effect as a function of Age, this is verified in past meta analyses
# "Age differences are rarely tested systematically in framing experiments and thus the relevant information for a meta-analysis is missing", Kuhburger 1998
# metareg.meanage <- metareg(m_RE,MeanAge)
# ggplot(data=df, aes(x=MeanAge)) + geom_histogram()

# SUBGROUP ANALYSIS #3: gender
# estimate = -0.1009, p-value = 0.4343, R^2=0.00%
metareg.genderInteract <- metareg(m_RE,PropMales) 
metareg.genderInteract <- metareg(m_RE,PropMales * MortalVnonMortal) 

# SUBGROUP ANALYSIS #4: between versus within manipulation differences (between = 1, within = 0)
#between = 0.5032; within = 0.4988 (test for group diffs p-val = 0.9439). More heterogeneity (I^2 for within...
#also within had less effect sizes than between, so mention that)
df$betweeen_within_bin <- factor(df$betweeen_within_bin)
df <- df[!is.na(df$betweeen_within_bin),]
m_betweenwithinsubgroup <- metagen(ES,
                             data=df,
                             lower = lowCI,
                             upper = highCI,
                             byvar = betweeen_within_bin,
                             studlab=paste(StudyID),
                             comb.fixed = FALSE,
                             comb.random = TRUE,
                             method.tau = "SJ",
                             hakn = TRUE,
                             prediction=TRUE,
                             sm="SMD")

withindf <- df[ which(df$betweeen_within_bin==0), ]
betweendf <- df[ which(df$betweeen_within_bin==1), ]
#ggplot(data=betweendf, aes(ES)) + geom_histogram() + labs(title="Between-subjects Design Effect Sizes", x = "Effect Size") + theme(plot.title = element_text(hjust = 0.5))
#ggplot(data=withindf, aes(ES)) + geom_histogram() + labs(title="Within-subjects Design Effect Sizes", x = "Effect Size") + theme(plot.title = element_text(hjust = 0.5))

#Determine if the between vs within null effect is due to outliers: defined as a study's confidence interval being outside
#the confidence interval of the pooled effect

outliersdf <- spot.outliers.random(data=m_betweenwithinsubgroup)
newdf <- df[!m_RE$studlab %in% c(as.character(outliersdf$StudyID)),]
#make new RE meta analysis model without the outliers
newdf <- newdf[!is.na(newdf$betweeen_within_bin),]

m_betweenwithinsubgroup.nooutliers <- metagen(ES,
                           data=newdf,
                           lower = lowCI,
                           upper = highCI,
                           byvar = betweeen_within_bin,
                           studlab=paste(StudyID),
                           comb.fixed = FALSE,
                           comb.random = TRUE,
                           method.tau = "SJ",
                           hakn = TRUE,
                           prediction=TRUE,
                           sm="SMD")
