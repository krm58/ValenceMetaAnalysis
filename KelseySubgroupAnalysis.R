library(readxl)
library(meta)
library(metafor)
library(dplyr)
library(dmetar)
library(stringr)
library(tidyr)
library(ggplot2)

load("C:/Users/krmcd/OneDrive - Duke University/Duke/Walter Sinnot-Armstrong/Meta Analysis/Analysis_Publication/ValenceMetaAnalysis-master/df.RData")

#Manual cleaning edits to df --> these were updated on github df on 5/13
#df[191, 2] = '1993' #need to add the publication data to 'Tindale' entry
#[9,28] = "1" #Stark 2017 article is a main effect
#df[134,10] = "306" #this study actually had 306 subjects, not 360
#df[135,10] = "306"

#trimdf <- df[c(1,3,4,28)]

# -160 from Levin 1998
# -1200 from Boettcher 2004 The Prospects for
# -600 from Damnjanovic 2016
# -260 Fagley 2010 study 1
# -45 Fagley 1987
# -761 Kam 2010
# -214 Druckman 2004
# -192 Druckman 2004
# -616 Fagley 1997
# -384 Ronnlund 2005
# -2487 Watanabe 2010
# -40 Wang 2017
# -22 Zheng 
# -372 Kim 2005
# -306 Li 1995
# -206 Simon 2004
# -400 Peng 2013
# -219 Paese 1993
# -259 Wallin
# -1773 Cao 2017
# -190 Christman 2014
# -360 Huang 2015
# -92 McElroy 2004
# -1134 McElroy 2003
# -3680 Mahoney 2011
# -304 Li 1998

###TOTAL: 16276 to be subtracted from sum(df$`# of Subjects`), for a grand total of 42260 subjects in our meta-analysis


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


# I'm responsbile for the following subgroups:
# .	KELSEY - student vs nonstudent
# .	KELSEY - age as continuous regressor -- turns out 
# .	KELSEY - gender
# .	KELSEY - when you show both frames? less framing effects when you show both? verify. between versus within manipulation differences

#SUBGROUP ANALYSIS #1: students versus non-students
#studentWords = c("Student","Students","student","students","undergrads","Undergraduates","Undergrads")

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
