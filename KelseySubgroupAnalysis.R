library(readxl)
library(meta)
library(metafor)
library(dplyr)
library(dmetar)
library(stringr)
library(tidyr)
library(ggplot2)

#Make histogram of publication years to show time-trending interest on Framing Effects
trimdf <- df[!duplicated(df[c("Authors")]),]
trimdf$PubYear <- as.numeric(trimdf$PubYear)
ggplot(data=trimdf, aes(x=PubYear)) + geom_histogram() + 
  labs(title="Empirical Papers on Framing Effects") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Year Published", y="Count")\

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

#Do outliers drive this effect?
outliersdf <- spot.outliers.random(data=m_RE)
#make new RE meta analysis model without the outliers
newdf <- df[!m_RE$studlab %in% c(as.character(outliersdf$StudyID)),]
#Total RE analysis without outliers:
#effect size = 0.4168, p<0.0001, still significant heterogeneity though
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


# I'm responsbile for the following subgroups:
# .	KELSEY - student vs nonstudent
# .	KELSEY - age as continuous regressor
# .	KELSEY - gender
# .	KELSEY - when you show both frames? less framing effects when you show both? verify. between versus within manipulation differences

#SUBGROUP ANALYSIS #1: students versus non-students
#studentWords = c("Student","Students","student","students","undergrads","Undergraduates","Undergrads")

studentdf <- df[df[, "student_bin"] == 1,]
nonstudentdf <- df[df[, "student_bin"] == 0,]

#pooled effect-size = student group = 0.5188; non-student group = 0.4491 (test for group differences p-value = 0.0699)
df$student_bin <- factor(df$student_bin)
m_studentsubgroup <- metagen(ES,
                             data=df,
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
#Do outliers drive this effect?
outliersdf <- spot.outliers.random(data=m_studentsubgroup)
newdf <- df[!m_RE$studlab %in% c(as.character(outliersdf$StudyID)),]
#make new RE meta analysis model without the outliers
newdf <- newdf[!is.na(newdf$student_bin),]
#student subgroup analysis without outliers:
#student ES = 0.4275, nonstudent = 0.3894, between groups p-value = 0.2172
m_studentsubgroup.nooutliers <- metagen(ES,
                             data=newdf,
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
# Histogram shows we don't have enough variability in MeanAge to quantify framing effect as a function of Age
metareg.meanage <- metareg(m_RE,MeanAge)
ggplot(data=df, aes(x=MeanAge)) + geom_histogram()

# SUBGROUP ANALYSIS #3: gender
# estimate = -0.1009, p-value = 0.4343
metareg.gender <- metareg(m_RE,PropMales)

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
ggplot(data=betweendf, aes(ES)) + geom_histogram() + labs(title="Between-subjects Design Effect Sizes", x = "Effect Size") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data=withindf, aes(ES)) + geom_histogram() + labs(title="Within-subjects Design Effect Sizes", x = "Effect Size") + theme(plot.title = element_text(hjust = 0.5))

#Determine if the between vs within null effect is due to outliers: defined as a study's confidence interval being outside
#the confidence interval of the pooled effect

outliersdf <- spot.outliers.random(data=m_betweenwithinsubgroup)
newdf <- df[!m_RE$studlab %in% c(as.character(outliersdf$StudyID)),]
#make new RE meta analysis model without the outliers
newdf <- newdf[!is.na(newdf$betweeen_within_bin),]

#When we remove outliers design subgroup differences are now significant!!!!!!!
#between = 0.4428, within = 0.3158 (still more heterogeneity). between groups difference: p-value = 0.0014!
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
