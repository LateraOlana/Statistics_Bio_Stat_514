library(rigr)
library(ggplot2)
library(ggpubr)
library(table1)
library(moments)
#Code for Question 1 and 2.
#Setting working directory and reading the data
working_directory <- setwd("C:/Users/latera/Desktop/Bio_R_Code/Bio_R_Code")
data_study001 <- read.table(file = "Data/BirthsKingCounty2001-Biost514-517-2022.txt", header = TRUE)

str(data_study001)
summary(data_study001)

data_study001$Smoking_status = factor(data_study001$smoker, c("N","Y"), c(0,1))
data_study001$Drinking_status = factor(data_study001$drinker, c("N","Y"), c(0,1))
data_study001$Gender = factor(data_study001$sex, c("F","M"), c(0,1))
summary(data_study001$Smoking_status)
summary(data_study001$Drinking_status)
summary(data_study001$Gender)

data_study001$Race = factor(data_study001$race, c("asian","black","hispanic","white","other"), c(1,2,3,4,5))

#Categorizing parity
#0: No prior children, 1-2: Moderate prior children and 3+: multiple prior children
data_study001$parity_cat <- cut(data_study001$parity,
                                breaks=c(-1, 0, 2, 13),
                                labels=c('No prior children', 'Moderate prior children', 'Multiple prior children'))
data_study001$parity_cat
table(data_study001$parity_cat)
data_study001$parity_cat = factor(data_study001$parity_cat, c("No prior children","Moderate prior children","Multiple prior children"), c(1,2,3))

#Categorizing daily smoking
#0: No smoking, 1-5 cigarette: Significant smoking and 6+ cigarette: Serious smoking
data_study001$smokingN_cat <- cut(data_study001$smokeN,
                                breaks=c(-1, 0, 5, 26),
                                labels=c('No smoking', 'Significant smoking', 'Serious smoking'))
data_study001$smokingN_cat
table(data_study001$smokingN_cat)
data_study001$smokingN_cat = factor(data_study001$smokingN_cat, c('No smoking', 'Significant smoking', 'Serious smoking'), c(1,2,3))
table(data_study001$smokingN_cat)

#Categorizing daily drinking
#0: 'No alcohol', 1 -2: 'High drinking', 3+: 'Extremely high drinking'.
data_study001$drinkingN_cat <- cut(data_study001$drinkN,
                                  breaks=c(-1, 0, 2, 17),
                                  labels=c('No alcohol', 'High drinking', 'Extremely high drinking'))
data_study001$drinkingN_cat
table(data_study001$drinkingN_cat)
data_study001$drinkingN_cat = factor(data_study001$drinkingN_cat, c('No alcohol', 'High drinking', 'Extremely high drinking'), c(1,2,3))
table(data_study001$drinkingN_cat)
#Categorizing gestational week
#1: for gestational week less than 38 and 0: for gestational week greater than 38 weeks.
data_study001$gestation_cat <- cut(data_study001$gestation,
                                   breaks=c( 0, 38, 46),
                                   labels=c('Premature', 'Not Premature'))
data_study001$gestation_cat
table(data_study001$gestation_cat)
data_study001$gestation_cat = factor(data_study001$gestation_cat, c('Premature', 'Not Premature'), c(1,0))

#Lets try to create a summary summary table
data_study002 <- data_study001
data_study002$firstep <- 
  factor(data_study002$firstep, 
         levels=c(0,1),
         labels=c("Not Participated", # Reference
                  "Participated"))
table(data_study002$firstep)
data_study002$Gender <- 
  factor(data_study002$Gender, levels=c(1,0),
         labels=c("Male", 
                  "Female"))

data_study002$Drinking_status <- 
  factor(data_study002$Drinking_status, levels=c(0,1),
         labels=c("Not drinking", 
                  "Drinking"))

data_study002$Smoking_status <- 
  factor(data_study002$Smoking_status, levels=c(0,1),
         labels=c("Not Smoking", 
                  "Smoking"))
data_study002$Welfare <- 
  factor(data_study002$welfare, levels=c(0,1),
         labels=c("No", 
                  "Yes"))
data_study002$Marriage <- 
  factor(data_study002$married, levels=c(0,1),
         labels=c("No", 
                  "Yes"))
data_study002$Race <- 
  factor(data_study002$Race, levels=c(1,2,3,4,5),
         labels=c("Asian","Black","Hispanic","White","Other"))

data_study002$Parity <- 
  factor(data_study002$parity_cat, levels=c(1,2,3),
         labels=c("No prior children","Moderate prior children","Multiple prior children"))
data_study002$SMokingN <- 
  factor(data_study002$smokingN_cat, levels=c(1,2,3),
         labels=c('No smoking', 'Significant smoking', 'Serious smoking'))
data_study002$DrinkingN <- 
  factor(data_study002$drinkingN_cat, levels=c(1,2,3),
         labels=c('No alcohol', 'High drinking', 'Extremely high drinking'))
data_study002$Gestation <- 
  factor(data_study002$gestation_cat, levels=c(1,0),
         labels=c('Premature', 'Not Premature'))

data_study002$age

label(data_study002$Gender)       <- "Sex"
label(data_study002$plural)       <- "Multiplicity of birth"
label(data_study002$age) <- "Age"
label(data_study002$Race) <- "Race"
label(data_study002$parity) <- "Number of children"
label(data_study002$Marriage) <- "Marriage status"
label(data_study002$bwt) <- "Birth weight"
label(data_study002$smokeN) <- "Number of cigarettes"
label(data_study002$drinkN) <- "Number of drinks"
label(data_study002$Welfare) <- "Welfare benificiary"
label(data_study002$Smoking_status) <- "Smoking status"
label(data_study002$Drinking_status) <- "Drinking status"
label(data_study002$wpre) <- "Pregnancy weight"
label(data_study002$wgain) <- "Pregnancy weight gain"
label(data_study002$education) <- "Education"
label(data_study002$gestation) <- "Week of birth"
label(data_study002$Parity) <- "Prior birth"
label(data_study002$SMokingN) <- "Daily Smoking"
label(data_study002$DrinkingN) <- "Daily Drinking"
label(data_study002$Gestation) <- "Prematurity"

units(data_study002$age)       <- "years"
units(data_study002$bwt) <- "gm"
units(data_study002$wpre) <- "points"
units(data_study002$wgain) <- "lbs"


pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ Gender + age+Race + Parity + Marriage + bwt+SMokingN+DrinkingN+Welfare+Smoking_status+Drinking_status+wpre+wgain+education+Gestation | firstep, data=data_study002, overall="Total",extra.col=list(`P-value`=pvalue))
table1(~ Gender +Parity + Marriage + SMokingN+Welfare+Smoking_status+Gestation | firstep, data=data_study002, overall="Total",extra.col=list(`P-value`=pvalue))



t.test (age~firstep, var.equal=TRUE, data_study001)
t.test (wgain~firstep, var.equal=TRUE, data_study001)
t.test (wpre~firstep, var.equal=TRUE, data_study001)
t.test (education~firstep, var.equal=TRUE, data_study001)
t.test (bwt~firstep, var.equal=TRUE, data_study001)

#Fisher test
fisher.test(table(data_study002$DrinkingN, data_study002$firstep))
fisher.test(table(data_study002$Drinking_status, data_study002$firstep),alternative="less")
table(data_study002$firstep)

#Chi-square
chisq.test(table(data_study002$Race, data_study002$firstep),simulate.p.value=TRUE)
mosaicplot(table(data_study002$Race, data_study002$firstep),
           main = "Race distribution",
           color = TRUE
)
#Summary table supplemental information
summary(data_study002[data_study002$firstep=="Participated",]$age)
summary(data_study002[data_study002$firstep=="Not Participated",]$age)
summary(data_study002$age)

summary(data_study002[data_study002$firstep=="Participated",]$bwt)
summary(data_study002[data_study002$firstep=="Not Participated",]$bwt)
summary(data_study002$bwt)

summary(data_study002[data_study002$firstep=="Participated",]$wpre)
summary(data_study002[data_study002$firstep=="Not Participated",]$wpre)
summary(data_study002$wpre)

summary(data_study002[data_study002$firstep=="Participated",]$wgain)
summary(data_study002[data_study002$firstep=="Not Participated",]$wgain)
summary(data_study002$wgain)

summary(data_study002[data_study002$firstep=="Participated",]$education)
summary(data_study002[data_study002$firstep=="Not Participated",]$education)
summary(data_study002$education)


#Testing for outliers
out <- boxplot.stats(data_study002$age)$out


#Testing for outliers, using Rosner's
library(EnvStats)
test_outliers <- rosnerTest(data_study002$age)
test_outliers
test_outliers$all.stats


#Testing for outliers
out <- boxplot.stats(data_study002$bwt)$out
out
ggplot(data_study002) +
  aes(x = "", y = bwt) +
  geom_boxplot(fill = "#0e4a80") +
  xlab("") + ylab("Birth Weight (grams)")+
  theme_minimal()
#Testing for outliers, using Rosner's
test_outliers <- rosnerTest(data_study002$bwt
                            )
test_outliers
test_outliers$all.stats


#Testing for outliers
out <- boxplot.stats(data_study002$wpre)$out
out
ggplot(data_study002) +
  aes(x = "", y = wpre) +
  geom_boxplot(fill = "#0e4a80") +
  xlab("") + ylab("Pregnancy weight (points)")+
  theme_minimal()
#Testing for outliers, using Rosner's
test_outliers <- rosnerTest(data_study002$wpre
)
test_outliers
test_outliers$all.stats

#Testing for outliers
out <- boxplot.stats(data_study002$wgain)$out
out
ggplot(data_study002) +
  aes(x = "", y = wgain) +
  geom_boxplot(fill = "#0e4a80") +
  xlab("") + ylab("Maternal weight gain")+
  theme_minimal()
#Testing for outliers, using Rosner's
test_outliers <- rosnerTest(data_study002$wgain
)
test_outliers
test_outliers$all.stats


#Testing for outliers
out <- boxplot.stats(data_study002$education)$out
out
ggplot(data_study002) +
  aes(x = "", y = education) +
  geom_boxplot(fill = "#0e4a80") +
  xlab("") + ylab("Maternal weight gain")+
  theme_minimal()
#Testing for outliers, using Rosner's
test_outliers <- rosnerTest(data_study002$education
)
test_outliers
test_outliers$all.stats


#test skewness
skewness(data_study002$bwt)
ggplot(data_study002, aes(x=bwt)) + 
  geom_histogram(binwidth=1, color="black", fill="white")+
  ylab("Percentage count") + xlab("Birth weight (grams)") +
  ggtitle("Histogram plot")


#Assessing the relationship of dichotomous variables with birth weight
table1(~ bwt | Marriage, data=data_study002, overall="Total")
table1(~ bwt | Welfare, data=data_study002, overall="Total")
table1(~ bwt | Smoking_status, data=data_study002, overall="Total")

#Plotting graph

ggplot(data_study002, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step")+
  theme(legend.position = "none")


ggplot(data_study002, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  facet_grid(~Smoking_status) +
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step")+
  theme(legend.position = "none")


ggplot(data_study002, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  facet_grid(~Welfare) +
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step")+
  theme(legend.position = "none")  

ggplot(data_study002, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  facet_grid(~Marriage) +
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step")+
  theme(legend.position = "none")  

#T-test for smoking status across Step groups
df1 <- subset(data_study002, Smoking_status=='Smoking')
df2 <- subset(data_study002, Smoking_status=='Not Smoking')
t.test (bwt~firstep, var.equal=TRUE, data=df1)
t.test (bwt~firstep, var.equal=TRUE, data=df2)

ggplot(data_study002, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  facet_grid(~Race) +
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step")+
  theme(legend.position = "none")  

df3 <- subset(data_study002, Race=='Asian')
t.test (bwt~firstep, var.equal=TRUE, data=df3)


#Proportion of birth weights 
data_study002$BWT <- cut(data_study002$bwt,
                                breaks=c(0, 1500, 2500,5175),
                                labels=c('Very low', 'low','Normal'))
table(data_study002$BWT)
label(data_study002$BWT) <- "Birth Weight"
table1(~ BWT | firstep, data=data_study002, overall="Total",extra.col=list(`P-value`=pvalue))
chisq.test(table(data_study002$BWT, data_study002$firstep),simulate.p.value=TRUE)


#The impact of outliers

Outliers = boxplot(data_study001$bwt)$out
dta_x <- data_study001[-which(data_study001$bwt %in% Outliers),]
dta_x$firstep <- 
  factor(dta_x$firstep, 
         levels=c(0,1),
         labels=c("Not Participated", # Reference
                  "Participated"))
t.test (bwt~firstep, var.equal=TRUE, dta_x)

ggplot(dta_x, aes(x = firstep, y = bwt, fill=firstep)) +
  geom_boxplot() + 
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight and First Step - No outliers")+
  theme(legend.position = "none")

df4 <- subset(data_study002, BWT=='low')
ggplot(df4, aes(x = firstep, y = df4$bwt, fill=firstep)) +
  geom_boxplot() + 
  facet_grid(~Smoking_status)+
  xlab("First Step") + ylab("Birth Weight")+
  ggtitle("Birthweight ([1500-2500]) and First Step")+
  theme(legend.position = "none")


library(tidyverse)

df <- data.frame(race = c("White", "Black", "Asian"))
df %>%
  mutate(race_factored = as.numeric(factor(race)))

