# We first load necessary libraries and functions
library('ggplot2')
source('helper_functions.R')
library(dplyr)
library(plyr)
library(tidyverse)


### START Open data file ###
# Insert code below that loads your dataset into a variable called 'rawdata'
dataset17 <- read.csv("~/Desktop/dataset17.csv")
rawdata <- dataset17

### END Open data file ###


### START Explore through visual inspection ###
# Insert code below to check your dataset by looking at the raw data
rawdata

#missing data? 
unique(is.na(rawdata))

plot(rawdata$ID, rawdata$rt)
hist(rawdata$rt, breaks=250)
### END Explore through visual inspection ###


### START How large is N? ###
# Insert code below to check how many subjects does the dataset contains
unique <- unique(rawdata$ID)
n<-length(unique)
n

#datapoints per participant
rawdata %>%
  count(ID)

### END How large is N? ###


### START Change condition and correct to factor ###
# For ggplot, we need to make sure our condition and correct columns are coded as factors
# Insert code below to change these columns to factors

rawdata$condition <- as.factor(rawdata$condition)
rawdata$correct <- as.factor(rawdata$correct)

### END Change condition and correct to factor ###


### START Show histogram of distribution ###
# Insert code below to look at the distribution of response times, using either ggplot or hist()
# It may be necessary to reduce the limits of the x-axis
ggplot(data=rawdata, aes(x=rt))+
  geom_histogram(bins=100)+
  xlim(0,2000)

### END Show histogram of distribution ###


### START Remove extreme values
# Insert code below to remove outliers
#determining cutoff value for outliers via 3SD, based on literature
summary(rawdata)
threesd <- 3*sd(rawdata$rt)
threesd

cutoff<- mean(rawdata$rt) + threesd
cutoff

cutoff_values <- rawdata[(rawdata$rt > cutoff),]
cutoff_values
summary(cutoff_values)
#117 excluded datapoints 
#90.6% are in condition 1 aka incongruent condition 
#more  correct answers excluded than incorrect 
boxplot(rawdata$rt)
rawdata2 <- rawdata[!(rawdata$rt > cutoff),]
boxplot(rawdata2$rt)
max(rawdata2$rt)

#Visualization of datapoint pre and post outlier extraction 
#pre
ggplot(rawdata, aes(x = ID, y = rt), xlim=c(0:12)) +
  geom_point(aes(color = rt), color="#2972b6") +
  labs(y= "Response time (ms)", x = "ID") 

#post
ggplot(rawdata2, aes(x = ID, y = rt)) +
  geom_point(aes(color = rt), color="#2972b6")+
  labs(y= "Response time (ms)", x = "ID")

#look at the new data again
describe(rawdata2)
summary(rawdata2)
#One previous article uses 3SD, other article uses 1500 -> we are more conservative with 1169.736
# 1.22% of the data was outliers (in line with literature/ 1. other paper -> 1.3 speed & 0.8 accuracy; 
# 2. paper 4% -< more or less in line)

### END Remove extreme values


### START Show histogram for correct and incorrect
# Insert ggplot code to show RTs for both correct and incorrect as overlaying distributions
# HINT: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# Find the mean of each group
rawdata2_corr <- ddply(rawdata2, "correct", summarise, rt.mean=mean(rt))
rawdata2_corr

#Histograms with means
ggplot(rawdata2, aes(x = rt, fill = correct)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  geom_vline(data=rawdata2_corr, aes(xintercept=rt.mean,  colour=correct),
              linetype="dashed", size=0.5)

rawdata_mod_corr <- rawdata2
rawdata_mod_corr$correct <- recode_factor(rawdata_mod_corr$correct, '0' = 'incorrect', '1' = 'correct')
ggplot(rawdata_mod_corr, aes(x = rt)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(correct ~ .) 

#Overlapping histograms for reaction times split in two groups by "correct"
ggplot(rawdata_mod, aes(x = rt, fill = correct, color=correct )) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_color_manual(values=c("#4ebcff", "#945cb4"))+
  scale_fill_manual(values=c("#4ebcff", "#945cb4")) +
  xlim(0, 2000) +
labs(y= "Frequency", x = "Response time (ms)")

### END Show histogram for correct and incorrect

### START Do the same for condition 1 vs condition 2
# Insert ggplot code to show RTs for both condition 1 and condition 2 as overlaying distributions
rawdata2_cond <- ddply(rawdata2, "condition", summarise, rt.mean=mean(rt))
rawdata2_cond

ggplot(rawdata2, aes(x = rt, fill = condition)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  geom_vline(data=rawdata2_cond, aes(xintercept=rt.mean,  colour=condition),
             linetype="dashed", size=0.5)

rawdata_mod2_con <- rawdata2
rawdata_mod2_con$condition <- recode_factor(rawdata_mod2_con$condition, '1' = 'incongruent', '2' = 'congruent')
ggplot(rawdata_mod2_con, aes(x = rt)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(condition ~ .)

#Overlapping histograms for reaction times split in two groups by "condition"
ggplot(rawdata_mod2_con, aes(x = rt, fill=condition, color = condition)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  scale_color_manual(values=c("#4ebcff", "#945cb4"))+
  scale_fill_manual(values=c("#4ebcff", "#945cb4")) +
  xlim(0, 2000) +
  labs(y= "Frequency", x = "Response time (ms)")

#Only the correct answers in the incongruent trials
rawdata2_cond1_cor1 <- rawdata2 %>%
  filter(correct == 1, condition == 1)
rawdata2_cond1_cor1
summary(rawdata2_cond1_cor1)

ggplot(rawdata2_cond1_cor1, aes(x = rt, fill = condition)) +
  geom_histogram(bins=100, position = "identity", alpha = 0.5)

##To get a better understanding of the different conditions/ outcomes
#Only the wrong answers in the incongruent trials
rawdata2_cond1_cor0 <- rawdata2 %>%
  filter(correct == 0, condition == 1)
rawdata2_cond1_cor0
summary(rawdata2_cond1_cor0)

ggplot(rawdata2_cond1_cor0, aes(x = rt, fill = condition)) +
  geom_histogram(bins=100, position = "identity", alpha = 0.5)

#Only the correct answers in the congruent trials
rawdata2_cond2_cor1 <- rawdata2 %>%
  filter(correct == 1, condition == 2)
rawdata2_cond2_cor1
summary(rawdata2_cond2_cor1)

ggplot(rawdata2_cond2_cor1, aes(x = rt, fill = condition)) +
  geom_histogram(bins=100, position = "identity", alpha = 0.5)

#Only the wrong answers in the congruent trials
rawdata2_cond2_cor0 <- rawdata2 %>%
  filter(correct == 0, condition == 2)
rawdata2_cond2_cor0
summary(rawdata2_cond2_cor0)

ggplot(rawdata2_cond2_cor0, aes(x = rt, fill = condition)) +
  geom_histogram(bins=100, position = "identity", alpha = 0.5)

### END Do the same for condition 1 vs condition 2


### START Inspect data: paired t-test over RT data by condition
# Insert code below to check whether RTs differ by condition. Note: you should do this by aggregating data
# by subject, using the median (due to skewed distribution of response times!)
rawdata_median <- aggregate(rawdata2$rt, by = list(ID = rawdata2$ID, condition = rawdata2$condition), FUN="median")
rawdata_median

res <- t.test(x ~ condition, data = rawdata_median, paired = TRUE)
res
rawdata_median2 <- aggregate(rawdata2$rt, by = list(ID = rawdata2$ID, correct = rawdata2$correct), FUN="median")
rawdata_median2

res <- t.test(x ~ correct, data = rawdata_median2, paired = TRUE)
res
### END Inspect data: paired t-test over RT data by condition

#!!! for next step, need to overwrite rawdata 
rawdata <- rawdata2
summary(rawdata)
str(rawdata)


### START Fit model for each participant x condition
str(rawdata2)
fit_data(rawdata2)

#For each individual 
fit_data(subset(rawdata2, ID==1 & condition==1))
fit_data(subset(rawdata2, ID==1 & condition==2))
fit_data(subset(rawdata2, ID==2 & condition==1))
fit_data(subset(rawdata2, ID==2 & condition==2))
fit_data(subset(rawdata2, ID==3 & condition==1))
fit_data(subset(rawdata2, ID==3 & condition==2))
fit_data(subset(rawdata2, ID==4 & condition==1))
fit_data(subset(rawdata2, ID==4 & condition==2))
fit_data(subset(rawdata2, ID==5 & condition==1))
fit_data(subset(rawdata2, ID==5 & condition==2))
fit_data(subset(rawdata2, ID==6 & condition==1))
fit_data(subset(rawdata2, ID==6 & condition==2))
fit_data(subset(rawdata2, ID==7 & condition==1))
fit_data(subset(rawdata2, ID==7 & condition==2))
fit_data(subset(rawdata2, ID==8 & condition==1))
fit_data(subset(rawdata2, ID==8 & condition==2))
fit_data(subset(rawdata2, ID==9 & condition==1))
fit_data(subset(rawdata2, ID==9 & condition==2))
fit_data(subset(rawdata2, ID==10 & condition==1))
fit_data(subset(rawdata2, ID==10 & condition==2))
fit_data(subset(rawdata2, ID==11 & condition==1))
fit_data(subset(rawdata2, ID==11 & condition==2))
fit_data(subset(rawdata2, ID==12 & condition==1))
fit_data(subset(rawdata2, ID==12 & condition==2))

### END Fit model for each participant x condition

### START Check which parameter(s) differ between conditions
#Here I create a subset for every case/ condition/ outcome to look at the parameetrs seperately again
part_1 <- subset(rawdata2, ID==1)
part_2 <- subset(rawdata2, ID==2)
part_3 <- subset(rawdata2, ID==3)
part_4 <- subset(rawdata2, ID==4)
part_5 <- subset(rawdata2, ID==5)
part_6 <- subset(rawdata2, ID==6)
part_7 <- subset(rawdata2, ID==7)
part_8 <- subset(rawdata2, ID==8)
part_9 <- subset(rawdata2, ID==9)
part_10 <- subset(rawdata2, ID==10)
part_11 <- subset(rawdata2, ID==11)
part_12 <- subset(rawdata2, ID==12)

Incongruent <- subset(rawdata2, condition==1)
Congruent <- subset(rawdata2, condition==2)

Correct_answ <- subset(rawdata2, correct==1)
Incorrect_answ <- subset(rawdata2, correct==0)

#For final analysis and the selected plots i created a matrix, dataframes, ggplots, 
#and performed paired-ttests
#For-loop getting the fit values for all participants divided into two conditions 
fitMatrix = c()
for (subj in unique(rawdata2$ID)) {
  for (cond in 1:2) {
    parameterresults = fit_data(subset(rawdata2, ID == subj & condition == cond))
    print(parameterresults)
    fitMatrix = rbind(fitMatrix, c(subj, cond, parameterresults))
  }
}

fitMatrix
colnames(fitMatrix) <- c("ID", "cond", "s", "A", "ter", "b", "v1")
fit_df <- data.frame(fitMatrix)
fit_df

ttestCondS <- t.test(s ~ cond, data = fit_df, paired = TRUE)
ttestCondA <- t.test(A ~ cond, data = fit_df, paired = TRUE)
ttestCondter <- t.test(ter ~ cond, data = fit_df, paired = TRUE)
ttestCondb <- t.test(b ~ cond, data = fit_df, paired = TRUE)
ttestCondv1 <- t.test(v1 ~ cond, data = fit_df, paired = TRUE)

ttestCondS
ttestCondA
ttestCondter
ttestCondb
ttestCondv1

ggboxplot(fit_df, 
          x = "cond", 
          y = "s",
          fill = "cond", 
          alpha= 0.6,
          ylab = "SD of drift rate (s)", 
          xlab = "Conditions") +
  geom_jitter(color="red", size=0.3, alpha=0.7, position = position_jitter(width = 0.03)) +
  scale_fill_manual(values=c("#4ebcff", "#945cb4"))

ggboxplot(fit_df, 
          x = "cond", 
          y = "A",
          fill = "cond", 
          alpha= 0.6,
          ylab = "Upper value starting point (A)", 
          xlab = "Conditions") +
  geom_jitter(color="red", size=0.3, alpha=0.7, position = position_jitter(width = 0.03)) +
  scale_fill_manual(values=c("#4ebcff", "#945cb4"))

ggboxplot(fit_df, x = "cond", 
          y = "ter",
          fill = "cond", 
          alpha= 0.6,
          ylab = "Non-decision time (ter)", 
          xlab = "Conditions") +
  geom_jitter(color="red", size=0.3, alpha=0.7, position = position_jitter(width = 0.03)) +
  scale_fill_manual(values=c("#4ebcff", "#945cb4"))
ggboxplot(fit_df, x = "cond", 
          y = "b",
          fill = "cond", 
          alpha= 0.6,
          ylab = "Threshold (b)", 
          xlab = "Conditions") +
  geom_jitter(color="red", size=0.3, alpha=0.7, position = position_jitter(width = 0.03)) +
  scale_fill_manual(values=c("#4ebcff", "#945cb4"))

ggboxplot(fit_df, x = "cond", 
          y = "v1",
          fill = "cond", 
          alpha= 0.6,
          ylab = "Mean drift rate (v1)", 
          xlab = "Conditions") +
  geom_jitter(color="red", size=0.3, alpha=0.7, position = position_jitter(width = 0.03)) +
  scale_fill_manual(values=c("#4ebcff", "#945cb4"))

#Finally, I created two tables to visualize some numbers better in the poster itself
#create matrix with 3 columns for frequency of outcomes 
Table_values <- matrix(c('958 (10.1%)', '132 (1.4%)', '1.090 (11.5%)', '3736 (39.4%)', '4657 (49.1%)', '8.393 (88.5%)', '4694 (49.5%)', '4.789 (50.5%)', '9.483 (100%)'), ncol=3, byrow=TRUE)

#define column names and row names of matrix
colnames(Table_values) <- c('Incongruent (1)', 'Congruent (2)', 'Total')
rownames(Table_values) <- c('Incorrect (0)', 'Correct (1)', 'Total')

#convert matrix to table 
Table_values <- as.table(Table_values)

#view table 
Table_values
grid.table(Table_values)

#Making table prettier
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:4], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="black", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L)))

grid.arrange(
  tableGrob(Table_values[1:3, 1:3], theme=tt3),
  nrow=1)

###
#create matrix with 2 columns for parameter values
Table_values_2 <- matrix(c('0.209', '0.232', '331.470', '338.570', '288.694', '296.490', '384.392', '385.827', '0.687', '0.972'), ncol=2, byrow=TRUE)

#define column names and row names of matrix
colnames(Table_values_2) <- c('Incongruent (1)', 'Congruent (2)')
rownames(Table_values_2) <- c('s', 'A', 'Ter', 'b', 'v1')

Table_values_2 <- as.table(Table_values_2)

#view table 
Table_values_2
grid.table(Table_values_2)

#Making table prettier
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:5], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="black", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L)))

grid.arrange(
  tableGrob(Table_values_2[1:5, 1:2], theme=tt3),
  nrow=1)

### THE END ###