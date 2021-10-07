# Title: Assignment 2
# Subtitle: Crim 250: Statistics for the Social Sciences
# Name: Tori Borlase
# Date: 09/27/2021


# Instructions: Copy your code, paste it into a Word document, and turn it into Canvas. You can turn in a .docx or .pdf file. Show any EDA (graphical or non-graphical) you have used to come to this conclusion.


# Problem 1: Load data

# Set your working directory to the folder where you downloaded the data.
setwd("/Users/toriborlase/Desktop/University of Pennsylvania/Fall 2021/CRIM 250/Tori-Borlase-Crim-250")

# Read the data
dat <- read.csv(file = 'dat.nsduh.small.1.csv')

# What are the dimensions of the dataset? 
dim(dat)

#Answer: 171 by 7. 171 rows and 7 columns.


## Problem 2: Variables

# Describe the variables in the dataset.

names(dat)

# Answer: The variables in the dataset are mjage, cigage, iralcage, age2, sexatract, speakengl, and irsex. According to the code book, these correspond to how old someone was when they first tried marijuana or hashish, how old someone was when they started smoking cigarettes every day, how old someone was when they first tried alcohol, the final age that someone was determined to be at the time of taking the survey (which was asked multiple times as a consistency check), which statement about sexual orientation best described the respondent's feelings, how well they speak English, and imputation revised gender.

# What is this dataset about? Who collected the data, what kind of sample is it, and what was the purpose of generating the data?

# Answer: This dataset is from the 2019 National Survey on Drug Use and Health (NSDUH). Individuals within the Center for Behavioral Health Statistics and Quality collected this data in order to measure the "prevalence and correlates of substance use and mental health issues" in the US. This was a stratified random sample, as they collected data in all 50 states for civilian and non-institutionalized populations.
  

## Problem 3: Age and gender

# What is the age distribution of the sample like? Make sure you read the codebook to know what the variable values mean.

counts <- table(dat$age2)
barplot(counts, main="Histogram of Ages of Participants", xlab="Age Category", ylab="Frequency")

# Answer: The age distribution of this sample is skewed left.  However, because the numbers within the data set are not actual ages, but rather a range of ages or individual ages that are coded as numbers 1 through 17, we cannot base our true age distribution off of the numbers that form the labels of the histogram. While it appears that the Final Edited Age is skewed left, this may just be because the upper coded ages contain wider ranges of ages, and the lower coded ages contain only one or two ages.

# Do you think this age distribution representative of the US population? Why or why  not?

# No, I do not believe this age distribution is representative of the US population. According to Census data, and other population pyramids that display the distribution of population, there are many more younger people than are represented in this graph, as well as many individuals outside of the scope of the study, such as those under 12, etc. Even though (as I mentioned before) there are multiple ages within certain categories, this data shows many more older individuals compared to teenagers and those in their early twenties, and Census data shows large numbers of those populations.

# Is the sample balanced in terms of gender? If not, are there more females or males?

table(dat$irsex)

# 1 (Male)  2 (Female)
#    91         80 

counts <- table(dat$irsex)
barplot(counts, main="Gender Distribution", xlab="Gender", ylab="Frequency", names=c("Male", "Female"))

# The sample is not gender balanced, as there are more males (91) than females (80).  Using a bar chart clearly demonstrates this disparity.

# Use this code to draw a stacked bar plot to view the relationship between sex and age. What can you conclude from this plot?

tab.agesex <- table(dat$irsex, dat$age2)
barplot(tab.agesex,
        main = "Age and Gender Comparison",
        xlab = "Age Category", ylab = "Frequency",
        legend.text = c("Male", "Female"), xlim = c(0,17),
        beside = FALSE) # Stacked bars (default)

# We can conclude from this plot that for most age categories, there were more males than females. However, for age categories 8, 9, 13, and 15, there appear to be more females or the same number of males and females.


## Problem 4: Substance use

# For which of the three substances included in the data set (marijuana, alcohol, and cigarettes) do individuals tend to use the substance earlier?

par(mfrow = c(3,1))
hist(dat$mjage, main="Histogram of Age of First Marijuana Use", xlab="Age Categories", ylab="Frequency", xlim=c(0,50), ylim=c(0,50), breaks = 20)
hist(dat$iralcage, main="Histogram of Age of First Alcohol Use", xlab="Age Categories", ylab="Frequency", xlim=c(0,50), ylim=c(0,50), breaks = 20)
hist(dat$cigage, main="Histogram of Age of Starting to Smoke Cigarettes Daily", xlab="Age Categories", ylab="Frequency", xlim=c(0,50), ylim=c(0,50), breaks = 20)

# Individuals tend to use alcohol earlier, as seen on the histograms.


## Problem 5: Sexual attraction

# What does the distribution of sexual attraction look like? Is this what you expected?


dat1 <- dat[dat$sexatract !=99,]
counts1 <- table(dat1$sexatract)
barplot(counts1, main="Sexual Attraction Distribution", xlab="Sexual Attraction Categories", ylab="Frequency", ylim = c(0,150))

# The distribution of sexual attraction is skewed right, which is what I expected, as most people identify as only being attracted to the opposite sex, with fewer people identifying as being attracted to the same sex in any way. This is what I expected because I believe that LGBT+ populations are still a relatively small minority within the US compared to those who are only attracted to the opposite sex.

# What is the distribution of sexual attraction by gender? 

dat1 <- dat[dat$sexatract !=99,] 
tab.sexor <- table(dat1$irsex, dat1$sexatract)
barplot(tab.sexor,
        main = "Sexual Attraction and Gender Comparison",
        xlab = "Sexual Attraction Category", ylab = "Frequency",
        legend.text = c("Male", "Female"), xlim = c(0,7), ylim = c(0,140),
        beside = FALSE) # Stacked bars (default)

# The distribution by gender is also skewed right, but there are more females that identify with statements about being attracted to the same sex in some way, and more males that identify with statements about being attracted to the opposite sex, as can be seen in the stacked bar chart.


## Problem 6: English speaking

# What does the distribution of English speaking look like in the sample? Is this what you might expect for a random sample of the US population?

counts2 <- table(dat$speakengl)
barplot(counts2, main="English Language Category Frequency", xlab="English Speaking Categories", ylab="Frequency", ylim = c(0,200))

# This distribution is also skewed right, with most individuals responding that they speak English very well, and less than 50 individuals total saying that the speak English well or not well, and nobody saying they did not speak English at all.  This is similar to the distribution that I would expect in the United States because even though the US has no official language, most people need to speak some English in order to work and live here. However, there are probably more people in the US that speak no English at all, but they just were not able to participate in the survey because it was conducted in English. 

# Are there more English speaker females or males?

table(dat$irsex, dat$speakengl)
tab.sexor <- table(dat$irsex, dat$speakengl)
barplot(tab.sexor,
        main = "English Skill and Gender Comparison",
        xlab = "English Ability Category", ylab = "Frequency",
        legend.text = c("Male", "Female"), xlim = c(0,4), ylim = c(0,200),
        beside = FALSE) # Stacked bars (default)

# There are more male English speakers than female English speakers, but that might also be due to the fact that there are more males than females within the data set to begin with.