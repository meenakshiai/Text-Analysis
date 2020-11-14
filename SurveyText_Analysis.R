##import libraries

library(lubridate)
library(plyr)
library(pander)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)



## read the data while removing 3rd row and start row as second row.
sample <- read.csv("Sample Data.csv", skip = 1, stringsAsFactors = FALSE)[-c(1), ]


heading <- c("Start.Date", "End.Date", "ResponseType", "IP Add", "Progress", "Duration in Secs", "Finished", "Recorded.Date", 
             "LastName", "FirstName",
             "Email", "External.Data.Reference", "Loc.Latitude", "Loc.Longitude", "Channel", "Language",
             "recommend", "RecommendRating", "Law Practice", "LawPracOther", "Legalarea", "LegalareaOther", 
             "ImproveGGU", "ImproveGGUother", "HowGGUsupportYou", "HowGGUsupportyouother", "Wouldyouattendregional",
             "AttendLawschool", "Whynotlaw", "Whynotother", "ImprovementRecommend", "Comments", "LawReunionLikes")

names(sample) <- heading


##Convert date(integer value) to date for analysis.

sample$Start.Date <- as.Date(sample$Start.Date, format = "%m/%d/%Y")

sample$End.Date <- as.Date(sample$End.Date, format = "%m/%d/%Y")

sample$Recorded.Date <- as.Date(sample$Recorded.Date, format = "%m/%d/%Y")


## histogram of the survey Start and end date.
hist(sample$Start.Date, "days", freq = TRUE)
hist(sample$End.Date, "days", freq = TRUE)


##convert to numeric for plot, heatmap etc.
sample$Finish <- ifelse(sample$Finished == "TRUE",1,0)

plot(sample$Start.Date, sample$Finish)

##bar plot for finished survey
finishsurvey <- table(sample$Finish)

par(mar=c(2, 5, 0.08, 0.05))
barplot(finishsurvey, beside = T,las=2, ylim = c(0,550),col=c("darkgreen","orange"),
        main="No. of students who finished or did not finish the survey", ylab = "no of students in the survey",
        legend=c("FALSE", "TRUE"),args.legend = list(x="topright",bty="n", 
                                                     inset=.02), yaxp=c(0,550,5))

##covert rating given for recommending GGU to binary

sample$recommendDummy <- ifelse(sample$recommend == "Passive", 0,
         ifelse(sample$recommend == "Promoter",1,
                ifelse(sample$recommend == "Detractor",2,3)))

##bar plot for rating given for recommending in words
recommendggu <- table(sample$recommendDummy)

par(mar=c(2, 5, 0.08, 0.05))
barplot(recommendggu, beside = T,legend=c("Passive", "Promoter", "Detractor", "Not Sure"),
        main = "How likely students promote GGU", xlab = "Net Promoter Score", 
        ylab = "No of students in the survey",
        las=2, ylim = c(0,220),col=c("lightgreen","forestgreen","orange","orange"),
        args.legend = list(x="topright",bty="n", inset=.02), yaxp=c(0,220,5))



##remove the NAs by using complete cases

dfrating <- sample$RecommendRating

dfrating[dfrating==""] <- NA

dfrating <- na.omit(dfrating)


## bar chart of rating column
dfnewrating <- table(as.integer(dfrating))


par(mar=c(5, 5, 2, 0.05))
barplot(dfnewrating, beside = T,
        las=2, ylim = c(0,110),
        col=c("orange","orange","orange","orange","orange","orange","orange","lightgreen",
              "lightgreen","forestgreen", "forestgreen"), main = "Net Promoter rating by students",
        xlab = "NPS rating", ylab = "No of Students",
        legend=c("0-6 Detractor", "7-8 Passive","9-10 Promoter"),
                args.legend = list(x="topleft",bty="n", inset=.02),yaxp=c(0,110,5))


##Removing NA's from the column "students working in legal area"

legalpractice <- sample$Legalarea
legalpractice[legalpractice==""] <- NA
legalpractice <-na.omit(legalpractice)


##1) start with the text analysis process with tidytext.

text_df <- tibble(line = 1: length(legalpractice), text = legalpractice)


## break the text into individual tokens called tokenization using unnest

newtextdf <- text_df %>%
  unnest_tokens(word, text)

## remove the stop words which are not useful and stored under stopwords list.

data("stop_words")

newtextdf <- newtextdf %>%
  anti_join(stop_words)

## Now count the most common words.
newtextdf %>%
  count(word, sort = TRUE)

## Use ggplot2 for the visualization.

newtextdf %>%
  count(word, sort = TRUE) %>%
  filter(n < 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


##Text analysis for column "how can GGU improve".
##2)Removing NA's from the column "students working in legal area"

improvggu <- sample$ImproveGGU
improvggu[improvggu==""] <- NA
improvggu <-na.omit(improvggu)

##start with the text analysis process with tidytext.

improveggu_df <- tibble(line = 1: length(improvggu), text = improvggu)


## break the text into individual tokens called tokenization using unnest

Newimproveggu_df <- improveggu_df %>%
  unnest_tokens(word, text)

## remove the stop words which are not useful and stored under stopwords list.

data("stop_words")

Newimproveggu_df <- Newimproveggu_df %>%
  anti_join(stop_words)

## Now count the most common words.
Newimproveggu_df %>%
  count(word, sort = TRUE)

## Use ggplot2 for the visualization.

Newimproveggu_df %>%
  count(word, sort = TRUE) %>%
  filter(n < 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## 3) Text analysis only for detractor ratings.

Detractordf <- data.frame(sample[sample$recommend == "Detractor", ])

##Removing NA's from the column "Why Not Law? "

nolaw <- Detractordf$Whynotlaw

nolaw[nolaw==""] <- NA
nolaw <-na.omit(nolaw)

##start with the text analysis process with tidytext.

nolawdf <- tibble(line = 1: length(nolaw), text = nolaw)


## break the text into individual tokens called tokenization using unnest

nolawdf <- nolawdf %>%
  unnest_tokens(word, text)

## remove the stop words which are not useful and stored under stopwords list.

data("stop_words")

nolawdf <- nolawdf %>%
  anti_join(stop_words)

## Now count the most common words.
nolawdf %>% 
  count(word, sort = TRUE)

## Use ggplot2 for the visualization.

nolawdf %>%
  count(word, sort = TRUE) %>%
  filter(n < 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()




