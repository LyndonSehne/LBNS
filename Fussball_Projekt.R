---
  title: "Bundesliga"
author: "Lyndon Sehne"
date: "February 7th, 2022"
output: html_document
---
# Season defined by first year: ie Season 19/20 named Season 20. 
  
require(tidyverse)
#install.packages("readxl")
library("readxl")
#install.packages('Rcpp')
library(Rcpp)

# main data file
dat <- read_excel("Daten_Sehne.xlsx")
dat <- na.omit(dat)
dat <- as.data.frame(dat)
# data file reduced to one team
dat_VfB <- dat[dat$`Team Name` == "VfB Stuttgart",]



# data file reduced to team nr of injuries and injury days

# season 20/21 main focus as only full Corona-Season
dat_20 <- dat[dat$Saison == 2021,]
meandays_20 <- mean(dat_20$Days) # 20.79392 !!!! mean days = mean injury length/duration
sumdays_20 <- sum(dat_20$Days) # 27344


meandays_total <- mean(dat$Days) # 18.75512

# march 2020 Corona started so szn 10 (10/11) to 18 (18/19) considered "normal"
dat_beforeCorona <- dat[!(dat$Saison == 1920 | dat$Saison == 2021 | dat$Saison == 2122),]
meandays_before <- mean(dat_beforeCorona$Days) # 18.65701

dat_10 <- dat[dat$Saison == 1011,]
meandays_10 <- mean(dat_10$Days)
sumdays_10 <- sum(dat_10$Days)

dat_11 <- dat[dat$Saison == 1112,]
meandays_11 <- mean(dat_11$Days)
sumdays_11 <- sum(dat_11$Days)

dat_12 <- dat[dat$Saison == 1213,]
meandays_12 <- mean(dat_12$Days)
sumdays_12 <- sum(dat_12$Days)

dat_13 <- dat[dat$Saison == 1314,]
meandays_13 <- mean(dat_13$Days)
sumdays_13 <- sum(dat_13$Days)

dat_14 <- dat[dat$Saison == 1415,]
meandays_14 <- mean(dat_14$Days)
sumdays_14 <- sum(dat_14$Days)

dat_15 <- dat[dat$Saison == 1516,]
meandays_15 <- mean(dat_15$Days)
sumdays_15 <- sum(dat_15$Days)

dat_16 <- dat[dat$Saison == 1617,]
meandays_16 <- mean(dat_16$Days)
sumdays_16 <- sum(dat_16$Days)

dat_17 <- dat[dat$Saison == 1718,]
meandays_17 <- mean(dat_17$Days)
sumdays_17 <- sum(dat_17$Days)

dat_18 <- dat[dat$Saison == 1819,]
meandays_18 <- mean(dat_18$Days)
sumdays_18 <- sum(dat_18$Days)

# not intended for use (as mixed covid season)
dat_19 <- dat[dat$Saison == 1920,]
meandays_19 <- mean(dat_19$Days)
sumdays_19 <- sum(dat_19$Days)

# not intended for use (as still running)
dat_21 <- dat[dat$Saison == 2122,]
meandays_21 <- mean(dat_21$Days)
sumdays_21 <- sum(dat_21$Days)

season <- c(10:21)
mean_injury_length <- c(meandays_10, meandays_11, meandays_12, meandays_13, meandays_14, meandays_15, meandays_16, meandays_17, meandays_18, meandays_19, meandays_20, meandays_21)
sum_days <- c(sumdays_10, sumdays_11, sumdays_12, sumdays_13, sumdays_14, sumdays_15, sumdays_16, sumdays_17, sumdays_18, sumdays_19, sumdays_20, sumdays_21)
df <- data.frame(season, mean_injury_length, sum_days)

#install.packages("reactable")
library(reactable)

df_rounded <- format(df, digits = 4)

reactable(df_rounded, defaultPageSize = 12, compact =  TRUE, rownames =  FALSE,
          filterable = FALSE, columns = list(season = colDef(name = "Season", width = 100),
                                             mean_injury_length = colDef(name = "mean injury length", width = 120),
                                             sum_days = colDef(name = "sum days", width = 100)))


dat_14to18 <- dat[(dat$Saison == 1415 | dat$Saison == 1516 
                   | dat$Saison == 1617 | dat$Saison == 1718 |dat$Saison == 1819),]
meandays_14to18 <- mean(dat_14to18$Days) # 18.731


df1 <- dat[(dat$Saison == 1415 | dat$Saison == 1516 | dat$Saison == 1617 | dat$Saison == 1718 |dat$Saison == 1819 |dat$Saison == 2021),]
dfmain <- subset(df1, select=c("Saison", "Days"))
dfmain$Saison <- as.factor(dfmain$Saison)
library(ggplot2)
p <- ggplot(dfmain, aes(x=Saison, y=log(Days))) + 
  geom_boxplot(notch=TRUE)
p

p + stat_summary(fun.y=mean, geom="point", shape=23, size=4) + theme_classic() + labs(x = "Season")


# create a data frame that has 14/15 - 18/19 as normal and 20/21 as covid
df2 <- dfmain
df2$Saison <- as.character(df2$Saison)
df2$Saison[df2$Saison == "2021"] <- "Covid"
df2$Saison[df2$Saison != "Covid"] <- "Normal"
boxplot(log(df2$Days) ~ df2$Saison)


t_daysout <- t.test(df2$Days ~ df2$Saison, mu = 0, alt = "greater", conf=0.95, var.eq= F, paired = F)
t_daysout




# additional data files for team tables per year
#install.packages("XML")
library("XML")
library("methods")
# szn2010 <- xmlToDataFrame("2010.xml")
# szn201$Season <- 1
# szn2011 <- xmlToDataFrame("2011.xml")
# szn201$Season <- 1
# szn2012 <- xmlToDataFrame("2012.xml")
# szn201$Season <- 1
# szn2013 <- xmlToDataFrame("2013.xml")
# szn201$Season <- 1 !!!!!!!!!!!!!!!!!!!!!!!!!!! not used because other data set compromised
szn2014 <- xmlToDataFrame("2014.xml")
szn2014$Season <- 1415
szn2015 <- xmlToDataFrame("2015.xml")
szn2015$Season <- 1516
szn2016 <- xmlToDataFrame("2016.xml")
szn2016$Season <- 1617
szn2017 <- xmlToDataFrame("2017.xml")
szn2017$Season <- 1717
szn2018 <- xmlToDataFrame("2018.xml")
szn2018$Season <- 1819
szn2019 <- xmlToDataFrame("2019.xml") 
szn2019$Season <- 1920
szn2020 <- xmlToDataFrame("2020.xml")
szn2020$Season <- 2021
end_table <- rbind(szn2014,szn2015,szn2016,szn2017,szn2018, szn2019, szn2020)
end_table_main <- subset(end_table, select=c("ShortName", "Points", "Season"))
# nur die Mannschaften, die seit 2014 nicht abgestiegen sind
end_table_main <- end_table_main[(end_table_main$ShortName == "Bayern" | end_table_main$ShortName == "BVB" 
                                  |end_table_main$ShortName == "Leverkusen" |end_table_main$ShortName == "Wolfsburg" 
                                  |end_table_main$ShortName == "Hoffenheim" |end_table_main$ShortName == "Gladbach" 
                                  |end_table_main$ShortName == "Mainz" |end_table_main$ShortName == "Augsburg" 
                                  |end_table_main$ShortName == "Frankfurt" |end_table_main$ShortName == "Hertha"),]
#end_table_main$`Team ID`<- 66
#end_table_main$SummedDays<- 66
end_table_main$`Team ID`[end_table_main$ShortName == "Bayern"] <- 301
end_table_main$`Team ID`[end_table_main$ShortName == "BVB"] <- 302
end_table_main$`Team ID`[end_table_main$ShortName == "Leverkusen"] <- 303
end_table_main$`Team ID`[end_table_main$ShortName == "Wolfsburg"] <- 311
end_table_main$`Team ID`[end_table_main$ShortName == "Hoffenheim"] <- 316
end_table_main$`Team ID`[end_table_main$ShortName == "Gladbach"] <- 308
end_table_main$`Team ID`[end_table_main$ShortName == "Mainz"] <- 313
end_table_main$`Team ID`[end_table_main$ShortName == "Augsburg"] <- 315
end_table_main$`Team ID`[end_table_main$ShortName == "Frankfurt"] <- 306
end_table_main$`Team ID`[end_table_main$ShortName == "Hertha"] <- 317

# this is too much at once, and I can`t get it done`
#end_table_main$SummedDays <- sum(dat$Days[dat$Saison == end_table_main$Season & dat$`Team ID` == end_table_main$`Team ID`])

dat_14_Bayern <- dat_14[dat_14$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 301] <- sum(dat_14_Bayern$Days)

dat_15_Bayern <- dat_15[dat_15$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 301] <- sum(dat_15_Bayern$Days)

dat_16_Bayern <- dat_16[dat_16$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 301] <- sum(dat_16_Bayern$Days)

dat_17_Bayern <- dat_17[dat_17$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 301] <- sum(dat_17_Bayern$Days)

dat_18_Bayern <- dat_18[dat_18$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 301] <- sum(dat_18_Bayern$Days)

dat_19_Bayern <- dat_19[dat_19$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 301] <- sum(dat_19_Bayern$Days)

dat_20_Bayern <- dat_20[dat_20$`Team ID` == "301",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 301] <- sum(dat_20_Bayern$Days)



dat_14_BVB <- dat_14[dat_14$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 302] <- sum(dat_14_BVB$Days)

dat_15_BVB <- dat_15[dat_15$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 302] <- sum(dat_15_BVB$Days)

dat_16_BVB <- dat_16[dat_16$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 302] <- sum(dat_16_BVB$Days)

dat_17_BVB <- dat_17[dat_17$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 302] <- sum(dat_17_BVB$Days)

dat_18_BVB <- dat_18[dat_18$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 302] <- sum(dat_18_BVB$Days)

dat_19_BVB <- dat_19[dat_19$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 302] <- sum(dat_19_BVB$Days)

dat_20_BVB <- dat_20[dat_20$`Team ID` == "302",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 302] <- sum(dat_20_BVB$Days)



dat_14_Leverkusen <- dat_14[dat_14$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 303] <- sum(dat_14_Leverkusen$Days)

dat_15_Leverkusen <- dat_15[dat_15$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 303] <- sum(dat_15_Leverkusen$Days)

dat_16_Leverkusen <- dat_16[dat_16$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 303] <- sum(dat_16_Leverkusen$Days)

dat_17_Leverkusen <- dat_17[dat_17$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 303] <- sum(dat_17_Leverkusen$Days)

dat_18_Leverkusen <- dat_18[dat_18$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 303] <- sum(dat_18_Leverkusen$Days)

dat_19_Leverkusen <- dat_19[dat_19$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 303] <- sum(dat_19_Leverkusen$Days)

dat_20_Leverkusen <- dat_20[dat_20$`Team ID` == "303",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 303] <- sum(dat_20_Leverkusen$Days)




dat_14_Wolfsburg <- dat_14[dat_14$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 311] <- sum(dat_14_Wolfsburg$Days)

dat_15_Wolfsburg <- dat_15[dat_15$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 311] <- sum(dat_15_Wolfsburg$Days)

dat_16_Wolfsburg <- dat_16[dat_16$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 311] <- sum(dat_16_Wolfsburg$Days)

dat_17_Wolfsburg <- dat_17[dat_17$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 311] <- sum(dat_17_Wolfsburg$Days)

dat_18_Wolfsburg <- dat_18[dat_18$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 311] <- sum(dat_18_Wolfsburg$Days)

dat_19_Wolfsburg <- dat_19[dat_19$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 311] <- sum(dat_19_Wolfsburg$Days)

dat_20_Wolfsburg <- dat_20[dat_20$`Team ID` == "311",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 311] <- sum(dat_20_Wolfsburg$Days)




dat_14_Hoffe <- dat_14[dat_14$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 316] <- sum(dat_14_Hoffe$Days)

dat_15_Hoffe <- dat_15[dat_15$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 316] <- sum(dat_15_Hoffe$Days)

dat_16_Hoffe <- dat_16[dat_16$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 316] <- sum(dat_16_Hoffe$Days)

dat_17_Hoffe <- dat_17[dat_17$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 316] <- sum(dat_17_Hoffe$Days)

dat_18_Hoffe <- dat_18[dat_18$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 316] <- sum(dat_18_Hoffe$Days)

dat_19_Hoffe <- dat_19[dat_19$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 316] <- sum(dat_19_Hoffe$Days)

dat_20_Hoffe <- dat_20[dat_20$`Team ID` == "316",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 316] <- sum(dat_20_Hoffe$Days)



dat_14_Gladbach <- dat_14[dat_14$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 308] <- sum(dat_14_Gladbach$Days)

dat_15_Gladbach <- dat_15[dat_15$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 308] <- sum(dat_15_Gladbach$Days)

dat_16_Gladbach <- dat_16[dat_16$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 308] <- sum(dat_16_Gladbach$Days)

dat_17_Gladbach <- dat_17[dat_17$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 308] <- sum(dat_17_Gladbach$Days)

dat_18_Gladbach <- dat_18[dat_18$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 308] <- sum(dat_18_Gladbach$Days)

dat_19_Gladbach <- dat_19[dat_19$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 308] <- sum(dat_19_Gladbach$Days)

dat_20_Gladbach <- dat_20[dat_20$`Team ID` == "308",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 308] <- sum(dat_20_Gladbach$Days)





dat_14_Mainz <- dat_14[dat_14$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 313] <- sum(dat_14_Mainz$Days)

dat_15_Mainz <- dat_15[dat_15$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 313] <- sum(dat_15_Mainz$Days)

dat_16_Mainz <- dat_16[dat_16$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 313] <- sum(dat_16_Mainz$Days)

dat_17_Mainz <- dat_17[dat_17$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 313] <- sum(dat_17_Mainz$Days)

dat_18_Mainz <- dat_18[dat_18$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 313] <- sum(dat_18_Mainz$Days)

dat_19_Mainz <- dat_19[dat_19$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 313] <- sum(dat_19_Mainz$Days)

dat_20_Mainz <- dat_20[dat_20$`Team ID` == "313",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 313] <- sum(dat_20_Mainz$Days)






dat_14_Augsburg <- dat_14[dat_14$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 315] <- sum(dat_14_Augsburg$Days)

dat_15_Augsburg <- dat_15[dat_15$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 315] <- sum(dat_15_Augsburg$Days)

dat_16_Augsburg <- dat_16[dat_16$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 315] <- sum(dat_16_Augsburg$Days)

dat_17_Augsburg <- dat_17[dat_17$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 315] <- sum(dat_17_Augsburg$Days)

dat_18_Augsburg <- dat_18[dat_18$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 315] <- sum(dat_18_Augsburg$Days)

dat_19_Augsburg <- dat_19[dat_19$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 315] <- sum(dat_19_Augsburg$Days)

dat_20_Augsburg <- dat_20[dat_20$`Team ID` == "315",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 315] <- sum(dat_20_Augsburg$Days)






dat_14_Frankfurt <- dat_14[dat_14$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 306] <- sum(dat_14_Frankfurt$Days)

dat_15_Frankfurt <- dat_15[dat_15$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 306] <- sum(dat_15_Frankfurt$Days)

dat_16_Frankfurt <- dat_16[dat_16$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 306] <- sum(dat_16_Frankfurt$Days)

dat_17_Frankfurt <- dat_17[dat_17$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 306] <- sum(dat_17_Frankfurt$Days)

dat_18_Frankfurt <- dat_18[dat_18$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 306] <- sum(dat_18_Frankfurt$Days)

dat_19_Frankfurt <- dat_19[dat_19$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 306] <- sum(dat_19_Frankfurt$Days)

dat_20_Frankfurt <- dat_20[dat_20$`Team ID` == "306",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 306] <- sum(dat_20_Frankfurt$Days)




dat_14_Hertha <- dat_14[dat_14$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1415 & end_table_main$`Team ID` == 317] <- sum(dat_14_Hertha$Days)

dat_15_Hertha <- dat_15[dat_15$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1516 & end_table_main$`Team ID` == 317] <- sum(dat_15_Hertha$Days)

dat_16_Hertha <- dat_16[dat_16$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1617 & end_table_main$`Team ID` == 317] <- sum(dat_16_Hertha$Days)

dat_17_Hertha <- dat_17[dat_17$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1718 & end_table_main$`Team ID` == 317] <- sum(dat_17_Hertha$Days)

dat_18_Hertha <- dat_18[dat_18$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1819 & end_table_main$`Team ID` == 317] <- sum(dat_18_Hertha$Days)

dat_19_Hertha <- dat_19[dat_19$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 1920 & end_table_main$`Team ID` == 317] <- sum(dat_19_Hertha$Days)

dat_20_Hertha <- dat_20[dat_20$`Team ID` == "317",]
end_table_main$SummedDays[end_table_main$Season == 2021 & end_table_main$`Team ID` == 317] <- sum(dat_20_Hertha$Days)


#write.csv(end_table_main, "PlatzierungplusVerletzungenproSaisonproVerein")
end_table_main$ShortName <- as.factor(end_table_main$ShortName)
end_table_main$Season <- as.factor(end_table_main$Season)
end_table_main$`Team ID` <- as.factor(end_table_main$`Team ID`)
table_ggplot <- subset(end_table_main, select = c("ShortName", "Season", "Points", "SummedDays")) 


library(ggplot2)
p <- ggplot(table_ggplot, aes(x=Points, y=log(SummedDays), color= ShortName)) + geom_point(shape=18)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred") + labs(y = "log (injury days)") + theme_classic()
p

