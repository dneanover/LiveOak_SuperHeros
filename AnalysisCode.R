########Live Oak and Super Heros##############----
library(tidyverse)
library(readr)
set.seed(2626)
setwd("C:\\Users\\damon\\Documents\\GitHub\\LiveOak_SuperHeros\\")

#importing data----
games <- read_csv("Video Games.csv", col_names = TRUE)
str(games)
table(games$Property)

#Data Cleaning ----
#Data cleaning that was missed in Excel while still a character value----
#removing those items that give R a hard time
games$Property <- gsub(":","-", games$Property)
games$Property <- gsub("'","", games$Property)

#Looking for duplciates or things written poorly----
table(games$Property)
games$Property[games$Property == "Iron Mancrossover"] <- "Iron Man (Cross-over)"
games$Property[games$Property == "suicide squad"] <- "Suicide Squad"

#Better
table(games$Property)

#Data cleaning with Recode as a level----
library(car)
games$Property_factor <- as.factor(games$Property)

levels(games$Property_factor)
games$Property_factor <- Recode(games$Property_factor,"c('Men in Black', 'Men In Black','The Men in Black') = 'Men In Black'", as.factor.result=TRUE)
levels(games$Property_factor)

#Counter per hero----
counts <- as.data.frame(table(games$Property_factor))
counts_arranged <- arrange(counts, desc(Freq))

#Subset needed to join Counts
marvel <- games[games$`Marvel/DC` == "Marvel",]
counts$DCorMar <- if_else(counts$Var1 %in% marvel$Property_factor, 
                          if_else(counts$Var1 == "(Cross-over)", "(Cross-over)",'Marvel'), 'DC') 
counts$DCorMar

#Counts Visual -----
library(ggthemes)

ggplot(counts, aes(x=reorder(Var1, - Freq), y=Freq, fill= counts$DCorMar))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_bw()+
  labs(title = "Super Hero Video Game Counts", x= "Property", y="Number of Video Games")+
  geom_hline(yintercept=6, col="yellow", size=1.5)+
  geom_hline(yintercept=1, col="red", size = 1.5)+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45))+
  scale_fill_colorblind(name = "Marvel or DC")+
  geom_text(x= 35, y = 20,color = "red", label = "Lowest Number (1)", size = 5)+
  geom_text(x= 32, y = 20,color = "yellow2", label = "Average (6)", size = 5)

#over time plot----
ggplot(games, aes(x=Year, y =games$Metacritic_average ,color=games$`Marvel/DC`))+
  geom_point()+
  scale_x_continuous(limits = c(1987,2017))+
  labs(title = "Critics Ratings Over Time", x="Year", y= "Metacritic Average Rating (out of 100)")+
  theme(plot.title = element_text(hjust = .5, size = 15))+
  geom_smooth()+
  scale_color_manual(name = "Marvel or DC", values = c("orange2", "steelblue2"))+
  geom_hline(yintercept=63.77, color="black", size = .5)
  
#Splitting out based on platform
mobile_Games <- games[games$Mobile == "Y",]
acracde_Games <- games[games$Arcade == "Y",]
standard_Games <- games[games$Arcade == "N" & games$Mobile == "N",]

summary(standard_Games)
#Average Rating for a game is 63.77
#40 Sequels
#87 Gaming Systems

#Some games are missing ratings
rated_Games <- standard_Games[!is.na(standard_Games$Metacritic_average),]
mean(rated_Games$Metacritic_average)
min(rated_Games$Metacritic_average)
max(rated_Games$Metacritic_average)

marvel_rated <- rated_Games[rated_Games$`Marvel/DC` == "Marvel",]
dc_rated <- rated_Games[rated_Games$`Marvel/DC` == 'DC',]
#marvel
mean(marvel_rated$Metacritic_average)
min(marvel_rated$Metacritic_average)
max(marvel_rated$Metacritic_average)
sum(marvel_rated$Metacritic_average > 63.77)

#DC
mean(dc_rated$Metacritic_average)
min(dc_rated$Metacritic_average)
max(dc_rated$Metacritic_average)
sum(dc_rated$Metacritic_average > 63.77)
quantile(dc_rated$Metacritic_average, probs = .25)
quantile(marvel_rated$Metacritic_average, probs=.25)

summary(dc_rated$Metacritic_average)
summary(marvel_rated$Metacritic_average)

#Looking at Characteristics of All Heros!
library(sqldf)

rated_Games$Marvel <- rated_Games$'Marvel/DC'


superhero_stats <- sqldf("select avg(Metacritic_average) 'Mean.Metacritic', 
                         min(Metacritic_Average) 'Min.Metacritic',
                         max(Metacritic_Average) 'Max.Metacritic',
                         Property_Factor , Marvel 
                        from rated_Games
                        group by Property_Factor")

superhero_stats

ggplot(superhero_stats, aes(x= Property_factor, y=Max.Metacritic, fill=Marvel))+
  geom_bar(stat="identity")+

#A Look at all of them together, too busy
ggplot(superhero_stats, aes(x= Property_factor, color=Marvel))+
  geom_bar(aes(y=Max.Metacritic), stat="identity", fill="black", size=3)+
  geom_bar(aes(y=Mean.Metacritic), stat="identity", fill="yellow", size=3)+
  geom_bar(aes(y=Min.Metacritic), stat="identity", fill="red", size=3)+
  coord_flip()+
  scale_color_manual(name = "Marvel or DC", values = c("orange2", "steelblue2"))
  

superhero_marvel <- superhero_stats[superhero_stats$Marvel == "Marvel",]
superhero_dc <- superhero_stats[superhero_stats$Marvel == "DC",]

counts_dc <- counts_dc[counts_dc$Freq> 0,]
counts_marvel <- counts_marvel[counts_marvel$Freq >0 ,]

max(counts_dc$Freq)
mean(counts_dc$Freq)
min(counts_dc$Freq)
median(counts_dc$Freq)

max(counts_marvel$Freq)
mean(counts_marvel$Freq)
min(counts_marvel$Freq)
median(counts_marvel$Freq)

ggplot(counts_marvel, aes(reorder(Var1, - Freq), y = Freq))+
  geom_bar(stat="identity", fill="steelblue2")+
  coord_flip()+
  labs(title = "Marvel Hero Game Count", x="Franchise Property", y="Number of Games")

ggplot(superhero_marvel, aes(x= Property_factor))+
  geom_bar(aes(y=Max.Metacritic), stat="identity", fill="black", size=3)+
  geom_bar(aes(y=Mean.Metacritic), stat="identity", fill="yellow", size=3)+
  geom_bar(aes(y=Min.Metacritic), stat="identity", fill="steelblue2", size=3)+
  coord_flip()+
  labs(title = "Marvel Hero Ratings", x = "Property", y = "Metacritic Ratings")

  
ggplot(counts_dc, aes(reorder(Var1, - Freq), y = Freq))+
  geom_bar(stat="identity", fill="orange")+
  coord_flip()+
  labs(title = "DC Hero Game Count", x="Franchise Property", y="Number of Games")

ggplot(superhero_dc, aes(x= Property_factor))+
  geom_bar(aes(y=Max.Metacritic), stat="identity", fill="black", size=3)+
  geom_bar(aes(y=Mean.Metacritic), stat="identity", fill="yellow", size=3)+
  geom_bar(aes(y=Min.Metacritic), stat="identity", fill="orange2", size=3)+
  coord_flip()+
  labs(title = "DC Hero Ratings", x = "Property", y = "Metacritic Ratings")


#Sequels

sequels <- rated_Games[rated_Games$Sequel == "TRUE",]
sequel_Counts <- as.data.frame(table(sequels$Property_factor))
sequel_Counts <- sequel_Counts[sequel_Counts$Freq > 0, ]
sequel_Counts$Marvel <- if_else(sequel_Counts$Var1 %in% marvel$Property_factor, 
                          if_else(sequel_Counts$Var1 == "(Cross-over)", "(Cross-over)",'Marvel'), 'DC')
sequel_Counts$Var

ggplot(sequel_Counts, aes(x= reorder(Var1, -Freq), y= Freq, fill=Marvel))+
    geom_bar(stat="identity")+
    labs(title = "Sequel Count by Affiliation", x = "Franchise", y = "Count")+
   theme(plot.title = element_text(hjust = 0.5, size = 15))+
    scale_fill_colorblind()


sequels$Marvel <- sequels$`Marvel/DC`

sequel_stats <- sqldf("select avg(Metacritic_average) 'Mean.Metacritic', 
                         min(Metacritic_Average) 'Min.Metacritic',
                         max(Metacritic_Average) 'Max.Metacritic',
                         Property_Factor , Marvel 
                         from sequels
                         group by Property_Factor")
sequel_stats <- sequel_stats[2:7,]
sequel_stats

ggplot(sequel_stats, aes(x= Property_factor))+
  geom_bar(aes(y=Max.Metacritic), stat="identity", fill="black", size=3)+
  geom_bar(aes(y=Mean.Metacritic), stat="identity", fill="yellow", size=3)+
  geom_bar(aes(y=Min.Metacritic), stat="identity", fill="orange2", size=3)+
  coord_flip()+
  labs(title = "DC Hero Ratings", x = "Property", y = "Metacritic Ratings")