setwd("/Users/Dan/Dropbox/BC_COAST_HERRING/")
test <- read.csv("MT_Area06_07_08_biomass.csv")
head(test)
library(ggplot2);library(gdata)

test <- subset(test,SeasonCode>19889)
test <- drop.levels(test)

test$LS <- with(test,factor(Loc_Code):factor(Section))

test2 <- ddply(test, .(Section, SeasonCode), summarize, SUM= sum(TotalFishTonnes))

test3 <- dcast(test2, SeasonCode~Section)

ggplot(aes(SeasonCode,SUM),data= test2)+
  geom_path()+
  facet_wrap(~Section,scales= "free_y")+theme_bw()
