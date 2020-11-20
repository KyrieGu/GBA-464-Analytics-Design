library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(grf)
library(stargazer)
#load the data and view basic summary statistics.
setwd("~/Desktop/MKT436/hw/2")
adsData = read.csv('HW2 Data.csv')
summary(adsData)
names(adsData)
#mkt hw2 2c model
twoInteraction = lm(clickPerDollar~factor(category)*factor(placement)+factor(category)*ageMean+factor(category)+ageMean+factor(placement)+factor(adType)+factor(body),data=adsData)
summary(twoInteraction)


# goal: Response/Cost * Value/click > (1 + %Return on Action)
margin = 0.011
response = predict(twoInteraction)
threshold = 1.1

# add new col
adsData$Score = margin*response
adsData$Extend = (adsData$Score > threshold)
names(adsData)

# only look at the targeted group
ads = adsData[adsData$Extend == 'TRUE',]


lmInteraction = lm(clickPerDollar~factor(category)*factor(placement)+factor(category)*ageMean+factor(category)+ageMean+factor(placement)+factor(adType)+factor(body),data=adsData)
summary(lmInteraction)
stargazer(lmInteraction,type="text")

lmCellMeans = lm(clickPerDollar~0+factor(placement) + ageMean + factor(category):factor(placement) + factor(category)+factor(adType)+factor(body)+ category:ageMean,data=adadsDatas)
stargazer(lmInteraction,lmCellMeans,type="text")

# GRAPH
dt = data.table(ads)
dagg = dt[,.(clickPerDollar = mean(clickPerDollar), 
             seClickPerDollar = sd(clickPerDollar)/sqrt(.N),.N),
          by = .(category,placement)]


dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=factor(category),
           y=clickPerDollar,
           x=factor(placement),
           ymax=clickPerDollar+seClickPerDollar,
           ymin=clickPerDollar-seClickPerDollar),
       data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="placement",y="clickPerDollar")


# use category as x-axis

dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=factor(placement),
           y=clickPerDollar,
           x=factor(category),
           ymax=clickPerDollar+seClickPerDollar,
           ymin=clickPerDollar-seClickPerDollar),
       data=dagg)+
  geom_bar(position=dodge,stat="identity") + 
  geom_errorbar(position=dodge)
labs(x="category",y="clickPerDollar")
require(mgcv)

df=ads[2:8]
row = unique(df)
