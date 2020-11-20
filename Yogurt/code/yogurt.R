#clean data
rm(list = ls())

#set up the envrionment
library(ggplot2)
library(gtools)
library(wordcloud)
library(tm)
library(reshape2)
library(foreign)

filnm <- "Wegmans Survey"

#read survey
survey <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)

#peek data
names(survey)

#extract 39 questions
survey_data <- survey[162:183]

