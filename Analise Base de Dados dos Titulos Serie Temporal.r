#Functions deep learning (Statistics Model)
library(caTools)
library(tidyverse)
library(dslabs)
library(ggplot2)

options(digits = 6) 
options(scipen=999)

FileAddress<-"C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/"
setwd(FileAddress)

DataBase<-read.table(paste(getwd(),"CompletedDataBase.txt", sep="/"),header = TRUE, sep="\t", dec=",")
#convert factor to date
DataBase[,2]<-as.Date(DataBase[,2])

head(DataBase)

TitulosNames<-unique(DataBase[,1])
length(TitulosNames)