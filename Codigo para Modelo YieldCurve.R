#Functions deep learning (Statistics Model)
library(caTools)
library(ggplot2)


FileAddress<-"C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/"
setwd(paste(FileAddress,"DataBase",sep="/"))

source(file=paste(FileAddress,"functions.R",sep="/"))

DataBase<-read.table(paste(getwd(),"Data5.txt", sep="/"),header = TRUE, sep="\t", dec=".")