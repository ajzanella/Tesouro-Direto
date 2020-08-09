#Functions deep learning (Statistics Model)
library(caTools)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(stringr)

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
DataBase <- mutate(DataBase, ClassTitulos = substring(Titulos.e.Vencimento, 1,str_locate(pattern=" ", Titulos.e.Vencimento)[,1]-1))
DataBase <- mutate(DataBase, Vencimento = substring(Titulos.e.Vencimento, str_locate(pattern=" ", Titulos.e.Vencimento)[,1]+1,nchar(as.character(Titulos.e.Vencimento))))
#comverter to Date
