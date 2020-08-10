#Functions deep learning (Statistics Model)
library(caTools)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(stringr)
library(lubridate)

options(digits = 10) 
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
DataBase <- mutate(DataBase, Vencimento = dmy(substring(Titulos.e.Vencimento, str_locate(pattern=" ", Titulos.e.Vencimento)[,1]+1,nchar(as.character(Titulos.e.Vencimento)))))

#function for LFT
LFT_Interest<- function(DataBase, DataVencimento){
DataVencimento = ymd(as.character("2021-03-01"))
DataBase<-DataBase %>% filter (ClassTitulos=="LFT", Vencimento == DataVencimento) %>% mutate(PU.Venda.Ant = lag(PU.Venda,1), TaxaAnual=((PU.Venda/PU.Venda.Ant)^252-1)*100) %>% select (ClassTitulos, Vencimento, Data, PU.Venda, TaxaAnual) %>% filter(!is.na(TaxaAnual)) %>% distinct(Data, .keep_all = TRUE)
#head(DataBase)

#DataBase %>%
#  ggplot(aes(x = Data, y = TaxaAnual)) +
#  geom_line() + 
#  geom_smooth(method = "loess")

}