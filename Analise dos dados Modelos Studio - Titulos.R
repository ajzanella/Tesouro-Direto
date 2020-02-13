#Functions deep learning (Statistics Model)
library(caTools)


FileAddress<-"C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/"
setwd(FileAddress)

DataBase<-read.table(paste(getwd(),"CompletedDataBase.txt", sep="/"),header = TRUE, sep="\t", dec=".")
#convert factor to date
DataBase[,2]<-as.Date(DataBase[,2])

#filter if have any missing data
if(nrow(DataBase[is.na(DataBase),])>0){
  #set for numerical variable
  #identify class for all columns
  
  #separate per treasury
  
  
  i=2
  DataBase_Col_class<-matrix(0,nrow = ncol(DataBase)-1, ncol = 2)
  while(i<=ncol(DataBase)){
    DataBase_Col_class[i-1,1]<-i
    DataBase_Col_class[i-1,2]<-(class(DataBase[,i]))
    i=i+1
  }
  i=1
  while(i<=nrow(DataBase_Col_class)){
    if(DataBase_Col_class[i,2]=="numeric"){
      DataBase[,as.numeric(DataBase_Col_class[i,1])] = ifelse(is.na(DataBase[,as.numeric(DataBase_Col_class[i,1])]),
                         ave(DataBase[,as.numeric(DataBase_Col_class[i,1])], FUN = function(x) mean(x, na.rm = TRUE)),
                         DataBase[,as.numeric(DataBase_Col_class[i,1])])
    }
    ################################################################################################
    ############nao implementado, os dados ainda nao tem essa classificacao para tratarmos##########
    ################################################################################################
    #set for non numerical variable
    if(DataBase_Col_class[i,2]=="factor"){
    dataset$Country = factor(dataset$Country,
                             levels = c('France', 'Spain', 'Germany'),
                             labels = c(1, 2, 3))
    }
    if(DataBase_Col_class[i,2]=="Date"){
    #and date?????????
    }
    ################################################################################################
    ############nao implementado, os dados ainda nao tem essa classificacao para tratarmos##########
    ################################################################################################
    i=i+1
  }
}
i=1
#n_treasury<-length(names(summary(DataBase[,1])))
#Treasury_names<-names(summary(DataBase[,1]))

#while

subDataBase <- DataBase[which(DataBase[,1]==Treasury_names[i]), ]


set.seed(123)
split = sample.split(subDataBase[,5], SplitRatio = 0.8)
training_set = subset(subDataBase, split == TRUE)
test_set = subset(subDataBase, split == FALSE)