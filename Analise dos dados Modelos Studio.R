r.createRTerm

#Functions deep learning (Statistics Model)
library(caTools)
library(ggplot2)


FileAddress<-"C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/"
setwd(paste(FileAddress,"DataBase",sep="/"))

source(file=paste(FileAddress,"functions.R",sep="/"))

DataBase<-read.table(paste(getwd(),"Data5.txt", sep="/"),header = TRUE, sep="\t", dec=".")
#convert factor to date
#######DataBase[,2]<-as.Date(DataBase[,2])

#filter if have any missing data

  #set for numerical variable
  #identify class for all columns
  
  #separate per treasury
  
  
  i=1
  DataBase_Col_class<-matrix(0,nrow = ncol(DataBase), ncol = 2)
  while(i<=ncol(DataBase)){
    DataBase_Col_class[i,1]<-i
    DataBase_Col_class[i,2]<-(class(DataBase[,i]))
    i=i+1
  }
  listaBase<-list(0)
  i=1
  while(i<=nrow(DataBase_Col_class)){
    if(DataBase_Col_class[i,2]=="numeric" | DataBase_Col_class[i,2]=="integer"){
      #se estiver faltando dadonumerico, colocar a media. Senao deixar sem mesmo
      if(nrow(DataBase[is.na(DataBase),])>0){
        DataBase[,as.numeric(DataBase_Col_class[i,1])] = ifelse(is.na(DataBase[,as.numeric(DataBase_Col_class[i,1])]),
                         ave(DataBase[,as.numeric(DataBase_Col_class[i,1])], FUN = function(x) mean(x, na.rm = TRUE)),
                         DataBase[,as.numeric(DataBase_Col_class[i,1])])
      }
    }

    #set for non numerical variable
    if(DataBase_Col_class[i,2]=="factor"){
      #create vector with levels
     ##    levels(DataBase[,as.numeric(DataBase_Col_class[i,1])])
      j=1
      classLevels<-matrix(0,nrow = length(levels(DataBase[,as.numeric(DataBase_Col_class[i,1])])), ncol = 2)
      while(j<=nrow(classLevels)){
        classLevels[j,1]<-levels(DataBase[,as.numeric(DataBase_Col_class[i,1])])[j]
        classLevels[j,2]<-j-1
        j=j+1
      }
      DataBase[,as.numeric(DataBase_Col_class[i,1])]  = factor(DataBase[,as.numeric(DataBase_Col_class[i,1])] ,
                             levels = classLevels[,1],
                             labels = classLevels[,2])
      listaBase<-list(listaBase, classLevels)
    }
    
    ################################################################################################
    ############nao implementado, os dados ainda nao tem essa classificacao para tratarmos##########
    ################################################################################################
    
    if(DataBase_Col_class[i,2]=="Date"){
    #and date?????????
    }
    ################################################################################################
    ############nao implementado, os dados ainda nao tem essa classificacao para tratarmos##########
    ################################################################################################
    i=i+1
  }

i=1
#n_treasury<-length(names(summary(DataBase[,1])))
#Treasury_names<-names(summary(DataBase[,1]))

#while

#subDataBase <- DataBase[which(DataBase[,1]==Treasury_names[i]), ]

#linearmodel (best model)
SimpleLinearRegression(DataBase, 0.05)

SVR(DataBase)
