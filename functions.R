library(caTools)
library(ggplot2)
library(jtools)
library(sandwich)
library(AutoModel)
#library(tidyverse)
#library(caret)
#library(leaps)
library(MASS)
library(e1071)
library(YieldCurve)

#summary(reg)$r.squared
#R Squared adj
#summary(reg)$adj.r.squared
#p-value
#summary(reg)$coefficients[,4]


SimpleLinearRegression<- function(DataBase, pvalue){
  correctnames<-matrix(0,nrow = ncol(DataBase), ncol = 2)
  i=1
  while(i<=ncol(DataBase)){
    correctnames[i,2]<-names(DataBase[i])
    names(DataBase)[i] <- paste("Var",i,sep="")
    correctnames[i,1]<-paste("Var",i,sep="")
    i=i+1
  }
  

  i=2
  j=1
  col<-ncol(DataBase)
  while(i<=col){
  #numeric variable ^2, ^3, ^4
    if(class(DataBase[,i])=="numeric" | class(DataBase[,i])=="integer"){
      DataBase<-cbind(DataBase,DataBase[,i]^2)
      names(DataBase)[col+i-2+j]<-paste(paste("Var",i,sep=""),"^2",sep="")
      j=j+1
      DataBase<-cbind(DataBase,DataBase[,i]^3)
      names(DataBase)[col+i-2+j]<-paste(paste("Var",i,sep=""),"^3",sep="")
      j=j+1
      DataBase<-cbind(DataBase,DataBase[,i]^4)
      names(DataBase)[col+i-2+j]<-paste(paste("Var",i,sep=""),"^4",sep="")
    } else {
      j=j-1
    }
    i=i+1
  }
  
  ########
  #retirar a variavel com fatores maiores que 5 classes
  i=2
  while(i<=col){
    if(class(DataBase[,i])=="factor" & length(levels(DataBase[,i]))>=5){
      DataBase<-DataBase[,-i]
    }
    i=i+1
  }
 
  set.seed(123)
  split = sample.split(DataBase[,1], SplitRatio = 0.8)
  training_set = subset(DataBase, split == TRUE)
  test_set = subset(DataBase, split == FALSE)
 
  training_set_test<-training_set
  test_set_test<-test_set
  max_p_value<-0
  ini<-0
  while(max_p_value >= pvalue | ini==0){
    ini=1
    
     # Fitting Simple Linear Regression to the Training set
    reg = lm(formula = Var1 ~ .,
                   data = training_set_test)
    
    j=1
    max_p_value<-summary(reg)$coefficients[j+1,4]
    max_j<-j+1
    while(j<=length(summary(reg)$coefficients[,4])-2){
      if(summary(reg)$coefficients[j+2,4]>max_p_value){
        max_p_value<-summary(reg)$coefficients[j+2,4]
        max_j<-j+2
      }
    j=j+1  
    }
    if(max_p_value>pvalue){
      training_set_test<-training_set_test[,-max_j]
      test_set_test<-test_set_test[,-max_j]
    }
  
  }

  training_set<-training_set_test
  test_set<-test_set_test
  regressor = lm(formula = Var1 ~ .,
                 data = training_set)
  
  # Predicting the Test set results
  y_pred = predict(regressor, newdata = test_set)
 
  listaretorno<-list(summ(regressor, digits = 9), training_set, test_set, y_pred, correctnames, summary(regressor)$adj.r.squared)
 
   names(listaretorno)[1]<-"Summary of Regressor"
   names(listaretorno)[2]<-"Training Set DataBase"
   names(listaretorno)[3]<-"Test Set DataBase"
   names(listaretorno)[4]<-"Y_pred"
   names(listaretorno)[5]<-"Correctnames"
   names(listaretorno)[6]<-"Adj.r.squared"
  
  
  return(listaretorno)
}

SVR<- function(DataBase){

  training_set<-DataBase[,-2]
  #set.seed(123)
  #split = sample.split(DataBase[,1], SplitRatio = 0.8)
  #training_set = subset(DataBase, split == TRUE)
  #test_set = subset(DataBase, split == FALSE)  
  
  regressor = svm(formula = Salary ~ .,
                  data = training_set,
                  type = 'eps-regression',
                  kernel = 'radial')
  
  # Predicting a new result
  y_pred = predict(regressor, data.frame(Level = 6.5))
  
  # Visualising the SVR results
  # install.packages('ggplot2')
  
  ggplot() +
    geom_point(aes(x = training_set$Level, y = training_set$Salary),
               colour = 'red') +
    geom_line(aes(x = training_set$Level, y = predict(regressor, newdata = training_set)),
              colour = 'blue') +
    ggtitle('Truth or Bluff (SVR)') +
    xlab('Level') +
    ylab('Salary')
  
  # Visualising the SVR results (for higher resolution and smoother curve)
  # install.packages('ggplot2')
  library(ggplot2)
  x_grid = seq(min(training_set$Level), max(training_set$Level), 0.1)
  ggplot() +
    geom_point(aes(x = training_set$Level, y = training_set$Salary),
               colour = 'red') +
    geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
              colour = 'blue') +
    ggtitle('Truth or Bluff (SVR)') +
    xlab('Level') +
    ylab('Salary')

}

YieldCurve<-function(DataBase){
  
  
  #separar por tipo de titulo e calcular a taxa dos que nao tem.
  
Datamenor<-DataBase[-1,2]
PUVendaPost<-DataBase[-1,6]
PUVendaAnt<-DataBase[-length(DataBase[,6]),6]



BaseYield<-cbind(Datamenor, PUVendaPost/PUVendaAnt)
  
  head(BaseYield)
  
  
}
