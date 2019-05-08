#Abrir o site Tesouro direto e retirar informações dos valores dos titulos. 
## Pacotes para execução das funções
#install.packages("readxl")

library(Hmisc)
library(date)
library(zoo)
#memory.limit(size = 4000)
library(sqldf)
library(stringr)
library(XML)
library(kulife)
library(xml2)
library(foreign)
library(rvest)
library(readxl)
library(janitor)

FileAddress<-"C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase"

setwd(paste(FileAddress, "Historic", sep="/"))

###Functions###





vencimentos<- function(diretorio){
  DataNames = matrix()
  i=1
  while (i <= length(list.files(diretorio,full.names = FALSE, recursive = FALSE))){
    j=1
    while (j<=length(list.files(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE))){
      
      lim = 1
      while (lim <= length(excel_sheets(paste(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),list.files(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE)[j], sep="/")))){		
        if (nchar(names(read_xls(paste(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),list.files(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE)[j], sep="/"),lim, range = cell_cols("A:E")))[2])>=6){
          DataNames = rbind(DataNames,paste(list.files(diretorio,full.names = FALSE, recursive = FALSE)[i],names(read_xls(paste(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),list.files(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE)[j], sep="/"),lim, range = cell_cols("A:E")))[2],sep=" "))
        } else{
          DataNames = rbind(DataNames, paste(list.files(diretorio,full.names = FALSE, recursive = FALSE)[i],format(excel_numeric_to_date(as.numeric(names(read_xls(paste(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),list.files(paste(diretorio,list.files(diretorio,full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE)[j], sep="/"),lim, range = cell_cols("A:E")))[2])),"%d/%m/%Y"),sep=" "))
        }
        lim = lim + 1
      }
      j=j+1
    }
    i=i+1
  }
  DataNames<-as.data.frame(DataNames)
  names(DataNames)<-"Titulos Vencimentos"
  DataNames<-na.omit(DataNames)
  DataNames<-DataNames[!duplicated(DataNames),]
  return(DataNames)
}


AlterarData<-function(BaseRecebida){
  BaseRecebida<-as.data.frame(BaseRecebida)
  colnames(BaseRecebida)<-c("V1","V2","V3","V4","V5")
  BaseRecebida_Num<-as.data.frame(matrix(data = NA, nrow = 1, ncol = ncol(BaseRecebida)))
  BaseRecebida_Date<-as.data.frame(matrix(data = NA, nrow = 1, ncol = ncol(BaseRecebida)))
  i=1
  while(i<=nrow(BaseRecebida)){
      if(is.na(str_locate(BaseRecebida[i,1],"/")[1])){
          BaseRecebida_Num<-rbind(BaseRecebida_Num,BaseRecebida[i,])
      } else {
          BaseRecebida_Date<-rbind(BaseRecebida_Date,BaseRecebida[i,])
      }
    i=i+1
  }
  BaseRecebida_Date<-na.omit(BaseRecebida_Date)
  BaseRecebida_Num<-na.omit(BaseRecebida_Num)
  BaseRecebida_Num[,1]<-format(excel_numeric_to_date(as.numeric(BaseRecebida_Num[,1])),"%d/%m/%Y")
  BaseRecebida_Date[,1]<-format(as.Date(paste(substring(BaseRecebida_Date[,1],1,2), substring(BaseRecebida_Date[,1],4,5), substring(BaseRecebida_Date[,1],7,10),sep="/"), "%d/%m/%Y"),"%d/%m/%Y")
  BaseRecebida1<-rbind(BaseRecebida_Num,BaseRecebida_Date)
  BaseRecebida1[,1]<-as.factor(format(as.Date(paste(substring(BaseRecebida1[,1],1,2), substring(BaseRecebida1[,1],4,5), substring(BaseRecebida1[,1],7,10),sep="/"), "%d/%m/%Y"),"%d/%m/%Y"))
  #BaseRecebida1[,1]<-order(BaseRecebida1[,1], decreasing = FALSE) #nao esta funcionando a ordenacao
  rm(BaseRecebida)
  rm(BaseRecebida_Date)
  rm(BaseRecebida_Num)
  return(as.matrix(BaseRecebida1)) 
}

###EndFunctions###

diretorio <- getwd()
Cabecalho = vencimentos(diretorio)
n_pasta = 1
n_cabecalho = 1 #nao inicia no while pq devo preencher todo array sem reiniciar, mas vou limitar as iteracoes filtrando o nome
Titulos<-cbind(as.matrix(paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)," ",sep="")), matrix(0,nrow=length(as.matrix(paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)," ",sep="")))))
#Titulos<-cbind(as.matrix(paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)," ",sep="")), matrix(0,nrow=length(as.matrix(paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)," ",sep="")))))
i=1
a=0
while(i<=nrow(Titulos)){
  a<-a+length(str_subset(Cabecalho,Titulos[i]))
  Titulos[i,2]<-a
  i=i+1
}
while (n_pasta <= length(list.files(getwd(),full.names = FALSE, recursive = FALSE))){
  while (n_cabecalho <= as.numeric(Titulos[n_pasta,2])){  #acho que arrumei o while para andar so o necessario da array do cabecalho
    DataBaseBonds = matrix(ncol=5)#com mesmo numero de colunas
    n_excel = 1
    while (n_excel<=length(list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE))){
      n_abas = 1
      while (n_abas <= length(excel_sheets(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/")))){
        if(is.na(str_locate(names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/"),n_abas, range = cell_cols("A:E")))[2], "/")[,1])){
        vencimento_aba<-format(excel_numeric_to_date(as.numeric(names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/"),n_abas, range = cell_cols("A:E")))[2])),"%d/%m/%Y")  
        } else{
          vencimento_aba<-names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/"),n_abas, range = cell_cols("A:E")))[2]
        }
        if(Cabecalho[n_cabecalho]== paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta],vencimento_aba,sep=" ")){
          CurrentData<-as.matrix(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/"),n_abas, range = cell_cols("A:E")))[-1,]
          CurrentData<-na.omit(CurrentData)
          CurrentCabecalho = as.matrix(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_excel], sep="/"),n_abas, range = cell_cols("A:E")))[1,]
          NamesCabecalho<-CurrentCabecalho[-1]
          CurrentCabecalho[-1] <- substring(NamesCabecalho,1,str_locate(NamesCabecalho, " ")[,1]+str_locate(substring(NamesCabecalho, first = str_locate(NamesCabecalho, " ")[,1]+1), " ")[,1])
          CurrentData<-AlterarData(CurrentData) #criei a funcao para corrigir o erro
          CurrentData[,2:5]<-round(as.numeric(CurrentData[,2:5]), digits=8)
          CurrentData<-as.data.frame(CurrentData)
          colnames(CurrentData)<-CurrentCabecalho
          CurrentData<-as.matrix(CurrentData)
          DataBaseBonds = rbind(DataBaseBonds,CurrentData)
          FileName<-gsub("/","",vencimento_aba)
        }
        n_abas = n_abas + 1
      }
      n_excel=n_excel+1
    }
    #salvar o arquivo
    write.table(na.omit(DataBaseBonds), file=paste(paste(FileAddress,paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta],FileName,sep=" "),sep="/"),".txt", sep=""), sep="\t", dec=",",row.names = FALSE, col.names = TRUE, quote = FALSE)
    n_cabecalho = n_cabecalho + 1
    rm(DataBaseBonds)
    rm(CurrentCabecalho)
    rm(CurrentData)
  }
  n_pasta = n_pasta + 1
}




write.table(count_Geral,file="C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Teste.txt",dec=",",row.names = FALSE, col.names = TRUE, quote = FALSE)




###Nao estou entendendo pq esta pulando os cabecalhos e nao esta olhando todas as camadas####






#Datadentro do arquivo de excel
#format(excel_numeric_to_date(as.numeric(names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E")))[2])),"%d/%m/%Y")
#read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E"))[1,]
#nrow(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E")))

#a<-read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E"))


#paste(list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta],format(excel_numeric_to_date(as.numeric(names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E")))[2])),"%d/%m/%Y"),sep=" ")


#read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[n_pasta], sep="/"),full.names = FALSE, recursive = FALSE)[n_cabecalho], sep="/"),n_excel, range = cell_cols("A:E"))

#names(read_xls(paste(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[i], sep="/"),list.files(paste(getwd(),list.files(getwd(),full.names = FALSE, recursive = FALSE)[i], sep="/"),full.names = FALSE, recursive = FALSE)[j], sep="/"),lim, range = cell_cols("A:E")))[2]
