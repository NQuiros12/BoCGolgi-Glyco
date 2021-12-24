#Librerias
library(lubridate)
library(car)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)
library(mosaicData)
library(stringr)
library(htmlwidgets)
library(ggthemes)
library(pander)
library(xtable)
library(reshape2)
library(scales)
library(tibble)
library(abind)
library(devtools)
library(ggpubr)
library(data.table)
library(knitr)
library(broom)
###################################################
#Creamos funcion
cargadodedatos_area <- function(file_lst){
  AreaData<-data.frame(matrix(nrow=600,ncol=64))      #Creamos un dataframe generico vacio
  archivos = lapply(file_lst, read.csv)               #Leemos todos los csv de una lista de nombres de .csv's
  
  for (i in 1:length(archivos)){                      #Generamos un loop que una nuestros df uno por uno
    if(i!=1){ 
        SubjectDataM<-read.csv(file_lst[i],header=TRUE,sep=",")
        AreaData<-bind_cols(AreaData,SubjectDataM,.name_repair = "minimal")  #El minimal indica que no haya reparacion de nombres
    }
    else if(i==1){
        AreaData<-read.csv(file_lst[i],header=TRUE,sep=",")
    }
  }
  nombres<-names(AreaData) #Definimos los nombres de las comulnas y los hacemos unicos
  nombresunicos<-make.unique(nombres,sep = "") #Almacenamos los nombres de las columnas en un vector
  
  colnames(AreaData)<-nombresunicos #Nombres de columnas cambiados
  
  return(AreaData)
}
###############################################
cargadodedatos_cargos<-function(file_lst){
  CargoData<-data.frame(matrix(nrow = 600,ncol = 64))
  archivos = lapply(file_lst, read.csv)
  
  for (i in 1:length(archivos)){
    if(i!=1){ 
      SubjectDataM<-read.csv(file_lst[i],header=TRUE,sep=",")
      CargoData<-bind_cols(CargoData,SubjectDataM,.name_repair = "minimal")#El minimal indica que no haya reparacion de nombres
    }
    else if (i==1){
      CargoData<-read.csv(file_lst[i],header=TRUE,sep=",")
    }
  }
  
  #Definimos los nombres y los hacemos unicos
  nombres<-names(CargoData)
  
  nombresunicos<-make.unique(nombres,sep = "")
  colnames(CargoData)<-nombresunicos
  
  return(CargoData)
}

###############################################################################

#Graficar
graficarCargos<-function(dataCargos,lista_col_agrup){
  #loop sobre las columnas agrupadas
  for (i in 1:length(lista_col_agrup)){ 
    #Switch Case para cada tipo de cisterna los datos son almacenados en distintos vectores
    if (i==1){
      PostGolgi<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
      
    }
    else if (i==2){
      C1<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
    }
    else if (i==3){
      C2<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
    }
    else if (i==4){
      C3<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
    }
    else if (i==5){
      C4<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
    }
    else if (i==6){
      C5<-rowMeans(dataCargos[,c(lista_col_agrup[[i]])])
    }
  }
  #Agrupamos todos los vectores en un solo dataframe
  dataframepromedio<-data.frame(C1,C2,C3,C4,C5,PostGolgi)
  #Determinamos el largo de este data frame para generar un vector con los ticks
  #El registro se hace cada 100 ticks
  n <- length(dataframepromedio$C1)*100
  ticks <- seq(100,n,by=100)#Generamos el vector
  dataframepromedio$ticks <- ticks #Lo introducimos al dataframe 
  d1 <- melt(dataframepromedio,id.vars="ticks")#Finalmente meltiamos los datos y solo conservamos los ticks
  #Esto va a devolver el grafico final
  graf <- ggplot(d1,aes(x=ticks,y=value,color=variable))
  graf+geom_point() #Tipo de grafico de puntos
  graf+geom_line() #Unido por lineas
  graf+labs(x="",y="",title ="") #Vaciamos los titulos de los ejes 
  graf+scale_color_manual(values = c("C1" = "blue",
                                     "C2" = "skyblue2",
                                     "C3"="green4",
                                     "C4"="red",
                                     "C5"="yellow",
                                     "PostGolgi"="grey1")) #Asignamos colores
  graf+theme_classic() #Definimos un theme
  return(graf)
  
}
testi<- function(){
  print("HOLAAAAAA ESTOY FUNCIONANDO!");
  
}
