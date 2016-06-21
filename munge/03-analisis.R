#---
#title: Análisis de datos uno 
#author: JCAA
#date: 29/04/2016
#abstract: se procesan tablas minables para generar nuevos indicadores y realizar predicciones.
#
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---
library(neuralnet)
library(NeuralNetTools)
#--
 minable1 <- fread("./data/minable1.csv")

 minable2 <- fread("./data/minable2.csv")

# costos por diabetes 

## costos por enfermos
cost <- 32695 #costo anual por enfermo
minable2[,costo.trat:= cost*enf]

##costos por días perdidos
sd <- 257.9 #salario diario promedio
minable2[,costo.morbi:=dias.perdidos*sd]


##costos por muertes
sal <- 61896#salario medio anual
i <- 0.04 # como indica banco mundial
g <- 0.03 # inflacion según banco de méxico


minable2[,ocup:=]
minable2[,ene:= ]
minable2[,vp:=sal/(i-g)*(1-(1+g)/(1+i)^ene)] # valor presente
minable2[,costo.morta :=defunciones*ocup*vp] # costo por defuncion

### forecast

minable4 <- minable1[,.(edo,mpo,loc,N.deporte,N.restaurant,alfa,der,pobtot)]

train1 <- dcast(minable2,edo+mpo+loc ~ year, value.var=("enf"),fun.aggregate=function(x){sum(x,na.rm=T)})

train2 <- dcast(minable2,edo+mpo+loc ~ year, value.var=("dias.perdidos"),fun.aggregate=function(x){sum(x,na.rm=T)})

train3 <- dcast(minable2,edo+mpo+loc ~ year, value.var=("defunciones"),fun.aggregate=function(x){sum(x,na.rm=T)})

names(train1)<-c("edo","mpo","loc","enf.2010","enf.2011","enf.2012","enf.2013","enf.2014")
names(train2)<-c("edo","mpo","loc","dia.2010","dia.2011","dia.2012","dia.2013","dia.2014")
names(train3)<-c("edo","mpo","loc","def.2010","def.2011","def.2012","def.2013","def.2014")

keys <- c("edo","mpo","loc")

setkeyv(train1,keys)
setkeyv(train2,keys)
setkeyv(train3,keys)
setkeyv(minable4, keys)

train <- train1[train2[train3[minable4]]]

train <- train[,.(edo,mpo,loc,
              enf.2010,enf.2011,enf.2012,enf.2013,
              dia.2010,dia.2011,dia.2012,dia.2013,
              def.2010,def.2011,def.2012,def.2013,
              N.deporte,N.restaurant,alfa,der,pobtot,
              enf.2014,dia.2014,def.2014)]


form <- paste("enf.2014+dia.2014+def.2014~",
              names(train)[which(!names(train)%in%c("enf.2014","def.2014","dia.2014","edo","loc","mpo"))]%>% 
                paste(collapse = "+")) %>%as.formula()

sdcols<-names(train)[which(!names(train)%in%c("edo","mpo","loc"))]

trainX <- train[,lapply(.SD,FUN=function(x){x[is.na(x)]<-0; x})] #quitamos NA
means <- trainX[,lapply(.SD,mean),.SDcols=sdcols] 
sdes <- trainX[,lapply(.SD,sd),.SDcols=sdcols] 
trainY <- trainX[,c(sdcols):=lapply(.SD,scale),.SDcols=sdcols] #escalamos
 

#entrenamos red
red <- neuralnet(formula= form, data= trainY,hidden = c(10),linear.output = FALSE)

plotnet(red)

#predecimos 2015
test <- trainY[,.(
                enf.2011,enf.2012,enf.2013,enf.2014,
                dia.2011,dia.2012,dia.2013,dia.2014,
                def.2011,def.2012,def.2013,def.2014,
                N.deporte,N.restaurant,alfa,der,pobtot)]

com <- compute(red, test)

trainY[,enf.2015:=com$net[,1]][,dia.2015:=com$net[,2]][,def.2015:=com$net[,3]]

#predecimos 2016

test <- trainY[,.(
  enf.2012,enf.2013,enf.2014,enf.2015,
  dia.2012,dia.2013,dia.2014,dia.2015,
  def.2012,def.2013,def.2014,def.2015,
  N.deporte,N.restaurant,alfa,der,pobtot)]

com <- compute(red, test)

trainY[,enf.2016:=com$net[,1]][,dia.2016:=com$net[,2]][,def.2016:=com$net[,3]]

# predecimos 2017

test <- trainY[,.(
  enf.2013,enf.2014,enf.2015,enf.2016,
  dia.2013,dia.2014,dia.2015,dia.2016,
  def.2013,def.2014,def.2015,def.2016,
  N.deporte,N.restaurant,alfa,der,pobtot)]

com <- compute(red, test)

#agregamos los forecast a tabla principal y reescalamos

predcols <- c("enf.2015","dia.2015","def.2015",
              "enf.2016","dia.2016","def.2016",
              "enf.2017","dia.2017","def.2017")

mes <- c(means$enf.2014,means$dia.2014,means$def.2014)
sde <- c(sdes$enf.2014,sdes$dia.2014,sdes$def.2014)


train[,c(predcols):=trainY[,mapply(FUN=function(x,m,s){x*s+m},x=.SD,m=mes,s=sde,SIMPLIFY=F),.SDcols=predcols]]


previo1 <- melt(train,id.vars = c("edo","mpo","loc"),measure.vars =predcols)

previo1[,c("caso","year"):=tstrsplit(variable,".",fixed=T)]

previo2 <- dcast(previo1,edo+mpo+loc+year~caso,value.var = "value",fun.aggregate=sum)
previo2[,year:=as.numeric(year)]
#faneamos
cat.loc[,tamano:=cut( as.numeric(z1),
                       breaks = c(-Inf,2500,15000,50000,100000,Inf),
                       right=F,
                       labels=c("a",
                                "b",
                                "c",
                                "d",
                                "e"))]
setkey(cat.edad2,tamano)
setkey(cat.loc,tamano)

fan <- cat.loc[cat.edad2,allow.cartesian=TRUE]

fan <- fan[,.(edo,mpo,loc,grupo_sexo,prop)]
#
setkeyv(fan,c("edo","mpo","loc"))
setkeyv(previo2,c("edo","mpo","loc"))

previo3 <- previo2[fan,allow.cartesian=TRUE]

previo3[,enf.f:=enf*prop][,dias.perdidos.f:=dia*prop][,defunciones.f:=def*prop]

previo3[,c("sexok","grupok"):=tstrsplit(grupo_sexo,"_",fixed=T)]
previo3[,sexok:=ifelse(sexok=="h","hombres","mujeres")]


setkeyv(previo3,c("edo","mpo","loc"))
setkeyv(cat.loc,c("edo","mpo","loc"))

previo4 <- cat.loc[previo3]

previo4[,V1:=NULL][,tamano:=NULL][,def:=NULL][,dia:=NULL][,enf:=NULL][,grupo_sexo:=NULL]


#agregamos a minable

minable2[,c("enf.f","dias.perdidos.f","defunciones.f"):=NA]


minable3 <- rbind(minable2, previo4,fill=T)

minable3 <- unique(minable3)
write.csv(minable3,"./data/minable3.csv")


locs <- minable2[!is.na(enf)&!is.na(defunciones)&!is.na(dias.perdidos),.(edo,mpo,loc)]

locs <- unique(locs)

minable.s <- minable3[edo%in%locs$edo & mpo %in% locs$mpo & loc %in% locs$loc  ]

write.csv(minable.s,"./data/minable_S.csv")

