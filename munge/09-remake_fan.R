#---
#title: Análisis de datos uno 
#author: JCAA
#date: 18/05/2016
#abstract: se fanean los valores calculados por grupo de edad por localidad
#
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---

remake <- fread("./data/remake2_3.csv")

fan <- fread("./data/fan.csv")

#separamos grupo etario de sexo 

fan[, c("sexo", "grupo") := tstrsplit(grupo_sexo, "_", fixed=TRUE)]
fan[,sexo:=ifelse(sexo=="h","hombres","mujeres")]

#agregamos años perdidos para posterior calculo 
fan[grupo=="20a29",ene:=65-20]
fan[grupo=="30a39",ene:=65-30]
fan[grupo=="40a49",ene:=65-40]
fan[grupo=="50a59",ene:=65-50]
fan[grupo=="60a69",ene:=65-60]
fan[grupo=="70a100",ene:=0]

#agregamos prevalencias
fan[grupo=="20a29",prevalencia:=0.0331]
fan[grupo=="30a39",prevalencia:=0.0849]
fan[grupo=="40a49",prevalencia:=0.1651]
fan[grupo=="50a59",prevalencia:=0.3109]
fan[grupo=="60a69",prevalencia:=0.3273]
fan[grupo=="70a100",prevalencia:=0.2611]


#
vivienda <- fread("./data/vivienda.csv")
ocup <- vivienda[,.(ocup.h=as.numeric(pocupada.m)/as.numeric(pea.m),
                    ocup.m=as.numeric(pocupada.f)/as.numeric(pea.f)),
                    ,by=.(entidad,mun,loc)]

ocup2 <- melt(ocup,id.vars = c("entidad","mun","loc"))

ocup2[, c("grupo","sexo") := tstrsplit(variable, ".", fixed=TRUE)]
ocup2[,sexo:=ifelse(sexo=="h","hombres","mujeres")]
ocup2[,variable:=NULL][,ocup:=value][,value:=NULL][,grupo:=NULL]

ocup3 <- ocup2[,.(ocup=mean(ocup,na.rm=T)),by=sexo]

setkey(fan,sexo)
setkey(ocup3,sexo)
#
fan <- ocup3[fan]
#derecho
#dere <- vivienda[,.(derechohabientes=pder.ss),by=.(entidad,mun,loc)]
#setkeyv(dere,c("entidad","mun","loc"))
#setkeyv(fan,c("edo","mpo","loc"))
#fan <- dere[fan]

##

deporte <- fread("./data/deporte.csv")
comida <-  fread("./data/comida.csv",encoding = "UTF-8")

comer <- c(722515,722517,722513,722514,722519,722511,722516,722412,722330)

agrupa.com <- c("clave.entidad","clave.municipio","clave.localidad")
comederos <- comida[codigo.de.la.clase.de.actividad.scian%in%comer,.N,by=agrupa.com]
names(comederos)<-c("edo","mpo","loc","N.restaurant")

## número de centros deportivos por localidad
deportista <- c(713943,713944713998,713992,713950,711121,713910,713941,713942,711111,713999,712190,711122)

juegos <- deporte[codigo.de.la.clase.de.actividad.scian%in%deportista,.N,by=agrupa.com]
names(juegos)<-c("edo","mpo","loc","N.deporte")

###
#super joins del amor
keys<-c("edo","mpo","loc")
juegos[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]

fan[,edo:=entidad][,entidad:=NULL][,mpo:=mun][,mun:=NULL]

setkeyv(juegos,keys)
setkeyv(fan,keys)

fan <- juegos[fan]

comederos[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]

setkeyv(comederos,keys)
setkeyv(fan,keys)

fan <- comederos[fan]

setkeyv(remake,keys)
setkeyv(fan,keys)

#remake.n <- remake[fan,allow.cartesian=TRUE]

remake.n <-  fan[remake,allow.cartesian=TRUE]

View(remake.n)



write.csv(remake.n,"./data/remake_fan.csv")
