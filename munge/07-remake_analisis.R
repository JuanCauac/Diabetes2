#---
#title: Análisis de datos uno 
#author: JCAA
#date: 29/04/2016
#abstract: se predicen valores faltantes de defunciones y días pedidos
#
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---

remake <- fread("./data/remake2.csv")

vivienda <- fread("./data/vivienda.csv")

poblacion <- vivienda[mun!=0 & loc!= 0 & loc != 9998,.(pobtot,alfa=as.numeric(p15ym.an)/as.numeric(p.15ymas),escolaridad=graproes,derechohabientes=pder.ss),by=.(entidad,mun,loc)]

#poblacion <- rename(poblacion,replace=c(entidad="edo",mun="mpo"))
poblacion[,edo:=entidad][,mpo:=mun][,entidad:=NULL][,mun:=NULL]


setkeyv(remake,c("edo","mpo","loc"))  
setkeyv(poblacion,c("edo","mpo","loc"))

remake <- poblacion[remake]

#hay 5 localidades que no estan en en centro de poblacion y vivienda
#pero si en el catalogo de localidades, las quitamos porque tienen poblacion 0

remake <- remake[!is.na(pobtot)]


#hay que predecir cuatos dias perdidos y defunciones hay 

remake[,escolaridad:=as.numeric(escolaridad)]

#form <- as.formula("dias.perdidos~tratamiento+alfa+pobtot+escolaridad+year")

form <- as.formula("dias.perdidos~year+pobtot")

m.dias <- glm(form,remake[!is.na(dias.perdidos),],family=poisson )#gaussian )

pred <- predict(m.dias,remake[is.na(dias.perdidos),])

#validar
plot(remake[,dias.perdidos])
plot(exp(pred))

remake[is.na(dias.perdidos),c("dias.perdidos","pred.d"):=.(exp(pred),T)]
write.csv(remake,"./data/remake2_2.csv")

######

form <- as.formula("defunciones~year+pobtot+tratamiento")

m.def <- glm(form,remake[!is.na(defunciones),],family=poisson )#gaussian )

pred <- predict(m.def,remake[is.na(defunciones),])

#validar
plot(remake[,defunciones])
plot(exp(pred))

mean(remake[,defunciones],na.rm=T)
mean(exp(pred))


remake[is.na(defunciones),c("defunciones","pred.m"):=.(exp(pred),T)]
write.csv(remake,"./data/remake2_2.csv")

