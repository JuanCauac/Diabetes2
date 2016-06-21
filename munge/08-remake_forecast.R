#---
#title: Análisis de datos uno 
#author: JCAA
#date: 18/05/2016
#abstract: se predicen valores próximos para las defunciones, dias perdidos y tratamiento
#
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---

remake <- fread("./data/remake2_2.csv")

keys <- c("edo","mpo","loc")

#funcion para predecir
modelando <- function(dt){
  test <- data.frame(year=c(2015,2016,2017,2018,2019,2020))
  #
  form.trat <- as.formula("tratamiento~year")
  model.trat <- glm(formula=form.trat,data=dt,family=poisson)   
  pred.trat <- predict(model.trat,test)
  #
  form.dia <- as.formula("dias.perdidos~year")
  model.dia <- glm(formula=form.dia,data=dt,family=poisson)   
  pred.dia <- predict(model.dia,test)
  #
  form.def <- as.formula("defunciones~year")
  model.def <- glm(formula=form.def,data=dt,family=poisson)   
  pred.def <- predict(model.def,test)
  #
  data.frame(tratamiento=exp(pred.trat),
             dias.perdidos=exp(pred.dia),
             defunciones=exp(pred.def),
             year=c(2015,2016,2017,2018,2019,2020))
  }

#hacemos predicciones
predi  <- remake[,modelando(.SD),by=keys]

#agregamos columnas faltantes

vivienda <- fread("./data/vivienda.csv")
poblacion <- vivienda[mun!=0 & loc!= 0 & loc != 9998,.(pobtot,alfa=as.numeric(p15ym.an)/as.numeric(p.15ymas),escolaridad=graproes,derechohabientes=pder.ss),by=.(entidad,mun,loc)]
poblacion[,edo:=entidad][,mpo:=mun][,entidad:=NULL][,mun:=NULL]

setkeyv(poblacion,keys)  
setkeyv(predi,keys)

predi <- poblacion[predi]

#ine <- unique(remake[,.(edo,mpo,loc,inegi)])
#setkeyv(ine,keys)
#predi<-ine[predi]
#predi[,pred.f:=T]

#
remake.n <- rbind(predi,remake,fill=T)

plot(remake.n[,sum(tratamiento),by=year],type="p")

write.csv(remake.n,"./data/remake2_3.csv")

##############
tab <- fread("./data/tablita.csv")
tab[,costo.syo:=as.numeric(gsub(costo.syo,pattern = ",",replacement = ""))]
diff(tab$costo.syo)

mod <- lm(data=tab,formula = costo.syo~I(Year)+I(Year^2))

co <- coef(mod)
b <- c(2010:2020)
co[1]+co[2]*b


