#---
#title: Análisis de datos uno 
#author: JCAA
#date: 29/04/2016
#abstract: se procesan tablas minables para generar nuevos indicadores y realizar predicciones.
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---

cat.clues <- fread("./data/catalogo_clues_new_new.csv")
cat.loc <- fread("./data/loc_cat.csv")
fan <- fread("./data/fan.csv")
seguimiento <- fread("./data/seguimiento.csv")

seguimiento[,N:=.N,by=clues]
seguimiento <- seguimiento[N==5]

#quitamos columans que no 
del <- names(seguimiento)[which(grepl(names(seguimiento),pattern="ingreso"))]
seguimiento[,c(del):=NULL]
del2 <- names(seguimiento)[which(grepl(names(seguimiento),pattern="total"))]
seguimiento[,c(del2):=NULL]
seguimiento[,V1:=NULL]

seg <-melt(seguimiento,id.vars = c("clues","year"),na.rm = T,variable.factor = F)

#separamos
seg[,sexo:=ifelse(grepl(pattern="M",variable),"mujeres","hombres")]
seg[,grupo:=ifelse(grepl(pattern="60.mas",variable),"60.mas","20.59")]
seg[,estatus:=ifelse(grepl(pattern="controlado",variable),"controlado","tratamiento")]
seg[grepl(pattern="diabetes",variable),causa:="diabetes"]
seg[grepl(pattern="obesidad",variable),causa:="obesidad"]
seg[grepl(pattern="hipertension",variable),causa:="hipertension"]


seg2 <- seg[,.(tratamiento=sum(as.numeric(value),na.rm=T)),by=.(clues,year)]

setkey(cat.clues,clues)
setkey(seg2,clues)

seg2 <- cat.clues[seg2]

seg2 <- rename(seg2,c(clave.entidad="edo",clave.municipio="mpo",clave.localidad="loc"))

seg.loc <- seg2[,.(tratamiento=sum(tratamiento,na.rm=T)),by=.(edo,mpo,loc,year)]

################

muertes <- fread("./data/muertes.csv")
egreso <- fread("./data/egresos.csv")

egreso <- egreso[edo==15]

cat.cie<-fread("./data/raw/egresos_hospitalarios/2014/catalogos2014/CATCIE10.csv",encoding = "UTF-8")
names(cat.cie) <- names(cat.cie)%>% gsub(pattern = "_+",replacement = ".") %>%  tolower()
cat.cie<-cat.cie[ ,.(nombre,causa)]


egreso[,grupo:=cut(edad,
                   breaks = c(-Inf,20,30,40,50,60,70,Inf),
                   right=F,
                   labels=c("0-19","20-29","30-39","40-49","50-59","60-69","70-100"))]


#contamos cuantos días se pierden por año en los hospitales por diavetes

egreso[,dias.esta:=as.numeric(dias.esta)]

agrupa <- c("grupo","sexo","edo","mpo","loc","estado","municipio","localidad","year")

causas <- cat.cie[grepl("Diabetes+",nombre,ignore.case = T)|grepl("hipertensión+",nombre,ignore.case = T)|grepl("obesidad+",nombre,ignore.case = T),causa]


dias <- egreso[grupo!="0-19" 
               & causa %in% causas 
               #&edo == 15
               ,.(dias.perdidos=sum(dias.esta,na.rm=T))
               ,by=.(clues,year)]

setkey(dias,clues)
setkey(cat.clues,clues)

dias2 <- cat.clues[dias]

dias2 <- rename(dias2,c(clave.entidad="edo",clave.municipio="mpo",clave.localidad="loc"))

dias.loc <- dias2[,.(dias.perdidos=sum(dias.perdidos,na.rm=T)),by=.(edo,mpo,loc,year)]

# Ahora las muertes relacionadas a diavetes e hipertensión
dead <-     muertes[causaia %in% causas|  
                      causaib %in% causas| 
                      causaic %in% causas| 
                      causaid %in% causas| 
                      causaiia %in% causas| 
                      causaiib %in% causas| 
                      causabas %in% causas,id]

defun <- egreso[id%in%dead &grupo!="0-19"&motegre==5 ,.(defunciones=.N),by=.(year,clues)]

setkey(defun,clues)
setkey(cat.clues,clues)

defun2 <- cat.clues[defun]

defun2 <- rename(defun2,c(clave.entidad="edo",clave.municipio="mpo",clave.localidad="loc"))

defun.loc <- defun2[,.(defunciones=sum(defunciones,na.rm=T)),by=.(edo,mpo,loc,year) ]


##

#defun.loc[,inegi:=ifelse(inegi>=1,T,F)]
#dias.loc[,inegi:=ifelse(inegi>=1,T,F)]
#seg.loc[,inegi:=ifelse(inegi>=1,T,F)]

keys <- c("edo","mpo","loc","year")

setkeyv(defun.loc,keys)
setkeyv(dias.loc,keys)
setkeyv(seg.loc,keys)

remake  <- defun.loc[dias.loc[seg.loc]]

View(remake)

write.csv(remake,"./data/remake2.csv")
####

