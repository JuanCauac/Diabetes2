#---
#title: Limpieza de datos 2
#author: JCAA
#date: 29/04/2016
#abstract: segunda limpieza, se buscan en las tablas limpias los registros competetentes 
# y se sumarizan para crear tabla minable.
#
---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()

#fact tables
muertes <- fread("./data/muertes.csv")
egreso <- fread("./data/egresos.csv")
seguimiento <-fread("./data/seguimiento.csv")

vivienda <- fread("./data/vivienda.csv")
deporte <- fread("./data/deporte.csv")
comida <-  fread("./data/comida.csv",encoding = "UTF-8")


#catalogos
cat.cie<-fread("./data/raw/egresos_hospitalarios/2014/catalogos2014/CATCIE10.csv",encoding = "UTF-8")
names(cat.cie) <- names(cat.cie)%>% gsub(pattern = "_+",replacement = ".") %>%  tolower()
cat.cie<-cat.cie[ ,.(nombre,causa)]

cat.edad <- fread("./data/catalogo_edades.csv")

cat.loc <- fread("./data/loc_cat.csv") 

cat.clues <- fread("./data/catalogo_clues.csv")



#######################
#Minable 1 Localidades
########################
## Número de habitantes por localidad 
agrupa.viv <- c("entidad","nom.ent","mun","nom.mun","loc","nom.loc")

poblacion <- vivienda[mun!=0,.(pobtot),by=agrupa.viv]
poblacion <- vivienda[mun!=0,.(pobtot,alfa=),by=agrupa.viv]

names(poblacion)<-c("edo","estado","mpo","municipio","loc","localidad","pobtot")

#calcular número de habitantes por grupo de edad y tipo de población
unique(cat.edad$tamano.de.localidad)
#clasificar las localidades
poblacion[,tamaño.de.localidad:=cut(pobtot,
                                    breaks = c(-Inf,2500,15000,50000,100000,Inf),
                                    right=F,
                                    labels=c("Menos de 2 500 habitantes",
                                             "2 500-14 999 habitantes",
                                             "15 000-49 999 habitantes",
                                             "50 000-99 999 habitantes",
                                             "100 000 y más habitantes"
                                             ))]
poblacion[,tamano:=cut(pobtot,
                 breaks = c(-Inf,2500,15000,50000,100000,Inf),
                 right=F,
                 labels=c("a",
                 "b",
                 "c",
                 "d",
                 "e"))]
#averiguar las proporciones
tams <- poblacion[,mean(pobtot),by=tamaño.de.localidad][order(tamaño.de.localidad)]

cat.edad[tamano.de.localidad=="Menos de 2 500 habitantes",tamano:="a"]
cat.edad[tamano.de.localidad=="2 500-14 999 habitantes",  tamano:="b"]
cat.edad[tamano.de.localidad=="15 000-49 999 habitantes", tamano:="c"]
cat.edad[tamano.de.localidad=="50 000-99 999 habitantes", tamano:="d"]
cat.edad[grepl(tamano.de.localidad ,pattern = "100 000 y más habitantes"),tamano:="e"]

cat.edad[tamano=="a",mean:= tams[1,V1] ]
cat.edad[tamano=="b",mean:= tams[2,V1] ]
cat.edad[tamano=="c",mean:= tams[3,V1] ]
cat.edad[tamano=="d",mean:= tams[4,V1] ]
cat.edad[tamano=="e",mean:= tams[5,V1] ]

cat.edad[,Valor:=as.numeric(gsub(Valor,pattern = ",",replacement = ".") )]
cat.edad[,prop:=Valor/mean]

cat.edad<-cat.edad[!is.na(tamano)&
                     variable!="población.total"&
                     grupos.quinquenales.de.edad!="No especificado"&
                     grupos.quinquenales.de.edad!="Total"]

cat.edad<-cat.edad[,.(tamano,variable,grupos.quinquenales.de.edad,prop,mean,Valor)]

names(cat.edad)<-c("tamano","sexo","edad","prop","mean","valor")
View(cat.edad)

setkey(cat.edad,tamano)
setkey(poblacion,tamano)

cartesiano <-poblacion[cat.edad,allow.cartesian=TRUE]

View(cartesiano)

cartesiano[,pob.edad:=pobtot*prop]

cartesiano[,c("prop","mean","valor"):=NULL]


casteado <- dcast(cartesiano, 
                  edo+mpo+loc+estado+municipio+localidad~sexo+edad,
                  value.var="pob.edad")
names(casteado)

casteado[,c("hombres_00-04 años","hombres_05-09 años"   
            ,"hombres_10-14 años","hombres_15-19 años"
            ,"mujeres_00-04 años","mujeres_05-09 años"   
            ,"mujeres_10-14 años","mujeres_15-19 años"):=NULL]

a <- names(casteado)[7:length(names(casteado))]

casteado[,c(a):=lapply(.SD,as.numeric),.SDcols=a]

names(casteado)<-names(casteado)%>% gsub(pattern = " ",replacement = "")%>% gsub(pattern = "-",replacement = ".")

casteado[,hombres_20a29:=hombres_20.24años+hombres_25.29años][
         ,hombres_30a29:=hombres_30.34años+hombres_35.39años][
         ,hombres_40a29:=hombres_40.44años+hombres_45.49años][
         ,hombres_50a29:=hombres_50.54años+hombres_55.59años][
         ,hombres_60a69:=hombres_60.64años+hombres_65.69años][
         ,hombres_70a100:=hombres_70.74años+hombres_75añosymás][
         ,mujeres_20a29:=mujeres_20.24años+mujeres_25.29años][
         ,mujeres_30a29:=mujeres_30.34años+mujeres_35.39años][
         ,mujeres_40a29:=mujeres_40.44años+mujeres_45.49años][
         ,mujeres_50a29:=mujeres_50.54años+mujeres_55.59años][
         ,mujeres_60a69:=mujeres_60.64años+mujeres_65.69años][
         ,mujeres_70a100:=mujeres_70.74años+mujeres_75añosymás]

del <- names(casteado)[7:30]

casteado[,c(del):=NULL]#despues de mucho desmadre aqui estan los grupos etarios por localidad

#
write.csv(casteado,"./data/casteado.csv")
casteado<-fread("./data/casteado.csv")

# variables del censo de poblacion y vivienda

poblacion <- vivienda[mun!=0,.(pobtot,
                               alfa=as.numeric(p15ym.an)/as.numeric(p.15ymas),
                               der=as.numeric(pder.ss)/as.numeric(pobtot)),
                      by=agrupa.viv]

poblacion <- rename(poblacion, c(entidad="edo",nom.ent="estado",mun="mpo",nom.mun="municipio",nom.loc="localidad"))

## número de restaurantes por localidad
write.csv(
comida[,.N,by=.(nombre.de.clase.de.la.actividad,codigo.de.la.clase.de.actividad.scian)],
"./reports/catalogo_comidas.csv"
)
#revisando estas son las clabes de restaurantes poco sanos
comer <- c(722515,722517,722513,722514,722519,722511,722516,722412,722330)


agrupa.com <- c("clave.entidad","entidad.federativa","clave.municipio","municipio","clave.localidad","localidad")
comederos <- comida[codigo.de.la.clase.de.actividad.scian%in%comer,.N,by=agrupa.com]
names(comederos)<-c("edo","estado","mpo","municipio","loc","localidad","N.restaurant")


## número de centros deportivos por localidad
write.csv(
  deporte[,.N,by=.(nombre.de.clase.de.la.actividad,codigo.de.la.clase.de.actividad.scian)],
  "./reports/catalogo_deporte.csv"
)

#revisando estas son las clabes de centros deportivos. 
deportista <- c(713943,713944713998,713992,713950,711121,713910,713941,713942,711111,713999,712190,711122)

juegos <- deporte[codigo.de.la.clase.de.actividad.scian%in%deportista,.N,by=agrupa.com]
names(juegos)<-c("edo","estado","mpo","municipio","loc","localidad","N.deporte")




#Join para tabla minable
keys <- c("edo","mpo","loc")


juegos[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]
comederos[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]
poblacion[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]
casteado[,c(keys):=lapply(.SD,as.numeric),.SDcols=keys]

poblacion[,c("estado","municipio","localidad"):=NULL]
juegos[,c("estado","municipio","localidad"):=NULL]
comederos[,c("estado","municipio","localidad"):=NULL]
casteado[,c("estado","municipio","localidad"):=NULL]


setkeyv(juegos,keys)
setkeyv(comederos,keys)
setkeyv(poblacion,keys)
setkeyv(casteado,keys)

sum(!casteado[,paste(edo,mpo,loc,sep="")] %in%poblacion[,paste(edo,mpo,loc,sep="")])
sum(!juegos[,paste(edo,mpo,loc,sep="")] %in%poblacion[,paste(edo,mpo,loc,sep="")])
sum(!comederos[,paste(edo,mpo,loc,sep="")] %in%poblacion[,paste(edo,mpo,loc,sep="")])


minable1 <- juegos[comederos[casteado[poblacion]]]

setkeyv(minable1,keys)
setkeyv(loc.cat,keys)

minable1<- loc.cat[minable1]

#minable1[,lat:=as.numeric(substring(latitud,1,2))+as.numeric(substring(latitud,3,4))/60+as.numeric(substring(latitud,5,6))/3600] 
#minable1[nchar(longitud)>6,lon:=-1*(as.numeric(substring(longitud,1,3))+as.numeric(substring(longitud,4,5))/60+as.numeric(substring(longitud,6,7))/3600)] 
#minable1[nchar(longitud)==6,lon:=-1*(as.numeric(substring(longitud,1,2))+as.numeric(substring(longitud,2,3))/60+as.numeric(substring(longitud,3,4))/3600)] 


write.csv(minable1,"./data/minable1.csv") #porfin tabla minable

####################################################################################
#Minable 2 muertos, horas perdidas y padecientes por grupo de edad año y localidad #
####################################################################################


#agregamos variable de grupo etario 
egreso[,grupo:=cut(edad,
                   breaks = c(-Inf,20,30,40,50,60,70,Inf),
                   right=F,
                   labels=c("0-19","20-29","30-39","40-49","50-59","60-69","70-100"))]


#contamos cuantos días se pierden por año en los hospitales por diavetes

egreso[,dias.esta:=as.numeric(dias.esta)]

agrupa <- c("grupo","sexo","edo","mpo","loc","estado","municipio","localidad","year")

causas <- cat.cie[grepl("Diabetes+",nombre,ignore.case = T)|grepl("hipertensión+",nombre,ignore.case = T),causa]

dias <- egreso[grupo!="0-19" 
               & causa %in% causas 
               #&edo == 15
               ,.(dias.perdidos=sum(dias.esta,na.rm=T))
               ,by=agrupa]

# Ahora las muertes relacionadas a diavetes e hipertensión
dead <-     muertes[causaia %in% causas|  
                      causaib %in% causas| 
                      causaic %in% causas| 
                      causaid %in% causas| 
                      causaiia %in% causas| 
                      causaiib %in% causas| 
                      causabas %in% causas,id]

defun <- egreso[id%in%dead &grupo!="0-19"&motegre==5 ,.(defunciones=.N),by=agrupa]

#Ahora el mega desmadre que es obtener los enfermos por grupo de edad

#quitamos variableno inutiles

del <- names(seguimiento)[which(grepl(names(seguimiento),pattern="ingreso"))]

seguimiento[,c(del):=NULL]

del2 <- names(seguimiento)[which(grepl(names(seguimiento),pattern="total"))]

seguimiento[,c(del2):=NULL]
seguimiento[,V1:=NULL]

#derretimos 
seg <-melt(seguimiento,id.vars = c("clues","year"))

seg[,sexo:=ifelse(grepl(pattern="M",variable),"mujeres","hombres")]
seg[,grupo:=ifelse(grepl(pattern="60.mas",variable),"60.mas","20.59")]
seg[,estatus:=ifelse(grepl(pattern="controlado",variable),"controlado","tratamiento")]
seg[grepl(pattern="diabetes",variable),causa:="diabetes"]
seg[grepl(pattern="obesidad",variable),causa:="obesidad"]
seg[grepl(pattern="hipertension",variable),causa:="hipertension"]


#crusamos con catalogo clues para agregar clabes de localidad

setkey(cat.clues,clues)
setkey(seg,clues)

seg <- cat.clues[seg]

# crusamos con catalogo de entidades para agregar tamaño localidad
setkeyv(poblacion,keys)
setkeyv(cat.loc,keys)

cat.loc2 <- poblacion[,.(edo,mpo,loc,pobtot)][cat.loc]

cat.loc2[,tamaño.de.localidad:=cut(pobtot,
                                  breaks = c(-Inf,2500,15000,50000,100000,Inf),
                                  right=F,
                                  labels=c("Menos de 2 500 habitantes",
                                           "2 500-14 999 habitantes",
                                           "15 000-49 999 habitantes",
                                           "50 000-99 999 habitantes",
                                           "100 000 y más habitantes"
                                  ))]
cat.loc2[,tamano:=cut(pobtot,
                       breaks = c(-Inf,2500,15000,50000,100000,Inf),
                       right=F,
                       labels=c("a",
                                "b",
                                "c",
                                "d",
                                "e"))]

seg <-rename(seg,c(clave.entidad="edo",clave.municipio="mpo",clave.localidad="loc"))

setkeyv(seg,keys)
setkeyv(cat.loc2,keys)

cat.loc2[,lapply(.SD,class),.SDcols=keys]
seg[,lapply(.SD,class),.SDcols=keys]

seg <- cat.loc2[seg]

# agregamos por localidad
seg.loc <- seg[,.(cuenta=sum(as.numeric(value))),by=.(edo,mpo,loc,sexo,grupo,year,tamano)] 
  
# separamos en dos partes para despues fanear

seg.old.h <- seg.loc[grupo=="60.mas"&sexo=="hombres",]
seg.yng.h <- seg.loc[grupo =="20.59"&sexo=="hombres",]
seg.old.m <- seg.loc[grupo=="60.mas"&sexo=="mujeres",]
seg.yng.m <- seg.loc[grupo =="20.59"&sexo=="mujeres",]

#modificamos un poco el catalogo

meltedcat <- dcast(cat.edad,tamano ~sexo+edad , value.var="prop")

meltedcat[,c(names(meltedcat)[2:5]):=NULL]
meltedcat[,c(names(meltedcat)[14:17]):=NULL]

names(meltedcat)<-names(meltedcat)%>%gsub(pattern = " ",replacement =".")%>%gsub(pattern = "-",replacement ="a")


meltedcat[,h_20a29:=as.numeric(hombres_20a24.años)+as.numeric(hombres_25a29.años),by=tamano]
meltedcat[,h_30a39:=as.numeric(hombres_30a34.años)+as.numeric(hombres_35a39.años),by=tamano]
meltedcat[,h_40a49:=as.numeric(hombres_40a44.años)+as.numeric(hombres_45a49.años),by=tamano]
meltedcat[,h_50a59:=as.numeric(hombres_50a54.años)+as.numeric(hombres_55a59.años),by=tamano]
meltedcat[,h_60a69:=as.numeric(hombres_60a64.años)+as.numeric(hombres_65a69.años),by=tamano]
meltedcat[,h_70a100:=as.numeric(hombres_70a74.años)+as.numeric(hombres_75.años.y.más),by=tamano]

meltedcat[,m_20a29:=as.numeric(mujeres_20a24.años)+as.numeric(mujeres_25a29.años),by=tamano]
meltedcat[,m_30a39:=as.numeric(mujeres_30a34.años)+as.numeric(mujeres_35a39.años),by=tamano]
meltedcat[,m_40a49:=as.numeric(mujeres_40a44.años)+as.numeric(mujeres_45a49.años),by=tamano]
meltedcat[,m_50a59:=as.numeric(mujeres_50a54.años)+as.numeric(mujeres_55a59.años),by=tamano]
meltedcat[,m_60a69:=as.numeric(mujeres_60a64.años)+as.numeric(mujeres_65a69.años),by=tamano]
meltedcat[,m_70a100:=as.numeric(mujeres_70a74.años)+as.numeric(mujeres_75.años.y.más),by=tamano]

meltedcat[,names(meltedcat)[2:25]:=NULL]

#- regresamos
cat.edad2 <- melt(meltedcat,id.vars = "tamano")
names(cat.edad2)<-c("tamano","grupo_sexo","prop")
#- separamos 

cat.edad.yng.h <- cat.edad2[grupo_sexo%in%c("h_20a29","h_30a39","h_40a49","h_50a59"),] 
cat.edad.old.h <- cat.edad2[grupo_sexo%in%c("h_60a69","h_70a100")]
cat.edad.yng.m <- cat.edad2[grupo_sexo%in%c("m_20a29","m_30a39","m_40a49","m_50a59"),] 
cat.edad.old.m <- cat.edad2[grupo_sexo%in%c("m_60a69","m_70a100")]


#faneamos 

setkey(cat.edad.yng.h,tamano)
setkey(cat.edad.old.h,tamano)
setkey(cat.edad.yng.m,tamano)
setkey(cat.edad.old.m,tamano)


setkey(seg.yng.h,tamano)
setkey(seg.old.h,tamano)
setkey(seg.yng.m,tamano)
setkey(seg.old.m,tamano)


yng.h <- seg.yng.h[cat.edad.yng.h, allow.cartesian=TRUE]
old.h <- seg.old.h[cat.edad.old.h, allow.cartesian=TRUE]
yng.m <- seg.yng.m[cat.edad.yng.m, allow.cartesian=TRUE]
old.m <- seg.old.m[cat.edad.old.m, allow.cartesian=TRUE]


#porfin aquí tenemos la tabla con enfermos por edad municiío, sexo, y año 
enfermos <- rbindlist(list(yng.h,old.h,yng.m,old.m))

enfermos[,enf:=prop*cuenta]

#####
#homologar campos.
defun[,grupok:=gsub(grupo,pattern = "-",replacement = "a") ]

dias[,grupok:=gsub(grupo,pattern = "-",replacement = "a") ]

enfermos[,grupok:=gsub(grupo_sexo,pattern = "._",replacement ="")]

#
dias[,sexok:=ifelse(sexo==1,"hombres","mujeres")]
defun[,sexok:=ifelse(sexo==1,"hombres","mujeres")]
enfermos[,sexok:=ifelse(sexo=="hombres","hombres","mujeres")]

yaves <- c("year","edo","mpo","loc","sexok","grupok")

############
setkeyv(defun,yaves)
setkeyv(dias,yaves)
setkeyv(enfermos,yaves)

minable2 <- enfermos[defun[dias]]

quita <- c("tamano","grupo_sexo","sexo","grupo","i.grupo","i.sexo", "i.grupo.1","i.sexo.1","i.estado","i.municipio","i.localidad" )

minable2[,c(quita):=NULL]


minable2[,c("estado","municipio","localidad"):=NULL]

setkeyv(minable2,keys)
setkeyv(cat.loc,keys)

minable2<-cat.loc[minable2]

minable2[edo==15,]


write.csv(minable2[edo==15,],"./data/minable2.csv")

