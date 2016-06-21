#---
#title: Limpieza de datos 1
#author: JCAA
#date: 29/04/2016
#abstract: primera limpieza, se limpian los nombres
#         de los campos y se escriben como tablas u objetos
#          listos para su exploración. 
#          Ver documento extras para pasos previos que se realizan y no se documentan 
---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()


# escuelas
escuelas <-fread("data/readable/escuelas.csv")

##quitar warning usando col classes
a<-as.character(escuelas[,lapply(.SD,class)])
escuelas <-fread("data/readable/escuelas.csv",colClasses=a)

##arreglar nombres
names(escuelas) <-gsub("\\.$",gsub("\\.+",make.names(names(escuelas)),replacement="."),replacement="")%>%tolower()

## escribir a file
write.csv(escuelas,"./data/escuelas.csv")
# escuelas <- fread("./data/escuelas.csv")

#---

#censo de poblacion y vivienda

vivienda <- fread("./data/raw/ITER_15TXT10.txt")
a <- as.character(vivienda[,lapply(.SD,class)])
vivienda <- fread("./data/raw/ITER_15TXT10.txt",colClasses = a)

names(vivienda)<-gsub("_+",names(vivienda),replacement=".")%>%tolower()

write.csv(vivienda,"./data/vivienda.csv")
vivienda<-fread("./data/vivienda.csv")

#-----------------------------------------------------------

# DENUE comida

comida <-  fread("./data/raw/DENUE_INEGI_72_.csv")
a <- as.character(comida[,lapply(.SD,class)])
comida <-  fread("./data/raw/DENUE_INEGI_72_.csv",colClasses = a,encoding = "Latin-1")

names(comida)<-
  names(comida)%>%
  gsub(pattern = " +",replacement=".") %>%
  gsub(pattern = "Ã¡+",replacement="a") %>%
  gsub(pattern = "Ã©+",replacement="e") %>%
  gsub(pattern = "í+",replacement="i") %>%
  gsub(pattern = "Ã³+",replacement="o") %>%
  gsub(pattern = "Ãº+",replacement="u") %>% 
  gsub(pattern = "Ã+",replacement="a") %>%
  tolower()

write.csv(comida,"./data/comida.csv")
comida <- fread("./data/comida.csv")

# DENUE deporte

deporte <- fread("./data/raw/DENUE_INEGI_71_.csv")
a <- as.character(deporte[,lapply(.SD,class)])
deporte <- fread("./data/raw/DENUE_INEGI_71_.csv",colClasses = a,encoding = "Latin-1")


names(deporte)<- names(deporte) %>%
  gsub(pattern = " +",replacement=".") %>%
  gsub(pattern = "á+",replacement="a") %>%
  gsub(pattern = "é+",replacement="e") %>%
  gsub(pattern = "í+",replacement="i") %>%
  gsub(pattern = "ó+",replacement="o") %>%
  gsub(pattern = "ú+",replacement="u") %>% 
  gsub(pattern = "Á+",replacement="a") %>%
  tolower()

write.csv(deporte,"./data/deporte.csv")
deporte <- fread("./data/deporte.csv")

#no usado
{
  #   
  # #datos mortalidad INEGI 
  #  morta10 <- fread("./data/readable/2010.csv")
  #  morta11 <- fread("./data/readable/2011.csv")
  #  morta12 <- fread("./data/readable/2012.csv")
  #  morta13 <- fread("./data/readable/2013.csv")
  #  morta14 <- fread("./data/readable/2014.csv")
  #  morta10[,year:=2010]
  #  morta11[,year:=2011]
  #  morta12[,year:=2012]
  #  morta13[,year:=2013]
  #  morta14[,year:=2014]
  #  morta <- rbindlist(list(morta10,morta12,morta13,morta14),fill = T)
  #  morta <- melt(morta, id.vars=c("year","enfermedad","sexo","edad"))
  #  names(morta)<-c("year","enfermedad","sexo","edad","municipio","muertes")
  #  morta[,muertes:=as.numeric(gsub(pattern = ",+",muertes,replacement = ""))]
  # write.csv(morta,"./data/mortalidad.csv")
  
  #-----------------------------------------------------------
  
  # Datos de GBD mortalidad y morbilidad 
  # 
  # muertes<-fread("./data/raw/IHME-Data-México-Deaths.csv")
  # a <- as.character(muertes[,lapply(.SD,class)])
  # muertes <- fread("./data/raw/IHME-Data-México-Deaths.csv",colClasses = a,encoding = "UTF-8")
  # 
  # names(muertes)<-names(muertes)%>% 
  #                 gsub(pattern = "ï»¿+",replacement = "id") %>%
  #                 gsub(pattern = "_+",replacement = ".")
  
} # no usado 
#-----------------------------------------------------------

# datos de secretaria de Salud

#egresos hospitalarios 2010-2013


egresos2010 <- fread("./data/raw/egresos_hospitalarios/2010-2013/EGRESO_2010.csv")
egresos2010[,SEGUNDA_AFECCION:=NULL]
egresos2010[,year:=2010]

egresos2011 <- read.csv("./data/raw/egresos_hospitalarios/2010-2013/EGRESO_2011.csv",fill=T)
egresos2011<-data.table(egresos2011)
egresos2011[,year:=2011]

egresos2012 <- read.csv("./data/raw/egresos_hospitalarios/2010-2013/EGRESO_2012.csv",fill=T)
egresos2012<-data.table(egresos2012)
egresos2012[,year:=2012]

egresos2013 <- fread("./data/raw/egresos_hospitalarios/2010-2013/EGRESO_2013.csv")
egresos2013[,year:=2013]

egresos <- rbindlist(list(egresos2010,egresos2011,egresos2012,egresos2013))
rm(egresos2010,egresos2011,egresos2012,egresos2013)

names(egresos)<-names(egresos)%>% 
  gsub(pattern = "ï»¿+",replacement="") %>%
  gsub(pattern = "_+",replacement=".") %>%
  tolower()
#write.csv(egresos,"./data/egresos.csv")

# cruce con catalogos. 

cat.cie<-fread("./data/raw/egresos_hospitalarios/2010-2013/catalogos/saeh_catcie10.csv",encoding="UTF-8")
names(cat.cie)<-names(cat.cie)%>%
                gsub(pattern = "ï»¿\"letra\"",replacement = "letra")%>%
                gsub(pattern = "_+",replacement = ".")%>%
                tolower()
cat.cie <- cat.cie[,.(causa,nombre)]


cat.edo <- fread("./data/raw/egresos_hospitalarios/2010-2013/catalogos/saeh_catentidadres.csv",encoding = "UTF-8")
names(cat.edo)<-c("edo","estado")


cat.mpo <- fread("./data/raw/egresos_hospitalarios/2010-2013/catalogos/saeh_catmpores.csv",encoding = "UTF-8")
names(cat.mpo)<-c("edo","mpo","municipio")
cat.mpo[,c("edo","mpo"):=.(as.integer(edo),as.integer(mpo))]
cat.mpo <- unique(cat.mpo)

cat.loc <- fread("./data/raw/egresos_hospitalarios/2010-2013/catalogos/saeh_catlocalidades.csv",encoding = "UTF-8")
names(cat.loc)<-c("edo","mpo","loc","localidad")
cat.loc[,c("edo","mpo","loc"):=.(as.integer(edo),as.integer(mpo),as.integer(loc))]
cat.loc <- unique(cat.loc)


setkey(egresos,afecprin)
setkey(cat.cie,causa)
 
egresos <- cat.cie[egresos]

setkey(cat.edo,edo)
setkey(egresos,entidad)

egresos<-cat.edo[egresos]

setkeyv(cat.mpo,c("edo","mpo"))
setkeyv(egresos,c("edo","munic"))

egresos<-cat.mpo[egresos]

setkeyv(cat.loc,c("edo","mpo","loc"))
setkeyv(egresos,c("edo","mpo","loc"))

egresos<- cat.loc[egresos]

write.csv(egresos,"./data/egresos.csv")

##############################
#egresos hostpitalarios 2014
##############################

egresos2014 <- fread("./data/raw/egresos_hospitalarios/2014/EGRESO.csv")
a<- as.character(egresos2014[,lapply(.SD,class)])
egresos2014 <- fread("./data/raw/egresos_hospitalarios/2014/EGRESO.csv",colClasses = a)
names(egresos2014) <- names(egresos2014) %>% gsub(pattern = "_+",replacement = ".") %>%  tolower()
egresos2014[,year:=2014]

# cruce con catalogos

cat.cie2014 <- fread("./data/raw/egresos_hospitalarios/2014/catalogos2014/CATCIE10.csv",encoding = "UTF-8")
names(cat.cie2014) <- names(cat.cie2014)%>% gsub(pattern = "_+",replacement = ".") %>%  tolower()
cat.cie2014<-cat.cie2014[ ,.(nombre,causa)]

cat.edo2014 <- fread("data/raw/egresos_hospitalarios/2014/catalogos2014/CATENTIDADRES.csv",encoding = "UTF-8",colClasses = c("integer","character"))
names(cat.edo2014) <- c("edo","estado")
cat.edo2014[,edo:=as.integer(edo)]

cat.mpo2014 <- fread("./data/raw/egresos_hospitalarios/2014/catalogos2014/CATMPORES.csv",encoding = "UTF-8",colClasses = c("integer","integer","character"))
names(cat.mpo2014) <- c("edo","mpo","municipio")
cat.mpo2014[,c("edo","mpo"):=.(as.integer(edo),as.integer(mpo))]
cat.mpo2014<-unique(cat.mpo2014)

cat.loc2014 <- fread("data/raw/egresos_hospitalarios/2014/catalogos2014/CATLOCALIDADRES.csv",encoding = "UTF-8",colClasses = c("integer","integer","integer","character"))
names(cat.loc2014)<-c("edo","mpo","loc","localidad")
cat.loc2014[,c("edo","mpo","loc"):=.(as.integer(edo),as.integer(mpo),as.integer(loc))]

#cruce con catalogos 
setkey(egresos2014,afecprin)
setkey(cat.cie2014,causa)
egresos2014 <- cat.cie2014[egresos2014]

setkey(egresos2014,entidad)
setkey(cat.edo2014,edo)
egresos2014 <- cat.edo2014[egresos2014]

setkeyv(egresos2014,c("edo","munic"))
setkeyv(cat.mpo2014,c("edo","mpo"))
egresos2014 <- cat.mpo2014[egresos2014]

setkeyv(egresos2014,c("edo","mpo","loc"))
setkeyv(cat.loc2014,c("edo","mpo","loc"))
egresos2014 <- cat.loc2014[egresos2014]



names(egresos)[which(!names(egresos) %in% names(egresos2014))]


egresos<-rbind(egresos,egresos2014)

write.csv(egresos,"./data/egresos.csv")

#---------------

# muertes

muertes2010 <- fread("./data/raw/egresos_hospitalarios/2010-2013/DEFUNC_2010.csv",encoding = "UTF-8")
muertes2010[,year:=2010]

muertes2011 <- fread("./data/raw/egresos_hospitalarios/2010-2013/DEFUNC_2011.csv",encoding = "UTF-8")
muertes2011[,year:=2011]

muertes2012 <- fread("./data/raw/egresos_hospitalarios/2010-2013/DEFUNC_2012.csv",encoding = "UTF-8")
muertes2012[,year:=2012]

muertes2013 <- fread("./data/raw/egresos_hospitalarios/2010-2013/DEFUNC_2013.csv",encoding = "UTF-8")
muertes2013[,year:=2013]

muertes2014 <- fread("./data/raw/egresos_hospitalarios/2014/DEFUNC.csv",encoding = "UTF-8")
muertes2014[,year:=2014]

muertes <- rbindlist(list(muertes2010,muertes2011,muertes2012,muertes2013,muertes2014))
rm(muertes2010,muertes2011,muertes2012,muertes2013,muertes2014)

names(muertes)<- names(muertes)%>%
                gsub(pattern = "ï»¿",replacement = "" ) %>%
                tolower()

write.csv(muertes,"./data/muertes.csv")



#------------------------------------------
# datos de seguimiento de diabetes

seguimiento2010 <- fread("./data/readable/seguimiento/2010-seguimiento.csv",na.strings = "")
seguimiento2011 <- fread("./data/readable/seguimiento/2011-seguimiento.csv",na.strings = "")
seguimiento2012 <- fread("./data/readable/seguimiento/2012-seguimiento.csv",na.strings = "")
seguimiento2013 <- fread("./data/readable/seguimiento/2013-seguimiento.csv",na.strings = "")
seguimiento2014 <- fread("./data/readable/seguimiento/2014-seguimiento.csv",na.strings = "")

seguimiento2010[,year:=2010]
seguimiento2011[,year:=2011]
seguimiento2012[,year:=2012]
seguimiento2013[,year:=2013]
seguimiento2014[,year:=2014]

noms<- c("clues",
          "diabetes.ingreso.M.menos.20",
          "diabetes.ingreso.M.20.59",
          "diabetes.ingreso.M.60.mas",
          "diabetes.ingreso.H.menos.20",
          "diabetes.ingreso.H.20.59",
          "diabetes.ingreso.H.60.mas",
          "diabetes.tratamiento.M.menos.20",
          "diabetes.tratamiento.M.20.59",
          "diabetes.tratamiento.M.60.mas",
          "diabetes.tratamiento.H.menos.20",
          "diabetes.tratamiento.H.20.59",
          "diabetes.tratamiento.H.60.mas",
          "diabetes.controlado.M.menos.20",
          "diabetes.controlado.M.20.59",
          "diabetes.controlado.M.60.mas",
          "diabetes.controlado.H.menos.20",
          "diabetes.controlado.H.20.59",
          "diabetes.controlado.H.60.mas",
          "diabetes.total",
          #
          "hipertension.ingreso.M.menos.20",
          "hipertension.ingreso.M.20.59",
          "hipertension.ingreso.M.60.mas",
          "hipertension.ingreso.H.menos.20",
          "hipertension.ingreso.H.20.59",
          "hipertension.ingreso.H.60.mas",
          "hipertension.tratamiento.M.menos.20",
          "hipertension.tratamiento.M.20.59",
          "hipertension.tratamiento.M.60.mas",
          "hipertension.tratamiento.H.menos.20",
          "hipertension.tratamiento.H.20.59",
          "hipertension.tratamiento.H.60.mas",
          "hipertension.controlado.M.menos.20",
          "hipertension.controlado.M.20.59",
          "hipertension.controlado.M.60.mas",
          "hipertension.controlado.H.menos.20",
          "hipertension.controlado.H.20.59",
          "hipertension.controlado.H.60.mas",
          "hipertension.total",
          #
          "obesidad.ingreso.M.menos.20",
          "obesidad.ingreso.M.20.59",
          "obesidad.ingreso.M.60.mas",
          "obesidad.ingreso.H.menos.20",
          "obesidad.ingreso.H.20.59",
          "obesidad.ingreso.H.60.mas",
          "obesidad.tratamiento.M.menos.20",
          "obesidad.tratamiento.M.20.59",
          "obesidad.tratamiento.M.60.mas",
          "obesidad.tratamiento.H.menos.20",
          "obesidad.tratamiento.H.20.59",
          "obesidad.tratamiento.H.60.mas",
          "obesidad.controlado.M.menos.20",
          "obesidad.controlado.M.20.59",
          "obesidad.controlado.M.60.mas",
          "obesidad.controlado.H.menos.20",
          "obesidad.controlado.H.20.59",
          "obesidad.controlado.H.60.mas",
          "obesidad.total",
          "total",
          "year")

noms2<- c("clues",
         "diabetes.ingreso.M.20.59",
         "diabetes.ingreso.M.60.mas",
         "diabetes.ingreso.H.20.59",
         "diabetes.ingreso.H.60.mas",
         "diabetes.tratamiento.M.20.59",
         "diabetes.tratamiento.M.60.mas",
         "diabetes.tratamiento.H.20.59",
         "diabetes.tratamiento.H.60.mas",
         "diabetes.controlado.M.20.59",
         "diabetes.controlado.M.60.mas",
         "diabetes.controlado.H.20.59",
         "diabetes.controlado.H.60.mas",
         "diabetes.total",
         #
         "hipertension.ingreso.M.20.59",
         "hipertension.ingreso.M.60.mas",
         "hipertension.ingreso.H.20.59",
         "hipertension.ingreso.H.60.mas",
         "hipertension.tratamiento.M.20.59",
         "hipertension.tratamiento.M.60.mas",
         "hipertension.tratamiento.H.20.59",
         "hipertension.tratamiento.H.60.mas",
         "hipertension.controlado.M.20.59",
         "hipertension.controlado.M.60.mas",
         "hipertension.controlado.H.20.59",
         "hipertension.controlado.H.60.mas",
         "hipertension.total",
         #
         "obesidad.ingreso.M.20.59",
         "obesidad.ingreso.M.60.mas",
         "obesidad.ingreso.H.20.59",
         "obesidad.ingreso.H.60.mas",
         "obesidad.tratamiento.M.20.59",
         "obesidad.tratamiento.M.60.mas",
         "obesidad.tratamiento.H.20.59",
         "obesidad.tratamiento.H.60.mas",
         "obesidad.controlado.M.20.59",
         "obesidad.controlado.M.60.mas",
         "obesidad.controlado.H.20.59",
         "obesidad.controlado.H.60.mas",
         "obesidad.total",
         "total",
         "year")


names(seguimiento2010)<-noms
names(seguimiento2011)<-noms
names(seguimiento2012)<-noms
names(seguimiento2013)<-noms2
names(seguimiento2014)<-noms2


seguimiento <- rbindlist(list(seguimiento2010,
                              seguimiento2011,
                              seguimiento2012,
                              seguimiento2013,
                              seguimiento2014),fill=T)


seguimiento[,c("diabetes.ingreso.M.menos.20",
               "diabetes.ingreso.H.menos.20",
               "diabetes.tratamiento.M.menos.20",
               "diabetes.tratamiento.H.menos.20",
               "diabetes.controlado.M.menos.20",
               "diabetes.controlado.H.menos.20",
               "hipertension.ingreso.M.menos.20",
               "hipertension.ingreso.H.menos.20",
               "hipertension.tratamiento.M.menos.20",
               "hipertension.tratamiento.H.menos.20",
               "hipertension.controlado.M.menos.20",
               "hipertension.controlado.H.menos.20",
               "obesidad.ingreso.M.menos.20",
               "obesidad.ingreso.H.menos.20",
               "obesidad.tratamiento.M.menos.20",
               "obesidad.tratamiento.H.menos.20",
               "obesidad.controlado.M.menos.20",
               "obesidad.controlado.H.menos.20")
               :=NULL]

write.csv(seguimiento,"./data/seguimiento.csv")


# catalogo etario


edad.cat <- fread("./data/readable/catalogo.etario.csv",encoding = "Latin-1")

names(edad.cat)<-names(edad.cat)%>% 
                 tolower %>%
                  gsub(pattern = " +",replacement = ".")%>%
                  gsub(pattern = "ñ+",replacement = "n")


edad.cat<- melt(edad.cat,id.vars = c("tamano.de.localidad",
                          "grupos.quinquenales.de.edad",
                          "estimador"),
              c("hombres","mujeres","población.total"))

edad.cat <- dcast(edad.cat,tamano.de.localidad+grupos.quinquenales.de.edad+variable~estimador,value.var = "value" )

write.csv(edad.cat,"./data/catalogo_edades.csv")


####################

loc.cat <- fread("./data/readable/cat.loc.csv")
names(loc.cat)<-c("edo","estado","mpo","municipio","loc","localidad","ambito","lat","lon","z1")
num <- c("edo","mpo","loc","lat","lon")
loc.cat[,lapply(.SD,class),.SDcols=num]
write.csv(loc.cat,"./data/loc_cat.csv")


####

clues.cat <- fread("./data/readable/catclues.txt")
names(clues.cat)<-names(clues.cat)%>%gsub(pattern = " ",replacement = ".")%>%tolower()


clues.cat2 <-clues.cat[,.(clues,clave.entidad,clave.municipio,clave.localidad)] 

write.csv(clues.cat2,"./data/catalogo_clues.csv")
