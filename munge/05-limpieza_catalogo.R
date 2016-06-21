#---
#title: Análisis de datos uno 
#author: JCAA
#date: 29/04/2016
#abstract: se corrige catalogo de clues
#
#
#---
setwd("E:/Diabetes/")
library('ProjectTemplate')
load.project()
#---

cat.clues <- fread("./data/readable/catclues.txt", encoding ="UTF-8")
cat.loc <- fread("./data/loc_cat.csv",encoding ="Latin-1")

names(cat.clues) <- gsub("\\.$",gsub("\\.+",make.names(names(cat.clues)),replacement="."),replacement="")%>%tolower()

cat.clues <- cat.clues[,.(clues,
                          edo=clave.entidad,
                          estado=nombre.de.la.entidad,
                          mpo.mal=clave.municipio,
                          municipio=nombre.del.municipio,
                          loc.mal=clave.localidad,
                          localidad=nombre.de.la.localidad)]
cat.clues<-cat.clues[edo==15,]


homogenea <- function(x){
  x%>%
    gsub(pattern = "Á+",replacement = "A" ,ignore.case = T)%>%
    gsub(pattern = "É+",replacement = "E" ,ignore.case = T)%>%
    gsub(pattern = "Í+",replacement = "I" ,ignore.case = T)%>%
    gsub(pattern = "Ó+",replacement = "O" ,ignore.case = T)%>%
    gsub(pattern = "Ú+",replacement = "U" ,ignore.case = T)%>%
    gsub(pattern = "Ñ+",replacement = "N" ,ignore.case = T)%>%
    gsub(pattern = " +",replacement = ".")  %>% 
    gsub(pattern = "[()]+",replacement = "." ,ignore.case = T)%>%
    tolower()
}

cat.loc<-unique(cat.loc)

cat.clues[,local:=homogenea(localidad)]
cat.loc[,local:=homogenea(localidad)]

cat.clues[,municipal:=homogenea(municipio)]
cat.loc[,municipal:=homogenea(municipio)]

cat.muni <- unique(cat.loc[,.(municipal,mpo,municipio)])

unique(cat.clues$municipal)[which(!unique(cat.clues$municipal)%in%unique(cat.muni$municipal))]

cat.clues<-cat.clues[!municipal%in%c("la.concordia","no.especificado"),]

setkeyv(cat.clues,c("municipal"))#,"local"))
setkeyv(cat.muni,c("municipal"))#,"local"))

cat.clues2 <- cat.muni[cat.clues]

sum(unique(cat.clues2$local) %in% unique(cat.loc$local)) 
length(unique(cat.clues2$local))

cat.key <- cat.loc[,.(edo,mpo,loc,local,localidad)]


setkeyv(cat.key,c("mpo","local"))
setkeyv(cat.clues2,c("mpo","local"))

cat.clues3 <- cat.key[cat.clues2]

View(cat.clues3)

names(cat.clues3)

cat.clues3[is.na(edo)&is.na(mpo)&is.na(loc),]

cat.clues3[,c("local","localidad","municipal","municipio","i.edo","estado","mpo.mal","i.municipio","loc.mal","i.localidad" ):=NULL]

cat.cl4<-copy(cat.clues3)


write.csv(cat.clues3,"./data/catalogo_clues_new_new.csv")

