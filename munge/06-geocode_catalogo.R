library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)


cat.clues2 <- fread("./data/readable/catclues.txt",encoding = "UTF-8")
names(cat.clues2) <- gsub("\\.$",gsub("\\.+",make.names(names(cat.clues2)),replacement="."),replacement="")%>%tolower()

cat.clues<-cat.clues[clave.entidad==15,]
cat.clues<-unique(cat.clues)

cat.clues[,.N]

des <-c("nombre.de.la.entidad","nombre.del.municipio","nombre.de.la.localidad")

trans<-function(x){
  x%>%
    gsub(pattern = "Á",replacement = "A" ,ignore.case = T)%>%
    gsub(pattern = "É",replacement = "E" ,ignore.case = T)%>%
    gsub(pattern = "Í",replacement = "I" ,ignore.case = T)%>%
    gsub(pattern = "Ó",replacement = "O" ,ignore.case = T)%>%
    gsub(pattern = "Ú",replacement = "U" ,ignore.case = T)%>%
    gsub(pattern = "Ñ",replacement = "N" ,ignore.case = T)
}

cat.clues[,c(des):=lapply(.SD,trans),.SDcols=des]

#

feo <- function(address){
  ans <- data.frame(lat=NA,lon=NA)
  for(add in address){
    geo_reply = geocode(add, output='all', messaging=TRUE, override_limit=TRUE)
    print(add)    
    if(is.na(geo_reply) ){
      la <- 0
      lo <- 0
    }else if(geo_reply$status== "OVER_QUERY_LIMIT"){
      while(geo_reply$status == "OVER_QUERY_LIMIT"){
        print(paste("OVER QUERY LIMIT - Pausing for 1 hour at:",Sys.time()))
        Sys.sleep(60*60)
        geo_reply <- geocode(add, output='all', messaging=TRUE, override_limit=TRUE)
      }#while
    } else if (geo_reply$status == "ZERO_RESULTS"){
      la <- 0
      lo <- 0
    }else if (geo_reply$status=="INVALID_REQUEST"){
      la <- 0
      lo <- 0
      print("INVALID_REQUEST")
    }else if (geo_reply$status == "OK"){
      la<-geo_reply$results[[1]]$geometry$location$lat
      lo<-geo_reply$results[[1]]$geometry$location$lng 
    }
    ans<-rbind(ans, data.frame(lat=la,lon=lo))
  }#for
  ans<-tail(ans,dim(ans)[1]-1)
}#function 



cat.clues[,V1:=NULL]
cat.clues[,c("lat","lon"):=as.numeric(1)]
cat.clues[,lat:=as.numeric(lat)]
cat.clues[,class(lat)]
cat.clues[,lon:=as.numeric(lon)]
cat.clues[,class(lon)]
#
N=dim(cat.clues)[1]


# loop
for(i in 1:N){
  
  #llamado a función
  cat.clues[head(which((
    !is.na(nombre.de.la.entidad)&
      !is.na(nombre.del.municipio)&
      !is.na(nombre.de.la.localidad)&
      !is.na(domicilio)&
      (lat==1)&
      (lon==1))
    == T),10), # solo 10 per reques
    c("lat","lon"):=feo(
                        iconv(
                              paste("MÉXICO, ESTADO DE",
                                    nombre.de.la.entidad,",",
                                    nombre.del.municipio,",",
                                    nombre.de.la.localidad),
                              from="latin1",to="UTF-8")
                        )[,c(1,2)]]
  
  n <-cat.clues[!is.na(nombre.de.la.entidad)&
             !is.na(nombre.del.municipio)&
             !is.na(nombre.de.la.localidad)&
             !is.na(domicilio)&
             lat==1&
             lon==1,.N]
  
  print(paste("faltan:", n))
  write.table(cat.clues,"./data/clues_geoloc.txt")
  Sys.sleep(3) # almenos 3 segundos entre request
}

write.csv(cat.clues,"./data/cat_clues.csv")



