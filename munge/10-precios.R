  precios <-fread("./data/readable/read_cat_precios.csv")
  names(precios)<-gsub(names(precios),pattern = " ",replacement = ".")%>%tolower()

  precios[,mean(precio.promedio,na.rm=T),by=cosa]
#   cosa       V1
#   1:  comida  105.588
#   2: deporte 1147.404
  
  
  (1-(1+0.03)/(1+0.04))
  61896/(0.04-0.03)
