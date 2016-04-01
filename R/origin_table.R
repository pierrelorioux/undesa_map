library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(rgdal)
library(raster)
library(reshape2)

setwd("/Users/pierrelorioux/Documents/RO_Dakar/Projects/39_UNDESA_MAP") 

proper <- function(x)
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

undesa <- read.csv("data/undesa_w_ISO3.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
colnames(undesa)[1] <- "countryName_Destination"

#Fuck the factor convert everything in Characters. 
undesa <- data.frame(lapply(undesa,as.character),stringsAsFactors=FALSE)

test <- undesa %>% gather(Origin,nbMigrants, 5:239)
test$Origin <- (gsub(".", " ",test$Origin,fixed=TRUE ))

#get ISO 3 of the origin countries
undesaJoin <- read.csv("data/undesaIso3join.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
colnames(undesaJoin)[1] <- "Origin"

undesaJoin <- data.frame(lapply(undesaJoin,as.character),stringsAsFactors=FALSE)
test <- data.frame(lapply(test,as.character),stringsAsFactors=FALSE)

test <- full_join(test,undesaJoin,by="Origin")

#write.table(test, "out/test.csv", sep=";",row.names=FALSE)
test<- filter(test,ISO3.y!="NA")
test<- filter(test,ISO3.x!="NA")
test<-droplevels(test)
#write.table(test, "out/test.csv", sep=";",row.names=FALSE)
test <- mutate( test, nbMigrants = proper(gsub(" ", "",test$nbMigrants,fixed=TRUE)))
test <- mutate( test, nbMigrants =as.numeric(test$nbMigrants))
test <- replace(test, is.na(test), 0)

write.table(test, "out/all_migration.csv", sep=";",row.names=FALSE)

dest <- array()
iso <- array()
namesOr1 <-array()
nbmigOr1 <-  array()
namesOr2 <-array()
nbmigOr2 <-  array()
namesOr3 <-array()
nbmigOr3 <-  array()

test[,1] <- as.factor(test[,1])

e = 1
for (i in levels(test[,1])){
  print(i)
  l = test$nbMigrants[test$countryName_Destination==i]
  n= length(l)
  nb1 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n)[n]
  nb2 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n-1)[n-1]
  nb3 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n-2)[n-2]

  na1 =as.character(test$Origin[match(nb1, test$nbMigrants)])
  na2 =as.character(test$Origin[match(nb2, test$nbMigrants)])
  na3 =as.character(test$Origin[match(nb3, test$nbMigrants)])

  iso3 =test$ISO3.x[match(i, test$countryName_Destination)]
  dest[e]<- c(as.character(i))
  iso[e]<- c(as.character(iso3))
  namesOr1[e]<- c(na1)
  nbmigOr1[e]<- c(nb1)
  namesOr2[e]<- c(na2)
  nbmigOr2[e]<- c(nb2)
  namesOr3[e]<- c(na3)
  nbmigOr3[e]<- c(nb3)
  print(e)
  e = e+1
  rm(l, n, nb1, na1,nb2,na2,nb3,na3)
}

X <- cbind(dest,iso, namesOr1,nbmigOr1,namesOr2,nbmigOr2,namesOr3,nbmigOr3)
X <- data.frame(X)
dest_origin <- tbl_df(X)
colnames(dest_origin)[2] <- "ISO3"


test[,5] <- as.factor(test[,5])
#test <- test[!is.na(test$Origin), ]
test<-droplevels(test)

Ori <-array()
IsoOri <-array()
namesDe1 <-array()
nbmigDe1 <-  array()
namesDe2 <-array()
nbmigDe2 <-  array()
namesDe3 <-array()
nbmigDe3 <-  array()

f = 1
for (w in levels(test[,5])){
  print(w)
  l = test$nbMigrants[test$Origin==w]
  nd = length(l)

  nbd1 = sort(test$nbMigrants[test$Origin==w],partial = nd)[nd]
  print(nbd1)
  nbd2 = sort(test$nbMigrants[test$Origin==w],partial = nd-1)[nd-1]
  nbd3 = sort(test$nbMigrants[test$Origin==w],partial = nd-2)[nd-2]

  nad1 =as.character(test$countryName_Destination[match(nbd1, test$nbMigrants)])
  nad2 =as.character(test$countryName_Destination[match(nbd2, test$nbMigrants)])
  nad3 =as.character(test$countryName_Destination[match(nbd3, test$nbMigrants)])
  
  iso3 =test$ISO3.y[match(w, test$Origin)]
  
  Ori[f]<- c(as.character(w))
  IsoOri[f]<- c(as.character(iso3))
  namesDe1[f]<- c(nad1)
  nbmigDe1[f]<- c(nbd1)
  namesDe2[f]<- c(nad2)
  nbmigDe2[f]<- c(nbd2)
  namesDe3[f]<- c(nad3)
  nbmigDe3[f]<- c(nbd3)
  print(f)
  f = f+1
  rm(l, nd, nbd1, nad1,nbd2,nad2,nbd3,nad3)
}

Y <- cbind(Ori,IsoOri, namesDe1,nbmigDe1,namesDe2,nbmigDe2,namesDe3,nbmigDe3)
Y <- data.frame(Y)
origin_dest <- tbl_df(Y)
colnames(origin_dest)[2] <- "ISO3"


#Join by Iso Code

migration_matrix <- full_join(dest_origin,origin_dest,by="ISO3")

icrc_shp <- readOGR("/Users/pierrelorioux/Documents/RO_Dakar/Data/Baseline/World/countries/", "ICRC_world")

Migrants_matrix <- merge(icrc_shp,migration_matrix,by="ISO3")
shapefile(Migrants_matrix, "out/Migrants_by_Origin.shp",overwrite=TRUE)
#write.table(dest_origin, "out/origin_table.csv", sep=";",row.names=FALSE)


#REMOVE AFGANISTAN = 0 


#sort(unique(test$countryName_Destination[test$countryName_Destination=="Djibouti"]),partial=n-1)[n-1]

####FIND PCODES 
#icrc <- read.csv("/Users/pierrelorioux/Documents/RO_Dakar/Data/Baseline/World/countries/ICRC_world.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
#for(i in levels(undesa[,2])){
#  if(is.na(match(i, icrc$NAME_ICRC))){
#    print(i)
#  }
#}
#colnames(undesa)[2] <- "countryName"
#colnames(icrc)[3] <- "countryName"
#merged <- left_join(undesa,icrc,by="countryName")  
#write.table(merged, "out/undesa_join_ISO3.csv", sep=";",row.names=FALSE)
#undesaISO3 <- read.csv("out/undesa_ISO3_cleanned.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
