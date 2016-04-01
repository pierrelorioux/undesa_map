library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

setwd("/Users/pierrelorioux/Documents/RO_Dakar/Projects/39_UNDESA_MAP") 

proper <- function(x)
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

undesa <- read.csv("data/undesa_w_ISO3.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
colnames(undesa)[1] <- "countryName_Destination"

#write.table(undesa[1:3], "out/undesaIso3join.csv", sep=";",row.names=FALSE)

#undesa<-filter(undesa,ISO3!="NA")
#undesa<-droplevels(undesa)

test <- undesa %>% gather(Origin,nbMigrants, 5:239)

#get ISO 3 of the origin countries
undesaJoin <- read.csv("data/undesaIso3join.csv",header=TRUE,sep=";",dec=".",encoding="UTF-8")
colnames(undesaJoin)[1] <- "Origin"
test <- full_join(test,undesaJoin,by="Origin")

test<- filter(test,ISO3.y!="NA")
test<- filter(test,ISO3.x!="NA")
test<-droplevels(test)
#write.table(test, "out/test.csv", sep=";",row.names=FALSE)
test <- mutate( test, nbMigrants = proper(gsub(" ", "",test$nbMigrants,fixed=TRUE)))
test <- mutate( test, nbMigrants =as.numeric(test$nbMigrants))
test <- replace(test, is.na(test), 0)

dest <- list()
namesOr1 <-list()
nbmigOr1 <-  list()
namesOr2 <-list()
nbmigOr2 <-  list()
namesOr3 <-list()
nbmigOr3 <-  list()

e = 1
for (i in levels(test[,1])){

  print(i)
  
  l = test$nbMigrants[test$countryName_Destination==i]
  n= length(l)
  nb1 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n)[n]
  nb2 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n-1)[n-1]
  nb3 = sort(test$nbMigrants[test$countryName_Destination==i],partial=n-2)[n-2]
  #test$Origin[match(nb1, test$nbMigrants)]
  na1 =as.character(test$Origin[match(nb1, test$nbMigrants)])
  na2 =as.character(test$Origin[match(nb2, test$nbMigrants)])
  na3 =as.character(test$Origin[match(nb3, test$nbMigrants)])
  #na1 = as.character(sort(test$Origin[test$countryName_Destination==i],partial=n)[n])
  dest[e]<- c(as.character(i))
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

dest <- as.vector(dest)
namesOr1 <- as.vector(namesOr1)
nbmigOr1 <- as.vector(nbmigOr1)
namesOr2 <- as.vector(namesOr2)
nbmigOr2 <- as.vector(nbmigOr2)
namesOr3 <- as.vector(namesOr3)
nbmigOr3 <- as.vector(nbmigOr3)
X <- cbind(dest, namesOr1,nbmigOr1,namesOr2,nbmigOr2,namesOr3,nbmigOr3)
X <- data.frame(X)

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
