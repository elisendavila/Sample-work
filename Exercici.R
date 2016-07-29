################################################################################
################################################################################
###
### Anàlisi de sessions i batalles
###
################################################################################
################################################################################

require(foreign)
require(lubridate)
require(ISOweek)
require(doBy)


setwd("C:/Users/Eli/Documents/R")


sessions<-read.table(file="sh_sessions2.csv", sep = ",",quote="\'",as.is=TRUE,skip=1,colClasses=c(rep("character",11),rep("numeric",2),rep("character",6),rep("numeric",20),"character"))
noms<-read.table( file="sh_sessions2.csv", sep = ",",quote="\'"",nrows=1)
noms<-unlist(noms)

noms<-gsub("\"","",noms)
names(sessions)<-noms
sessions<-sessions[,-1]


anetejar<-c("platform","version","ip_ip","ip_country","ip_region","ip_city","ip_timezone","client_mobile_os","client_mobile_device",
"client_mobile_language","client_mobile_uid","client_mobile_device_aid","game_resources_we")

for(j in 1:length(anetejar)){

 sessions[,anetejar[j]]<-gsub("\"","",sessions[,anetejar[j]])
}

sessions$datetime<-as.POSIXct(sessions$datetime,format="%Y-%m-%d")

sessions$user_id<-factor(sessions$user_id)

which(sessions$client_mobile_device_aid!="None")->id_device
 sessions[id_device,]->sessions_id
 clients<-data.frame(user_id=sessions_id$user_id,client_mobile_device_aid=sessions_id$client_mobile_device_aid)
clients<-unique(clients)

batalla<-read.table(file="sh_battles.csv", sep = ",",quote="\'",as.is=TRUE,skip=1,colClasses=c(rep("character",11),rep("numeric",2),"character",rep("numeric",9),rep("character",2),rep("numeric",9),rep("character",19)))

noms<-read.table( file="sh_battles.csv", sep = ",",quote="",nrows=1)
noms<-unlist(noms)

noms<-gsub("\"","",noms)
names(batalla)<-noms
batalla<-batalla[,-1]


anetejar<-c("platform_last_logged","register_platform","register_ip_ip"
,"register_ip_country","register_ip_timezone","revenues_date_first_transaction",
"revenues_date_first_transaction_android","funnel_current","funnel_5min","funnel_1hour"
,"funnel_1d" ,"user_category","register_ip_region","register_ip_city","register_os",
"register_device","register_version","register_mobile_uid","client_mobile_device_aid",
"last_global_device_id","register_device_android","register_source" ,"register_source_android"
)

for(j in 1:length(anetejar)){

 batalla[,anetejar[j]]<-gsub("\"","",batalla[,anetejar[j]])
}

batalla$date_register <-as.POSIXct(batalla$date_register,format="%Y-%m-%d")



summaryBy(datetime~user_id,data=sessions,FUN=min,keep.names=TRUE)->primera
primera[,2]<-as.POSIXct(primera[,2],origin="1970-01-01 00:00.00 UTC")

summaryBy(datetime~client_mobile_device_aid,data=sessions,FUN=min,keep.names=TRUE)->primeraDevice

primeraDevice[,2]<-as.POSIXct(primeraDevice[,2],origin="1970-01-01 00:00.00 UTC")

primera$Second<-primera[,2]+days(1)
primera$Seven<- primera[,2]+days(7)
primera$Ten<-primera[,2]+days(10)

connexions<-data.frame(user_id=primera$user_id)

connexions$Second<-0
connexions$Seven<-0
connexions$Ten<-0

####els usuaris amb id 1 no son correctes

for(j in 1:nrow(primera)){

  hies2<-length(which(sessions$user_id == primera[j,"user_id"]  & sessions$datetime == primera[j,"Second"]))
  hies7<-length(which(sessions$user_id == primera[j,"user_id"]  & sessions$datetime == primera[j,"Seven"]))
  hies10<-length(which(sessions$user_id == primera[j,"user_id"]  & sessions$datetime == primera[j,"Ten"]))

  if(hies2>0){
     connexions$Second[j]<-1
  }
    if(hies7>0){
     connexions$Seven[j]<-1
  }
    if(hies10>0){
     connexions$Ten[j]<-1
  }
  
}

connexions<-connexions[-1,]

primeraDevice

primeraDevice$Second<-primeraDevice[,2]+days(1)
primeraDevice$Seven<- primeraDevice[,2]+days(7)
primeraDevice$Ten<-primeraDevice[,2]+days(10)

connexionsDevice<-data.frame(client_mobile_device_aid=primeraDevice$client_mobile_device_aid)

connexionsDevice$Second<-0
connexionsDevice$Seven<-0
connexionsDevice$Ten<-0

####els usuaris amb id 1 no son correctes

for(j in 1:nrow(primeraDevice)){

  hies2<-length(which(sessions$client_mobile_device_aid == primeraDevice[j,"client_mobile_device_aid"]  & sessions$datetime == primeraDevice[j,"Second"]))
  hies7<-length(which(sessions$client_mobile_device_aid == primeraDevice[j,"client_mobile_device_aid"]  & sessions$datetime == primeraDevice[j,"Seven"]))
  hies10<-length(which(sessions$client_mobile_device_aid == primeraDevice[j,"client_mobile_device_aid"]  & sessions$datetime == primeraDevice[j,"Ten"]))

  if(hies2>0){
     connexionsDevice$Second[j]<-1
  }
    if(hies7>0){
     connexionsDevice$Seven[j]<-1
  }
    if(hies10>0){
     connexionsDevice$Ten[j]<-1
  }

}

connexionsDevice<-connexionsDevice[-1,]
connexionsDevice<-connexionsDevice[-nrow(connexionsDevice),]


retention<-merge(connexions,primera[,c("user_id","datetime")],by="user_id",all.x=TRUE)

retention<-retention[which(retention$datetime<=max(sessions$datetime)-days(10)),]
retencio<-summaryBy(Second+Seven+Ten~datetime,data=retention,FUN=sum,keep.names=TRUE)
count<-function(vector){

 return(length(unique(vector)))
}

summaryBy(user_id~datetime,data=retention,FUN=count,keep.names=TRUE)->usuaris
merge(usuaris,retencio,by="datetime")

retencio<- merge(usuaris,retencio,by="datetime")

retentionDevice<-merge(connexionsDevice,primeraDevice[,c("client_mobile_device_aid","datetime")],by="client_mobile_device_aid",all.x=TRUE)

retentionDevice<-retentionDevice[which(retentionDevice$datetime<=max(sessions$datetime)-days(10)),]
retencioDevice<-summaryBy(Second+Seven+Ten~datetime,data=retentionDevice,FUN=sum,keep.names=TRUE)

primera$user_id[which(day(primera$datetime) %in% 14:17)] ->user
clients[which(clients$user_id %in% user),2]->mobils
unique(mobils)->mobils

subgrup<-which(retentionDevice$client_mobile_device_aid %in% mobils)

retentionDevice<-merge(connexionsDevice,primeraDevice[,c("client_mobile_device_aid","datetime")],by="client_mobile_device_aid",all.x=TRUE)

retentionDevice<-retentionDevice[which(retentionDevice$datetime<=max(sessions$datetime)-days(10)),]
retencioDevice<-summaryBy(Second+Seven+Ten~datetime,data=retentionDevice[subgrup,],FUN=sum,keep.names=TRUE)


summaryBy(client_mobile_device_aid~datetime,data=retentionDevice[subgrup,],FUN=count,keep.names=TRUE)->usuarisDevice
retencioDevice<-merge(usuarisDevice,retencioDevice,by="datetime")

retencio$P2<-retencio$Second/retencio$user_id
retencio$P7<-retencio$Seven/retencio$user_id
retencio$P10<-retencio$Ten/retencio$user_id

retencioDevice$P2<-retencioDevice$Second/retencioDevice$client_mobile_device_aid
retencioDevice$P7<-retencioDevice$Seven/retencioDevice$client_mobile_device_aid
retencioDevice$P10<-retencioDevice$Ten/retencioDevice$client_mobile_device_aid

primera$user_id[which(day(primera$datetime) %in% 14:17)] ->usuaris
clients$user_id
connexionsDevice


merge(connexions,batalla[,c("user_id","funnel_1d")],by="user_id",all.x=TRUE)->info

retention2<-merge(info,primera[,c("user_id","datetime")],by="user_id",all.x=TRUE)
retencio2<-summaryBy(Second+Seven+Ten~datetime+tutorial,data=retention2,FUN=sum,keep.names=TRUE)
summaryBy(user_id~datetime+tutorial,data=retention2,FUN=count,keep.names=TRUE)->usuaris2
merge(usuaris2,retencio2,by=c("datetime","tutorial"))

batalla<-merge(batalla,info[c("user_id","Second")],by="user_id",all.x=TRUE)

retencio<- merge(usuaris,retencio,by="datetime")

lm(Second~funnel_1d,data=batalla)

 nothing <- glm(Second ~ 1,data=batalla,family=binomial)
 
 retencio[,c("datetime","P2","P7","P10")]  -> ret
 
ret$P2<-paste( format(ret$P2*100,digits=2,nsmall=2),"%")
ret$P7<-paste( format(ret$P7*100,digits=2,nsmall=2),"%")
ret$P10<-paste( format(ret$P10*100,digits=2,nsmall=2),"%")

retencioDevice[,c("datetime","P2","P7","P10")]  -> retD

retD$P2<-paste( format(retD$P2*100,digits=2,nsmall=2),"%")
retD$P7<-paste( format(retD$P7*100,digits=2,nsmall=2),"%")
retD$P10<-paste( format(retD$P10*100,digits=2,nsmall=2),"%")
