source("Merge_Data_Tower_CRMS_20260528.R") #uncomment to load data

###########la3
#set NA codes to NA for flux variables

la3_amf$FC[la3_amf$FC<(-9900)]<-NA
la3_amf$FCH4[la3_amf$FCH4<(-9900)]<-NA
la3_amf$LE[la3_amf$LE<(-9900)]<-NA

#get number of missing values in each day for different filters
for(i in 1:(length(la3_amf[,1])/48)){
  xx<-(i*48-47):(i*48)###assumes you start at midnight, goes 24 hours
  yy<-(i*48-35):(i*48-12)###assumes you start at midnight, goes 600 to 1800 solar time for co2
  nalfc<-sum(is.na(la3_amf$FC[xx]))
  nalfcday<-sum(is.na(la3_amf$FC[yy]))
  nalfch4<-sum(is.na(la3_amf$FCH4[xx]))
  nalle<-sum(is.na(la3_amf$LE[xx]))
  nalleday<-sum(is.na(la3_amf$LE[yy]))
  outr<-c(floor(la3_amf$TIMESTAMP_START[i*48]/10000),nalfc,nalfch4,nalfcday,nalle,nalleday)
  if(i==1) outm<-matrix(outr,1,6)
  if(i>1)outm<-rbind(outm,outr)
}

##count number of NAs in 
nnafc<-rep(NA,48)
nnafch4<-nnafc
nnafcday<-nnafc
nnale<-nnafc
nnaleday<-nnafc

for(i in 1:48) nnafc[i]<-length(which(outm[,2]<i))
for(i in 1:48) nnafch4[i]<-length(which(outm[,3]<i))
for(i in 1:24) nnafcday[i]<-length(which(outm[,4]<i))
for(i in 1:48) nnale[i]<-length(which(outm[,5]<i))
for(i in 1:24) nnaleday[i]<-length(which(outm[,6]<i))

plot(1:48,nnafc,type='o',ylim=c(0,1000))
lines(1:48,nnafch4,col=2,type='o')
lines(1:48,nnafcday,col=3,type='o')
lines(1:48,nnale,col=4,type='o')
lines(1:48,nnaleday,col=5,type='o')

for(i in 1:dim(outm)[1]){
    fcflag<-outm[i,2]<13 #threshold missing values for FC
    fcdayflag<-outm[i,2]<13&outm[i,4]<7  #make conditional on both 1/4 of total and 1/4 of daytime
    fch4flag<-outm[i,3]<25 #threshold missing values for FCH4
    leflag<-outm[i,5]<13 #threshold missing values for FC
    ledayflag<-outm[i,5]<13&outm[i,6]<7  #make conditional on both 1/4 of total and 1/4 of daytime
    
    fcmean<-NA
    if(fcflag==1&fcdayflag==1) fcmean<-mean(la3_amf$FC[(i*48-47):(i*48)],na.rm=T)
    fch4mean<-NA
    if(fch4flag==1) fch4mean<-mean(la3_amf$FCH4[(i*48-47):(i*48)],na.rm=T)
    lemean<-NA
    if(leflag==1&ledayflag==1) lemean<-mean(la3_amf$LE[(i*48-47):(i*48)],na.rm=T)
    if(i==1){  outm2<-cbind(fcflag,fch4flag,fcdayflag,leflag,ledayflag)
    output<-cbind(outm[i,1],fcmean,fch4mean,lemean)
  }else{outm2<-rbind(outm2,cbind(fcflag,fch4flag,fcdayflag,leflag,ledayflag))
  output<-rbind(output,cbind(outm[i,1],fcmean,fch4mean,lemean))
  }
}
colnames(output)<-c('date','FCmean','FCH4mean','LEmean')
output<-data.frame(output)
write.csv(output,'US_LA3_mean_flux_clean_2019_2023.csv',row.names=F)
outm3<-cbind(outm,outm2)
colnames(outm3)<-c('date','FCna','FCH4na','FCdayna','LEna','LEdayna','FCgood','FCH4good','FCdaygood','LEgood','LEdaygood')

gooddatn<-data.frame(cbind(matrix(NA,60,2),matrix(0,60,5)))
colnames(gooddatn)<-c('Year','Month','FCdays','FCH4days','FCdaydays','LEdays','LEdaydays')
gooddatn[,1]<-c(rep(2019,12),rep(2020,12),rep(2021,12),rep(2022,12),rep(2023,12))
months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
gooddatn[,2]<-rep(months,5)
dym<-as.numeric(gooddatn[,1])*100+rep(1:12,5)
for(i in 1:60){
  xx<-which(floor(outm3[,1]/100)==dym[i])
  gooddatn[i,3]<-as.numeric(sum(outm3[xx,2]))  
  gooddatn[i,4]<-as.numeric(sum(outm3[xx,3])) 
  gooddatn[i,5]<-as.numeric(sum(outm3[xx,4]))
  gooddatn[i,6]<-as.numeric(sum(outm3[xx,5])) 
  gooddatn[i,7]<-as.numeric(sum(outm3[xx,6]))
  
}
plot(as.numeric(gooddatn[,1])+rep((1:12)/12,5),gooddatn[,3],type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,4]),col=2,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,5]),col=3,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,6]),col=4,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,7]),col=5,type='o')
########la2

#limit la2 to data after 2021
la2_amf<-la2_amf[which(la2_amf[,1]>202100000000),]

#set NA codes to NA
la2_amf$FC[la2_amf$FC<(-9900)]<-NA
la2_amf$FCH4[la2_amf$FCH4<(-9900)]<-NA
la2_amf$LE[la2_amf$LE<(-9900)]<-NA


for(i in 1:(length(la2_amf[,1])/48)){
  xx<-(i*48-47):(i*48)
  nalfc<-sum(is.na(la2_amf$FC[xx]))
  nalfch4<-sum(is.na(la2_amf$FCH4[xx]))
  nalle<-sum(is.na(la2_amf$LE[xx]))
  nalleday<-sum(is.na(la2_amf$LE[yy]))
  outr<-c(floor(la2_amf$TIMESTAMP_START[i*48]/10000),nalfc,nalfch4,nalfcday,nalle,nalleday)
  if(i==1) outm<-matrix(outr,1,6)
  if(i>1)outm<-rbind(outm,outr)
}

nnafc<-rep(NA,48)
nnafch4<-nnafc
for(i in 1:48) nnafc[i]<-length(which(outm[,2]<i))
for(i in 1:48) nnafch4[i]<-length(which(outm[,3]<i))
plot(1:48,nnafc)
lines(1:48,nnafch4,col=2)

for(i in 1:dim(outm)[1]){
  fcflag<-outm[i,2]<13 #threshold missing values for FC
  fcdayflag<-outm[i,2]<13&outm[i,4]<7  #make conditional on both 1/4 of total and 1/4 of daytime
  fch4flag<-outm[i,3]<25 #threshold missing values for FCH4
  leflag<-outm[i,5]<13 #threshold missing values for FC
  ledayflag<-outm[i,5]<13&outm[i,6]<7  #make conditional on both 1/4 of total and 1/4 of daytime
  
  fcmean<-NA
  if(fcflag==1&fcdayflag==1) fcmean<-mean(la2_amf$FC[(i*48-47):(i*48)],na.rm=T)
  fch4mean<-NA
  if(fch4flag==1) fch4mean<-mean(la2_amf$FCH4[(i*48-47):(i*48)],na.rm=T)
  lemean<-NA
  if(leflag==1&ledayflag==1) lemean<-mean(la2_amf$LE[(i*48-47):(i*48)],na.rm=T)
  if(i==1){  outm2<-cbind(fcflag,fch4flag,fcdayflag,leflag,ledayflag)
  output<-cbind(outm[i,1],fcmean,fch4mean,lemean)
  }else{outm2<-rbind(outm2,cbind(fcflag,fch4flag,fcdayflag,leflag,ledayflag))
  output<-rbind(output,cbind(outm[i,1],fcmean,fch4mean,lemean))
  }
}
colnames(output)<-c('date','FCmean','FCH4mean','LEmean')
output<-data.frame(output)
write.csv(output,'US_LA2_mean_flux_clean_2021_2023.csv',row.names=F)

outm3<-cbind(outm,outm2)
colnames(outm3)<-c('date','FCna','FCH4na','FCdayna','LEna','LEdayna','FCgood','FCH4good','FCdaygood','LEgood','LEdaygood')

gooddatn<-data.frame(matrix(NA,36,2),matrix(0,36,5))
colnames(gooddatn)<-c('Year','Month','FCdays','FCH4days','FCdaydays','LEdays','LEdaydays')
gooddatn[,1]<-c(rep(2021,12),rep(2022,12),rep(2023,12))
months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
gooddatn[,2]<-rep(months,3)
dym<-as.numeric(gooddatn[,1])*100+rep(1:12,3)
for(i in 1:60){
  xx<-which(floor(outm3[,1]/100)==dym[i])
  gooddatn[i,3]<-as.numeric(sum(outm3[xx,2]))  
  gooddatn[i,4]<-as.numeric(sum(outm3[xx,3])) 
  gooddatn[i,5]<-as.numeric(sum(outm3[xx,4]))
  gooddatn[i,6]<-as.numeric(sum(outm3[xx,5])) 
  gooddatn[i,7]<-as.numeric(sum(outm3[xx,6]))
  
}
plot(as.numeric(gooddatn[,1])+rep((1:12)/12,5),gooddatn[,3],type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,4]),col=2,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,5]),col=3,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,6]),col=4,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,7]),col=5,type='o')