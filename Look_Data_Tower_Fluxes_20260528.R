#source("Merge_Data_Tower_CRMS_20260528.R") #uncomment to load data

###########la3

la3_amf$FC[la3_amf$FC<(-9900)]<-NA
la3_amf$FCH4[la3_amf$FCH4<(-9900)]<-NA
for(i in 1:(length(la3_amf[,1])/48)){
  xx<-(i*48-47):(i*48)###assumes you start at midnight, goes 24 hours
  yy<-(i*48-35):(i*48-12)###assumes you start at midnight, goes 600 to 1800 solar time for co2
  nalfc<-sum(is.na(la3_amf$FC[xx]))
  nalfcday<-sum(is.na(la3_amf$FC[yy]))
  nalfch4<-sum(is.na(la3_amf$FCH4[xx]))
  outr<-c(floor(la3_amf$TIMESTAMP_START[i*48]/10000),nalfc,nalfch4,nalfcday)
  if(i==1) outm<-matrix(outr,1,4)
  if(i>1)outm<-rbind(outm,outr)
}

nnafc<-rep(NA,48)
nnafch4<-nnafc
nnafcday<-nnafc
for(i in 1:48) nnafc[i]<-length(which(outm[,2]<i))
for(i in 1:48) nnafch4[i]<-length(which(outm[,3]<i))
for(i in 1:24) nnafcday[i]<-length(which(outm[,4]<i))
plot(1:48,nnafc,type='o',ylim=c(0,1000))
lines(1:48,nnafch4,col=2,type='o')
lines(1:48,nnafcday,col=3,type='o')

for(i in 1:dim(outm)[1]){
    fcflag<-outm[i,2]<13 #threshold missing values for FC
    fcdayflag<-outm[i,2]<13&outm[i,4]<6  #make conditional on both 1/4 of total and 1/4 of daytime
    fch4flag<-outm[i,3]<25 #threshold missing values for FCH4
    fcmean<-NA
    if(fcflag==1&fcdayflag==1) fcmean<-mean(la3_amf$FC[(i*48-47):(i*48)],na.rm=T)
    fch4mean<-NA
    if(fch4flag==1) fch4mean<-mean(la3_amf$FCH4[(i*48-47):(i*48)],na.rm=T)
    if(i==1){  outm2<-cbind(fcflag,fch4flag,fcdayflag)
    output<-cbind(outm[i,1],fcmean,fch4mean)
  }else{outm2<-rbind(outm2,cbind(fcflag,fch4flag,fcdayflag))
  output<-rbind(output,cbind(outm[i,1],fcmean,fch4mean))
  }
}

outm3<-cbind(outm,outm2)
colnames(outm3)<-c('date','FCna','FCH4na','FCdayna','FCgood','FCH4good','FCdaygood')

gooddatn<-matrix(NA,60,5)
colnames(gooddatn)<-c('Year','Month','FCdays','FCH4days','FCdaydays')
gooddatn[,1]<-c(rep(2019,12),rep(2020,12),rep(2021,12),rep(2022,12),rep(2023,12))
months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
gooddatn[,2]<-rep(months,5)
dym<-as.numeric(gooddatn[,1])*100+rep(1:12,5)
for(i in 1:60){
  xx<-which(floor(outm3[,1]/100)==dym[i])
  gooddatn[i,3]<-sum(outm3[xx,5])  
  gooddatn[i,4]<-sum(outm3[xx,6]) 
  gooddatn[i,5]<-sum(outm3[xx,7]) 
}
plot(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,3]),type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,4]),col=2,type='o')
lines(as.numeric(gooddatn[,1])+rep((1:12)/12,5),as.numeric(gooddatn[,5]),col=3,type='o')

########la2


la2_amf$FC[la2_amf$FC<(-9900)]<-NA
la2_amf$FCH4[la2_amf$FCH4<(-9900)]<-NA
for(i in 1:(length(la2_amf[,1])/48)){
  xx<-(i*48-47):(i*48)
  nalfc<-sum(is.na(la2_amf$FC[xx]))
  nalfch4<-sum(is.na(la2_amf$FCH4[xx]))
  outr<-c(floor(la2_amf$TIMESTAMP_START[i*48]/10000),nalfc,nalfch4)
  if(i==1) outm<-matrix(outr,1,3)
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
  fch4flag<-outm[i,3]<25 #threshold missing values for FCH4
  if(i==1){  outm2<-cbind(fcflag,fch4flag)
  }else{outm2<-rbind(outm2,cbind(fcflag,fch4flag))
  }
}

outm3<-cbind(outm,outm2)
colnames(outm3)<-c('date','FCna','FCH4na','FCgood','FCH4good')

gooddatn2<-matrix(NA,36,4)
colnames(gooddatn2)<-c('Year','Month','FCdays','FCH4days')
gooddatn2[,1]<-c(rep(2021,12),rep(2022,12),rep(2023,12))
gooddatn2[,2]<-rep(months,3)
dym<-as.numeric(gooddatn2[,1])*100+rep(1:12,3)
for(i in 1:36){
  xx<-which(floor(outm3[,1]/100)==dym[i])
  gooddatn2[i,3]<-sum(outm3[xx,4])  
  gooddatn2[i,4]<-sum(outm3[xx,5])  
}

plot(as.numeric(gooddatn2[,1])+rep((1:12)/12,3),as.numeric(gooddatn2[,3]),type='o')
lines(as.numeric(gooddatn2[,1])+rep((1:12)/12,3),as.numeric(gooddatn2[,4]),col=2,type='o')
