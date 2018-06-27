#------------------------------------------------------------------------------#
# Run catch monitorign simulations
# Code written by W. Atlas
#------------------------------------------------------------------------------#

### for populations of even size (harvest rate is controlled below)

mean.pop<-8.5172
pop.var<-0
n.pops<-10

pop.sizes<-round(rlnorm(n.pops,mean.pop,pop.var))
pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100 ## the 100 in the rmultinom statement controls the variance

weekly<-seq(0,7,1)

pop.timingG<-rbind(c(0,0,0,0,0,0,0,0,0,0),pop.timing,c(0,0,0,0,0,0,0,0,0,0))

par(mar=c(3,4,1,0))
for(i in 1:10){
par(new=TRUE)
plot(weekly, pop.timingG[,i],type="l",las=1,axes=FALSE,xlab="",ylab="",ylim=c(0,0.4))
}
axis(2, las=1)
axis(1)
mtext(side=2, "Proportion of run",line=2.8)
mtext(side=1, "Week",line=2.5)

run.weeks<-matrix(NA,nrow=6,ncol=n.pops)

for (i in 1:n.pops){
	run.weeks[,i]<-round(pop.timing[,i]*pop.sizes[i])
	}

week1<-c(rep(1,run.weeks[1,1]),rep(2,run.weeks[1,2]),rep(3,run.weeks[1,3]),rep(4,run.weeks[1,4]),
	rep(5,run.weeks[1,5]),rep(6,run.weeks[1,6]),rep(7,run.weeks[1,7]),rep(8,run.weeks[1,8]),rep(9,run.weeks[1,9]),
	rep(10,run.weeks[1,10]))

week2<-c(rep(1,run.weeks[2,1]),rep(2,run.weeks[2,2]),rep(3,run.weeks[2,3]),rep(4,run.weeks[2,4]),
	rep(5,run.weeks[2,5]),rep(6,run.weeks[2,6]),rep(7,run.weeks[2,7]),rep(8,run.weeks[2,8]),rep(9,run.weeks[2,9]),
	rep(10,run.weeks[2,10]))

week3<-c(rep(1,run.weeks[3,1]),rep(2,run.weeks[3,2]),rep(3,run.weeks[3,3]),rep(4,run.weeks[3,4]),
	rep(5,run.weeks[3,5]),rep(6,run.weeks[3,6]),rep(7,run.weeks[3,7]),rep(8,run.weeks[3,8]),rep(9,run.weeks[3,9]),
	rep(10,run.weeks[3,10]))

week4<-c(rep(1,run.weeks[4,1]),rep(2,run.weeks[4,2]),rep(3,run.weeks[4,3]),rep(4,run.weeks[4,4]),
	rep(5,run.weeks[4,5]),rep(6,run.weeks[4,6]),rep(7,run.weeks[4,7]),rep(8,run.weeks[4,8]),rep(9,run.weeks[4,9]),
	rep(10,run.weeks[4,10]))

week5<-c(rep(1,run.weeks[5,1]),rep(2,run.weeks[5,2]),rep(3,run.weeks[5,3]),rep(4,run.weeks[5,4]),
	rep(5,run.weeks[5,5]),rep(6,run.weeks[5,6]),rep(7,run.weeks[5,7]),rep(8,run.weeks[5,8]),rep(9,run.weeks[5,9]),
	rep(10,run.weeks[5,10]))

week6<-c(rep(1,run.weeks[6,1]),rep(2,run.weeks[6,2]),rep(3,run.weeks[6,3]),rep(4,run.weeks[6,4]),
	rep(5,run.weeks[6,5]),rep(6,run.weeks[6,6]),rep(7,run.weeks[6,7]),rep(8,run.weeks[6,8]),rep(9,run.weeks[6,9]),
	rep(10,run.weeks[6,10]))

##### harvest numbers for each week - assumes constant 10% harvest rate

harvest.rate<-0.10
catch<-rep(NA,6)

for (i in 1:6){
  catch[i]<-sum(run.weeks[i,1:10])*harvest.rate
}

### the total number of sockeye harvested


###### monitoring method 1 - one random sampling event during the middle four weeks of the run #####
#require(MultinomialCI)

### 60 fish sampled

samp.no0<-60 ### number of samples taken
samples0<-matrix(NA,nrow=1000,ncol=samp.no0)
annual.samp0<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est0<-matrix(NA,ncol=n.pops,nrow=1000)
catch_50<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates50<-matrix(NA,nrow=1000,ncol=10)
for (i in 1:1000){
  
## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  total.catch<-sum(catch.tab_all)   
  
  
## the sampling process  
  
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  #samp.no0<-50 ### number of samples taken

samples0[i,]<-sample(catch.wks[[rand.week]],samp.no0,replace=FALSE,prob=NULL)
#samples0[i,]<-sample(catch3,samp.no0,replace=FALSE,prob=NULL) # switched it so we're ONLY sampling peak week. 

#annual.samp0[i,]<-as.vector(table(samples0[i,])) #as.vector()
annual.samp0[i,1]<-length(which(samples0[i,]==1))
annual.samp0[i,2]<-length(which(samples0[i,]==2))
annual.samp0[i,3]<-length(which(samples0[i,]==3))
annual.samp0[i,4]<-length(which(samples0[i,]==4))
annual.samp0[i,5]<-length(which(samples0[i,]==5))
annual.samp0[i,6]<-length(which(samples0[i,]==6))
annual.samp0[i,7]<-length(which(samples0[i,]==7))
annual.samp0[i,8]<-length(which(samples0[i,]==8))
annual.samp0[i,9]<-length(which(samples0[i,]==9))
annual.samp0[i,10]<-length(which(samples0[i,]==10))

catch.est0[i,]<-as.vector(annual.samp0[i,]/sum(annual.samp0[i,])) ## contribution of each population in the catch

harv.rates50[i,]<-catch.tab_all/pop.sizes

catch_50[i,]<-(catch.est0[i,]*total.catch)/pop.sizes

}

head(catch_50)

catch_50_result.10<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
catch_50_result.10[i,1]<-mean(catch_50[,i])
catch_50_result.10[i,2]<-mean(catch_50[,i])-(1.96*sd(catch_50[,i]))
catch_50_result.10[i,3]<-mean(catch_50[,i])+(1.96*sd(catch_50[,i]))
catch_50_result.10[i,4]<-sd(catch_50[,i])/mean(catch_50)

}

harv.rates50

### 120 fish sampled

samp.no1<-120 ### number of samples taken
samples1<-matrix(NA,nrow=1000,ncol=samp.no1)
annual.samp1<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est1<-matrix(NA,ncol=n.pops,nrow=1000)
catch_100<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates100<-matrix(NA,nrow=1000,ncol=10)
for (i in 1:1000){

  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  total.catch<-sum(catch.tab_all)   
  
  ## the sampling process
  
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples1[i,]<-sample(catch.wks[[rand.week]],samp.no1,replace=FALSE,prob=NULL)
  #samples1[i,]<-sample(catch3,samp.no1,replace=FALSE,prob=NULL)
  
  #annual.samp1[i,]<-as.vector(table(samples1[i,])) #as.vector()
  annual.samp1[i,1]<-length(which(samples1[i,]==1))
  annual.samp1[i,2]<-length(which(samples1[i,]==2))
  annual.samp1[i,3]<-length(which(samples1[i,]==3))
  annual.samp1[i,4]<-length(which(samples1[i,]==4))
  annual.samp1[i,5]<-length(which(samples1[i,]==5))
  annual.samp1[i,6]<-length(which(samples1[i,]==6))
  annual.samp1[i,7]<-length(which(samples1[i,]==7))
  annual.samp1[i,8]<-length(which(samples1[i,]==8))
  annual.samp1[i,9]<-length(which(samples1[i,]==9))
  annual.samp1[i,10]<-length(which(samples1[i,]==10))
  
  catch.est1[i,]<-as.vector(annual.samp1[i,]/sum(annual.samp1[i,])) ## contribution of each population in the catch

  catch_100[i,]<-(catch.est1[i,]*total.catch)/pop.sizes
  
  harv.rates100[i,]<-catch.tab_all/pop.sizes

}

catch_100_result.10<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_100_result.10[i,1]<-mean(catch_100[,i])
  catch_100_result.10[i,2]<-mean(catch_100[,i])-(1.96*sd(catch_100[,i]))
  catch_100_result.10[i,3]<-mean(catch_100[,i])+(1.96*sd(catch_100[,i]))
  catch_100_result.10[i,4]<-sd(catch_100[,i])/mean(catch_100)

}

catch_100_result.10
harv.rates100

### 240 fish sampled
samp.no2<-240 ### number of samples taken
samples2<-matrix(NA,nrow=1000,ncol=samp.no2)
annual.samp2<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est2<-matrix(NA,ncol=n.pops,nrow=1000)
catch_200<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates200<-matrix(NA,nrow=1000,ncol=10)


for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)

  # the sampling process
  
  total.catch<-sum(catch.tab_all)     
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples2[i,]<-sample(catch.wks[[rand.week]],samp.no2,replace=FALSE,prob=NULL)
  #samples2[i,]<-sample(catch3,samp.no2,replace=FALSE,prob=NULL)
  
  #annual.samp1[i,]<-as.vector(table(samples1[i,])) #as.vector()
  annual.samp2[i,1]<-length(which(samples2[i,]==1))
  annual.samp2[i,2]<-length(which(samples2[i,]==2))
  annual.samp2[i,3]<-length(which(samples2[i,]==3))
  annual.samp2[i,4]<-length(which(samples2[i,]==4))
  annual.samp2[i,5]<-length(which(samples2[i,]==5))
  annual.samp2[i,6]<-length(which(samples2[i,]==6))
  annual.samp2[i,7]<-length(which(samples2[i,]==7))
  annual.samp2[i,8]<-length(which(samples2[i,]==8))
  annual.samp2[i,9]<-length(which(samples2[i,]==9))
  annual.samp2[i,10]<-length(which(samples2[i,]==10))
  
  catch.est2[i,]<-as.vector(annual.samp2[i,]/sum(annual.samp2[i,])) ## contribution of each population in the catch
  
  catch_200[i,]<-(catch.est2[i,]*total.catch)/pop.sizes
  harv.rates200[i,]<-catch.tab_all/pop.sizes
}

catch_200_result.10<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_200_result.10[i,1]<-mean(catch_200[,i])
  catch_200_result.10[i,2]<-mean(catch_200[,i])-(1.96*sd(catch_200[,i]))
  catch_200_result.10[i,3]<-mean(catch_200[,i])+(1.96*sd(catch_200[,i]))
  catch_200_result.10[i,4]<-sd(catch_200[,i])/mean(catch_200)

}

  catch_200_result.10
  harv.rates200

### 360 fish sampled

samp.no3<-360 ### number of samples taken
samples3<-matrix(NA,nrow=1000,ncol=samp.no3)
annual.samp3<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est3<-matrix(NA,ncol=n.pops,nrow=1000)
catch_300<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates300<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  # the sampling process
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples3[i,]<-sample(catch.wks[[rand.week]],samp.no3,replace=FALSE,prob=NULL)
  #samples3[i,]<-sample(catch3,samp.no3,replace=FALSE,prob=NULL)
  
  #annual.samp1[i,]<-as.vector(table(samples1[i,])) #as.vector()
  annual.samp3[i,1]<-length(which(samples3[i,]==1))
  annual.samp3[i,2]<-length(which(samples3[i,]==2))
  annual.samp3[i,3]<-length(which(samples3[i,]==3))
  annual.samp3[i,4]<-length(which(samples3[i,]==4))
  annual.samp3[i,5]<-length(which(samples3[i,]==5))
  annual.samp3[i,6]<-length(which(samples3[i,]==6))
  annual.samp3[i,7]<-length(which(samples3[i,]==7))
  annual.samp3[i,8]<-length(which(samples3[i,]==8))
  annual.samp3[i,9]<-length(which(samples3[i,]==9))
  annual.samp3[i,10]<-length(which(samples3[i,]==10))
  
  catch.est3[i,]<-as.vector(annual.samp3[i,]/sum(annual.samp3[i,])) ## contribution of each population in the catch
  
  catch_300[i,]<-(catch.est3[i,]*total.catch)/pop.sizes
  harv.rates300[i,]<-catch.tab_all/pop.sizes

}

catch_300_result.10<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_300_result.10[i,1]<-mean(catch_300[,i])
  catch_300_result.10[i,2]<-mean(catch_300[,i])-(1.96*sd(catch_300[,i]))
  catch_300_result.10[i,3]<-mean(catch_300[,i])+(1.96*sd(catch_300[,i]))
  catch_300_result.10[i,4]<-sd(catch_300[,i])/mean(catch_300)

}

catch_300_result.10
harv.rates300

### 600 fish sampled

samp.no5<-600 ### number of samples taken
samples5<-matrix(NA,nrow=1000,ncol=samp.no5)
annual.samp5<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est5<-matrix(NA,ncol=n.pops,nrow=1000)
catch_500<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates500<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  # the sampling process
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples5[i,]<-sample(catch.wks[[rand.week]],samp.no5,replace=FALSE,prob=NULL)
  #samples5[i,]<-sample(catch3,samp.no5,replace=FALSE,prob=NULL)
  
  #annual.samp1[i,]<-as.vector(table(samples1[i,])) #as.vector()
  annual.samp5[i,1]<-length(which(samples5[i,]==1))
  annual.samp5[i,2]<-length(which(samples5[i,]==2))
  annual.samp5[i,3]<-length(which(samples5[i,]==3))
  annual.samp5[i,4]<-length(which(samples5[i,]==4))
  annual.samp5[i,5]<-length(which(samples5[i,]==5))
  annual.samp5[i,6]<-length(which(samples5[i,]==6))
  annual.samp5[i,7]<-length(which(samples5[i,]==7))
  annual.samp5[i,8]<-length(which(samples5[i,]==8))
  annual.samp5[i,9]<-length(which(samples5[i,]==9))
  annual.samp5[i,10]<-length(which(samples5[i,]==10))
  
  catch.est5[i,]<-as.vector(annual.samp5[i,]/sum(annual.samp5[i,])) ## contribution of each population in the catch
  
  catch_500[i,]<-(catch.est5[i,]*total.catch)/pop.sizes

  harv.rates500[i,]<-catch.tab_all/pop.sizes
 
}

catch_500_result.10<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_500_result.10[i,1]<-mean(catch_500[,i]) # this is actually the proportion of the total catch but when all pop sizes are equal 
  catch_500_result.10[i,2]<-mean(catch_500[,i])-(1.96*sd(catch_500[,i]))
  catch_500_result.10[i,3]<-mean(catch_500[,i])+(1.96*sd(catch_500[,i]))
  catch_500_result.10[i,4]<-sd(catch_500[,i])/mean(catch_500)
}


catch_500_result.10

harv.rate_mean<-matrix(NA,nrow=5,ncol=10)

for(i in 1:10){
	harv.rate_mean[1,i]<-c(mean(harv.rates50[,i]))
	harv.rate_mean[2,i]<-c(mean(harv.rates100[,i]))
	harv.rate_mean[3,i]<-c(mean(harv.rates200[,i]))
	harv.rate_mean[4,i]<-c(mean(harv.rates300[,i]))
	harv.rate_mean[5,i]<-c(mean(harv.rates500[,i]))
}

harv.mean<-rep(NA,10)

for(i in 1:10){
	harv.mean[i]<-mean(harv.rate_mean[,i])
}

##########################################################################

##########
### for populations of variable size (harvest rate is controlled below)

#mean.pop<-8.5172
#pop.var<-1
n.pops<-10

pop.sizesV<-c(770,4327,1234,13655,17335,3633,5599,3292,4561,2910)
pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100

run.weeks<-matrix(NA,nrow=6,ncol=n.pops)

for (i in 1:n.pops){
	run.weeks[,i]<-round(pop.timing[,i]*pop.sizesV[i])
	}

week1<-c(rep(1,run.weeks[1,1]),rep(2,run.weeks[1,2]),rep(3,run.weeks[1,3]),rep(4,run.weeks[1,4]),
	rep(5,run.weeks[1,5]),rep(6,run.weeks[1,6]),rep(7,run.weeks[1,7]),rep(8,run.weeks[1,8]),rep(9,run.weeks[1,9]),
	rep(10,run.weeks[1,10]))

week2<-c(rep(1,run.weeks[2,1]),rep(2,run.weeks[2,2]),rep(3,run.weeks[2,3]),rep(4,run.weeks[2,4]),
	rep(5,run.weeks[2,5]),rep(6,run.weeks[2,6]),rep(7,run.weeks[2,7]),rep(8,run.weeks[2,8]),rep(9,run.weeks[2,9]),
	rep(10,run.weeks[2,10]))

week3<-c(rep(1,run.weeks[3,1]),rep(2,run.weeks[3,2]),rep(3,run.weeks[3,3]),rep(4,run.weeks[3,4]),
	rep(5,run.weeks[3,5]),rep(6,run.weeks[3,6]),rep(7,run.weeks[3,7]),rep(8,run.weeks[3,8]),rep(9,run.weeks[3,9]),
	rep(10,run.weeks[3,10]))

week4<-c(rep(1,run.weeks[4,1]),rep(2,run.weeks[4,2]),rep(3,run.weeks[4,3]),rep(4,run.weeks[4,4]),
	rep(5,run.weeks[4,5]),rep(6,run.weeks[4,6]),rep(7,run.weeks[4,7]),rep(8,run.weeks[4,8]),rep(9,run.weeks[4,9]),
	rep(10,run.weeks[4,10]))

week5<-c(rep(1,run.weeks[5,1]),rep(2,run.weeks[5,2]),rep(3,run.weeks[5,3]),rep(4,run.weeks[5,4]),
	rep(5,run.weeks[5,5]),rep(6,run.weeks[5,6]),rep(7,run.weeks[5,7]),rep(8,run.weeks[5,8]),rep(9,run.weeks[5,9]),
	rep(10,run.weeks[5,10]))

week6<-c(rep(1,run.weeks[6,1]),rep(2,run.weeks[6,2]),rep(3,run.weeks[6,3]),rep(4,run.weeks[6,4]),
	rep(5,run.weeks[6,5]),rep(6,run.weeks[6,6]),rep(7,run.weeks[6,7]),rep(8,run.weeks[6,8]),rep(9,run.weeks[6,9]),
	rep(10,run.weeks[6,10]))

##### harvest numbers for each week - assumes constant 10% harvest rate

harvest.rate<-0.10
catch<-rep(NA,6)

for (i in 1:6){
  catch[i]<-sum(run.weeks[i,1:10])*harvest.rate
}

### the total number of sockeye harvested


###### monitoring method 1 - one random sampling event during the middle four weeks of the run #####
#require(MultinomialCI)

### 60 fish sampled

samp.no0<-60 ### number of samples taken
samples0<-matrix(NA,nrow=1000,ncol=samp.no0)
annual.samp0<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est0<-matrix(NA,ncol=n.pops,nrow=1000)
catch_50<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates50v<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){
  
## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  total.catch<-sum(catch.tab_all)   
 
## the sampling process  
  
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))

samples0[i,]<-sample(catch.wks[[rand.week]],samp.no0,replace=FALSE,prob=NULL)
#samples0[i,]<-sample(catch3,samp.no0,replace=FALSE,prob=NULL)


#annual.samp0[i,]<-as.vector(table(samples0[i,])) #as.vector()
annual.samp0[i,1]<-length(which(samples0[i,]==1))
annual.samp0[i,2]<-length(which(samples0[i,]==2))
annual.samp0[i,3]<-length(which(samples0[i,]==3))
annual.samp0[i,4]<-length(which(samples0[i,]==4))
annual.samp0[i,5]<-length(which(samples0[i,]==5))
annual.samp0[i,6]<-length(which(samples0[i,]==6))
annual.samp0[i,7]<-length(which(samples0[i,]==7))
annual.samp0[i,8]<-length(which(samples0[i,]==8))
annual.samp0[i,9]<-length(which(samples0[i,]==9))
annual.samp0[i,10]<-length(which(samples0[i,]==10))

catch.est0[i,]<-as.vector(annual.samp0[i,]/sum(annual.samp0[i,])) ## contribution of each population in the catch

catch_50[i,]<-(catch.est0[i,]*total.catch)/pop.sizesV
harv.rates50v[i,]<-catch.tab_all/pop.sizesV

}

head(catch_50)

catch_50_result.10v<-matrix(NA,nrow=n.pops,ncol=4)
for(i in 1:10){
catch_50_result.10v[i,1]<-mean(catch_50[,i])
catch_50_result.10v[i,2]<-mean(catch_50[,i])-(1.96*sd(catch_50[,i]))
catch_50_result.10v[i,3]<-mean(catch_50[,i])+(1.96*sd(catch_50[,i]))
catch_50_result.10v[i,4]<-sd(catch_50[,i])/mean(catch_50[,i])

}

catch_50_result.10v

### 120 fish sampled

samp.no1<-120 ### number of samples taken
samples1<-matrix(NA,nrow=1000,ncol=samp.no1)
annual.samp1<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est1<-matrix(NA,ncol=n.pops,nrow=1000)
catch_100<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates100v<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){

  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  total.catch<-sum(catch.tab_all)   
  
  ## the sampling process
  
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples1[i,]<-sample(catch.wks[[rand.week]],samp.no1,replace=FALSE,prob=NULL)
  #samples1[i,]<-sample(catch3,samp.no1,replace=FALSE,prob=NULL)

  annual.samp1[i,1]<-length(which(samples1[i,]==1))
  annual.samp1[i,2]<-length(which(samples1[i,]==2))
  annual.samp1[i,3]<-length(which(samples1[i,]==3))
  annual.samp1[i,4]<-length(which(samples1[i,]==4))
  annual.samp1[i,5]<-length(which(samples1[i,]==5))
  annual.samp1[i,6]<-length(which(samples1[i,]==6))
  annual.samp1[i,7]<-length(which(samples1[i,]==7))
  annual.samp1[i,8]<-length(which(samples1[i,]==8))
  annual.samp1[i,9]<-length(which(samples1[i,]==9))
  annual.samp1[i,10]<-length(which(samples1[i,]==10))
  
  catch.est1[i,]<-as.vector(annual.samp1[i,]/sum(annual.samp1[i,])) ## contribution of each population in the catch

  catch_100[i,]<-(catch.est1[i,]*total.catch)/pop.sizesV
  harv.rates100v[i,]<-catch.tab_all/pop.sizesV

}

catch_100_result.10v<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_100_result.10v[i,1]<-mean(catch_100[,i])
  catch_100_result.10v[i,2]<-mean(catch_100[,i])-(1.96*sd(catch_100[,i]))
  catch_100_result.10v[i,3]<-mean(catch_100[,i])+(1.96*sd(catch_100[,i]))
  catch_100_result.10v[i,4]<-sd(catch_100[,i])/mean(catch_100[,i])

}

catch_100_result.10v

### 240 fish sampled
samp.no2<-240 ### number of samples taken
samples2<-matrix(NA,nrow=1000,ncol=samp.no2)
annual.samp2<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est2<-matrix(NA,ncol=n.pops,nrow=1000)
catch_200<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates200v<-matrix(NA,nrow=1000,ncol=10)


for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)

  # the sampling process
  
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples2[i,]<-sample(catch.wks[[rand.week]],samp.no2,replace=FALSE,prob=NULL)
  #samples2[i,]<-sample(catch3,samp.no2,replace=FALSE,prob=NULL)
  
  annual.samp2[i,1]<-length(which(samples2[i,]==1))
  annual.samp2[i,2]<-length(which(samples2[i,]==2))
  annual.samp2[i,3]<-length(which(samples2[i,]==3))
  annual.samp2[i,4]<-length(which(samples2[i,]==4))
  annual.samp2[i,5]<-length(which(samples2[i,]==5))
  annual.samp2[i,6]<-length(which(samples2[i,]==6))
  annual.samp2[i,7]<-length(which(samples2[i,]==7))
  annual.samp2[i,8]<-length(which(samples2[i,]==8))
  annual.samp2[i,9]<-length(which(samples2[i,]==9))
  annual.samp2[i,10]<-length(which(samples2[i,]==10))
  
  catch.est2[i,]<-as.vector(annual.samp2[i,]/sum(annual.samp2[i,])) ## contribution of each population in the catch
  
  catch_200[i,]<-(catch.est2[i,]*total.catch)/pop.sizesV
  harv.rates200v[i,]<-catch.tab_all/pop.sizesV

}

catch_200_result.10v<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_200_result.10v[i,1]<-mean(catch_200[,i])
  catch_200_result.10v[i,2]<-mean(catch_200[,i])-(1.96*sd(catch_200[,i]))
  catch_200_result.10v[i,3]<-mean(catch_200[,i])+(1.96*sd(catch_200[,i]))
  catch_200_result.10v[i,4]<-sd(catch_200[,i])/mean(catch_200[,i])

}

  catch_200_result.10v

### 360 fish sampled

samp.no3<-360 ### number of samples taken
samples3<-matrix(NA,nrow=1000,ncol=samp.no3)
annual.samp3<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est3<-matrix(NA,ncol=n.pops,nrow=1000)
catch_300<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates300v<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  # the sampling process
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples3[i,]<-sample(catch.wks[[rand.week]],samp.no3,replace=FALSE,prob=NULL)
  #samples3[i,]<-sample(catch3,samp.no3,replace=FALSE,prob=NULL)

  annual.samp3[i,1]<-length(which(samples3[i,]==1))
  annual.samp3[i,2]<-length(which(samples3[i,]==2))
  annual.samp3[i,3]<-length(which(samples3[i,]==3))
  annual.samp3[i,4]<-length(which(samples3[i,]==4))
  annual.samp3[i,5]<-length(which(samples3[i,]==5))
  annual.samp3[i,6]<-length(which(samples3[i,]==6))
  annual.samp3[i,7]<-length(which(samples3[i,]==7))
  annual.samp3[i,8]<-length(which(samples3[i,]==8))
  annual.samp3[i,9]<-length(which(samples3[i,]==9))
  annual.samp3[i,10]<-length(which(samples3[i,]==10))
  
  catch.est3[i,]<-as.vector(annual.samp3[i,]/sum(annual.samp3[i,])) ## contribution of each population in the catch
  
  catch_300[i,]<-(catch.est3[i,]*total.catch)/pop.sizesV
  harv.rates300v[i,]<-catch.tab_all/pop.sizesV

}

catch_300_result.10v<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_300_result.10v[i,1]<-mean(catch_300[,i])
  catch_300_result.10v[i,2]<-mean(catch_300[,i])-(1.96*sd(catch_300[,i]))
  catch_300_result.10v[i,3]<-mean(catch_300[,i])+(1.96*sd(catch_300[,i]))
  catch_300_result.10v[i,4]<-sd(catch_300[,i])/mean(catch_300[,i])

}

catch_300_result.10v

### 600 fish sampled

samp.no5<-600 ### number of samples taken
samples5<-matrix(NA,nrow=1000,ncol=samp.no5)
annual.samp5<-matrix(NA,nrow=1000,ncol=n.pops)
catch.est5<-matrix(NA,ncol=n.pops,nrow=1000)
catch_500<-matrix(NA,ncol=n.pops,nrow=1000)
harv.rates500v<-matrix(NA,nrow=1000,ncol=10)

for (i in 1:1000){
  ## the catch process  
  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)
  
  # the sampling process
  catch.wks<-list(catch2,catch3,catch4)
  rand.week<-round(runif(1,1,3))
  
  samples5[i,]<-sample(catch.wks[[rand.week]],samp.no5,replace=FALSE,prob=NULL)
  #samples5[i,]<-sample(catch3,samp.no5,replace=FALSE,prob=NULL)
  
  annual.samp5[i,1]<-length(which(samples5[i,]==1))
  annual.samp5[i,2]<-length(which(samples5[i,]==2))
  annual.samp5[i,3]<-length(which(samples5[i,]==3))
  annual.samp5[i,4]<-length(which(samples5[i,]==4))
  annual.samp5[i,5]<-length(which(samples5[i,]==5))
  annual.samp5[i,6]<-length(which(samples5[i,]==6))
  annual.samp5[i,7]<-length(which(samples5[i,]==7))
  annual.samp5[i,8]<-length(which(samples5[i,]==8))
  annual.samp5[i,9]<-length(which(samples5[i,]==9))
  annual.samp5[i,10]<-length(which(samples5[i,]==10))
  
  catch.est5[i,]<-as.vector(annual.samp5[i,]/sum(annual.samp5[i,])) ## contribution of each population in the catch
  
  catch_500[i,]<-(catch.est5[i,]*total.catch)/pop.sizesV
  harv.rates500v[i,]<-catch.tab_all/pop.sizesV

}

catch_500_result.10v<-matrix(NA,nrow=n.pops,ncol=4)

for(i in 1:10){
  catch_500_result.10v[i,1]<-mean(catch_500[,i])
  catch_500_result.10v[i,2]<-mean(catch_500[,i])-(1.96*sd(catch_500[,i]))
  catch_500_result.10v[i,3]<-mean(catch_500[,i])+(1.96*sd(catch_500[,i]))
  catch_500_result.10v[i,4]<-sd(catch_500[,i])/mean(catch_500[,i])
}

catch_500_result.10v

harv.rateV_mean<-matrix(NA,nrow=5,ncol=10)

for(i in 1:10){
	harv.rateV_mean[1,i]<-c(mean(harv.rates50v[,i]))
	harv.rateV_mean[2,i]<-c(mean(harv.rates100v[,i]))
	harv.rateV_mean[3,i]<-c(mean(harv.rates200v[,i]))
	harv.rateV_mean[4,i]<-c(mean(harv.rates300v[,i]))
	harv.rateV_mean[5,i]<-c(mean(harv.rates500v[,i]))
}

harv.meanV<-rep(NA,10)

for(i in 1:10){
	harv.meanV[i]<-mean(harv.rateV_mean[,i])
}

###### Making standardized mean respose plots

catch_CIs<-matrix(NA,ncol=4,nrow=5) # mean, total CI width, lci, uci

#60 samples
catch_CIs[1,1]<-mean(catch_50_result.10[,1]/catch_50_result.10[,1])
catch_CIs[1,2]<-mean((catch_50_result.10[,3]-catch_50_result.10[,2])/mean(catch_50_result.10[,1]))
catch_CIs[1,3]<-catch_CIs[1,1]-(0.5*catch_CIs[1,2])
catch_CIs[1,4]<-catch_CIs[1,1]+(0.5*catch_CIs[1,2])
#120 samples
catch_CIs[2,1]<-mean(catch_100_result.10[,1]/catch_100_result.10[,1])
catch_CIs[2,2]<-mean((catch_100_result.10[,3]-catch_100_result.10[,2])/mean(catch_100_result.10[,1]))
catch_CIs[2,3]<-catch_CIs[2,1]-(0.5*catch_CIs[2,2])
catch_CIs[2,4]<-catch_CIs[2,1]+(0.5*catch_CIs[2,2])
#240 samples
catch_CIs[3,1]<-mean(catch_200_result.10[,1]/catch_200_result.10[,1])
catch_CIs[3,2]<-mean((catch_200_result.10[,3]-catch_200_result.10[,2])/mean(catch_200_result.10[,1]))
catch_CIs[3,3]<-catch_CIs[3,1]-(0.5*catch_CIs[3,2])
catch_CIs[3,4]<-catch_CIs[3,1]+(0.5*catch_CIs[3,2])
#360 samples
catch_CIs[4,1]<-mean(catch_300_result.10[,1]/catch_300_result.10[,1])
catch_CIs[4,2]<-mean((catch_300_result.10[,3]-catch_300_result.10[,2])/mean(catch_300_result.10[,1]))
catch_CIs[4,3]<-catch_CIs[4,1]-(0.5*catch_CIs[4,2])
catch_CIs[4,4]<-catch_CIs[4,1]+(0.5*catch_CIs[4,2])
#600 samples
catch_CIs[5,1]<-mean(catch_500_result.10[,1]/catch_500_result.10[,1])
catch_CIs[5,2]<-mean((catch_500_result.10[,3]-catch_500_result.10[,2])/mean(catch_500_result.10[,1]))
catch_CIs[5,3]<-catch_CIs[5,1]-(0.5*catch_CIs[5,2])
catch_CIs[5,4]<-catch_CIs[5,1]+(0.5*catch_CIs[5,2])

catch_CIs.V<-matrix(NA,ncol=4,nrow=5)

#60 samples
catch_CIs.V[1,1]<-mean(catch_50_result.10v[,1]/catch_50_result.10v[,1])
catch_CIs.V[1,2]<-mean((catch_50_result.10v[,3]-catch_50_result.10v[,2])/mean(catch_50_result.10v[,1]))
catch_CIs.V[1,3]<-catch_CIs.V[1,1]-(0.5*catch_CIs.V[1,2])
catch_CIs.V[1,4]<-catch_CIs.V[1,1]+(0.5*catch_CIs.V[1,2])
#120 samples
catch_CIs.V[2,1]<-mean(catch_100_result.10v[,1]/catch_100_result.10v[,1])
catch_CIs.V[2,2]<-mean((catch_100_result.10v[,3]-catch_100_result.10v[,2])/mean(catch_100_result.10v[,1]))
catch_CIs.V[2,3]<-catch_CIs.V[2,1]-(0.5*catch_CIs.V[2,2])
catch_CIs.V[2,4]<-catch_CIs.V[2,1]+(0.5*catch_CIs.V[2,2])
#240 samples
catch_CIs.V[3,1]<-mean(catch_200_result.10v[,1]/catch_200_result.10v[,1])
catch_CIs.V[3,2]<-mean((catch_200_result.10v[,3]-catch_200_result.10v[,2])/mean(catch_200_result.10v[,1]))
catch_CIs.V[3,3]<-catch_CIs.V[3,1]-(0.5*catch_CIs.V[3,2])
catch_CIs.V[3,4]<-catch_CIs.V[3,1]+(0.5*catch_CIs.V[3,2])
#360 samples
catch_CIs.V[4,1]<-mean(catch_300_result.10v[,1]/catch_300_result.10v[,1])
catch_CIs.V[4,2]<-mean((catch_300_result.10v[,3]-catch_300_result.10v[,2])/mean(catch_300_result.10v[,1]))
catch_CIs.V[4,3]<-catch_CIs.V[4,1]-(0.5*catch_CIs.V[4,2])
catch_CIs.V[4,4]<-catch_CIs.V[4,1]+(0.5*catch_CIs.V[4,2])
#600 samples
catch_CIs.V[5,1]<-mean(catch_500_result.10v[,1]/catch_500_result.10v[,1])
catch_CIs.V[5,2]<-mean((catch_500_result.10v[,3]-catch_500_result.10v[,2])/mean(catch_500_result.10v[,1]))
catch_CIs.V[5,3]<-catch_CIs.V[5,1]-(0.5*catch_CIs.V[5,2])
catch_CIs.V[5,4]<-catch_CIs.V[5,1]+(0.5*catch_CIs.V[5,2])


#### Figure 2: estimating and plotting CV for estimates v. sample number

mean.10<-rep(NA,5)
mean.10[1]<-mean(catch_50_result.10[,4])
mean.10[2]<-mean(catch_100_result.10[,4])
mean.10[3]<-mean(catch_200_result.10[,4])
mean.10[4]<-mean(catch_300_result.10[,4])
mean.10[5]<-mean(catch_500_result.10[,4])

mean.10v<-rep(NA,5)
mean.10v[1]<-mean(catch_50_result.10v[,4])
mean.10v[2]<-mean(catch_100_result.10v[,4])
mean.10v[3]<-mean(catch_200_result.10v[,4])
mean.10v[4]<-mean(catch_300_result.10v[,4])
mean.10v[5]<-mean(catch_500_result.10v[,4])


##########################################################################
######    Weekly sampling

##########
### for populations of constant size (harvest rate is controlled below)

mean.pop<-8.5172
pop.var<-0 #### need to change this before running the code below
n.pops<-10

pop.sizes<-round(rlnorm(n.pops,mean.pop,pop.var))
pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100

run.weeks<-matrix(NA,nrow=6,ncol=n.pops)

for (i in 1:n.pops){
	run.weeks[,i]<-round(pop.timing[,i]*pop.sizes[i])
	}

week1<-c(rep(1,run.weeks[1,1]),rep(2,run.weeks[1,2]),rep(3,run.weeks[1,3]),rep(4,run.weeks[1,4]),
	rep(5,run.weeks[1,5]),rep(6,run.weeks[1,6]),rep(7,run.weeks[1,7]),rep(8,run.weeks[1,8]),rep(9,run.weeks[1,9]),
	rep(10,run.weeks[1,10]))

week2<-c(rep(1,run.weeks[2,1]),rep(2,run.weeks[2,2]),rep(3,run.weeks[2,3]),rep(4,run.weeks[2,4]),
	rep(5,run.weeks[2,5]),rep(6,run.weeks[2,6]),rep(7,run.weeks[2,7]),rep(8,run.weeks[2,8]),rep(9,run.weeks[2,9]),
	rep(10,run.weeks[2,10]))

week3<-c(rep(1,run.weeks[3,1]),rep(2,run.weeks[3,2]),rep(3,run.weeks[3,3]),rep(4,run.weeks[3,4]),
	rep(5,run.weeks[3,5]),rep(6,run.weeks[3,6]),rep(7,run.weeks[3,7]),rep(8,run.weeks[3,8]),rep(9,run.weeks[3,9]),
	rep(10,run.weeks[3,10]))

week4<-c(rep(1,run.weeks[4,1]),rep(2,run.weeks[4,2]),rep(3,run.weeks[4,3]),rep(4,run.weeks[4,4]),
	rep(5,run.weeks[4,5]),rep(6,run.weeks[4,6]),rep(7,run.weeks[4,7]),rep(8,run.weeks[4,8]),rep(9,run.weeks[4,9]),
	rep(10,run.weeks[4,10]))

week5<-c(rep(1,run.weeks[5,1]),rep(2,run.weeks[5,2]),rep(3,run.weeks[5,3]),rep(4,run.weeks[5,4]),
	rep(5,run.weeks[5,5]),rep(6,run.weeks[5,6]),rep(7,run.weeks[5,7]),rep(8,run.weeks[5,8]),rep(9,run.weeks[5,9]),
	rep(10,run.weeks[5,10]))

week6<-c(rep(1,run.weeks[6,1]),rep(2,run.weeks[6,2]),rep(3,run.weeks[6,3]),rep(4,run.weeks[6,4]),
	rep(5,run.weeks[6,5]),rep(6,run.weeks[6,6]),rep(7,run.weeks[6,7]),rep(8,run.weeks[6,8]),rep(9,run.weeks[6,9]),
	rep(10,run.weeks[6,10]))

##### harvest numbers for each week - assumes constant 10% harvest rate

harvest.rate<-0.10
catch<-rep(NA,6)

for (i in 1:6){
  catch[i]<-sum(run.weeks[i,1:10])*harvest.rate
}

### the total number of sockeye harvested

####### weekly sampling ########

weeks<-6
n.pops<-10

  ## the catch process  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)


  
#### sampling from the catch for each week
sample_10<-array(NA, dim=c(10,6,1000))
sample_10.all<-matrix(NA,nrow=1000,ncol=60)
sample_10.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est10<-matrix(nrow=1000,ncol=n.pops)


## 10 fish per week
for (i in 1:1000){
  sample_10[,1,i]<-sample(catch1,10,replace=FALSE,prob=NULL)
  sample_10[,2,i]<-sample(catch2,10,replace=FALSE,prob=NULL)
  sample_10[,3,i]<-sample(catch3,10,replace=FALSE,prob=NULL)
  sample_10[,4,i]<-sample(catch4,10,replace=FALSE,prob=NULL)
  sample_10[,5,i]<-sample(catch5,10,replace=FALSE,prob=NULL)
  sample_10[,6,i]<-sample(catch6,10,replace=FALSE,prob=NULL)

	sample_10.all[i,]<-c(sample_10[,1,i],sample_10[,2,i],sample_10[,3,i],sample_10[,4,i],sample_10[,5,i],sample_10[,6,i])

  sample_10.tab[i,1]<-length(which(sample_10.all[i,]==1))
  sample_10.tab[i,2]<-length(which(sample_10.all[i,]==2))
  sample_10.tab[i,3]<-length(which(sample_10.all[i,]==3))
  sample_10.tab[i,4]<-length(which(sample_10.all[i,]==4))
  sample_10.tab[i,5]<-length(which(sample_10.all[i,]==5))
  sample_10.tab[i,6]<-length(which(sample_10.all[i,]==6))
  sample_10.tab[i,7]<-length(which(sample_10.all[i,]==7))
  sample_10.tab[i,8]<-length(which(sample_10.all[i,]==8))
  sample_10.tab[i,9]<-length(which(sample_10.all[i,]==9))
  sample_10.tab[i,10]<-length(which(sample_10.all[i,]==10))

	catch.est10[i,]<-as.vector(sample_10.tab[i,]/sum(sample_10.tab[i,]))
}

dim(catch.est10)

catch_10w<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_10w[i,1]<-mean(catch.est10[,i])
catch_10w[i,2]<-mean(catch.est10[,i])-(1.96*sd(catch.est10[,i]))
catch_10w[i,3]<-mean(catch.est10[,i])+(1.96*sd(catch.est10[,i]))
catch_10w[i,4]<-sd(catch.est10[,i])/mean(catch.est10[,i])
}

## 20 fish per week

sample_20<-array(NA, dim=c(20,6,1000))
sample_20.all<-matrix(NA,nrow=1000,ncol=120)
sample_20.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est20<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_20[,1,i]<-sample(catch1,20,replace=FALSE,prob=NULL)
  sample_20[,2,i]<-sample(catch2,20,replace=FALSE,prob=NULL)
  sample_20[,3,i]<-sample(catch3,20,replace=FALSE,prob=NULL)
  sample_20[,4,i]<-sample(catch4,20,replace=FALSE,prob=NULL)
  sample_20[,5,i]<-sample(catch5,20,replace=FALSE,prob=NULL)
  sample_20[,6,i]<-sample(catch6,20,replace=FALSE,prob=NULL)

	sample_20.all[i,]<-c(sample_20[,1,i],sample_20[,2,i],sample_20[,3,i],sample_20[,4,i],sample_20[,5,i],sample_20[,6,i])

  sample_20.tab[i,1]<-length(which(sample_20.all[i,]==1))
  sample_20.tab[i,2]<-length(which(sample_20.all[i,]==2))
  sample_20.tab[i,3]<-length(which(sample_20.all[i,]==3))
  sample_20.tab[i,4]<-length(which(sample_20.all[i,]==4))
  sample_20.tab[i,5]<-length(which(sample_20.all[i,]==5))
  sample_20.tab[i,6]<-length(which(sample_20.all[i,]==6))
  sample_20.tab[i,7]<-length(which(sample_20.all[i,]==7))
  sample_20.tab[i,8]<-length(which(sample_20.all[i,]==8))
  sample_20.tab[i,9]<-length(which(sample_20.all[i,]==9))
  sample_20.tab[i,10]<-length(which(sample_20.all[i,]==10))

	catch.est20[i,]<-as.vector(sample_20.tab[i,]/sum(sample_20.tab[i,]))
}

catch_20w<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_20w[i,1]<-mean(catch.est20[,i])
catch_20w[i,2]<-mean(catch.est20[,i])-(1.96*sd(catch.est20[,i]))
catch_20w[i,3]<-mean(catch.est20[,i])+(1.96*sd(catch.est20[,i]))
catch_20w[i,4]<-sd(catch.est20[,i])/mean(catch.est20[,i])
}

## 40 fish per week

sample_40<-array(NA, dim=c(40,6,1000))
sample_40.all<-matrix(NA,nrow=1000,ncol=240)
sample_40.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est40<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_40[,1,i]<-sample(catch1,40,replace=FALSE,prob=NULL)
  sample_40[,2,i]<-sample(catch2,40,replace=FALSE,prob=NULL)
  sample_40[,3,i]<-sample(catch3,40,replace=FALSE,prob=NULL)
  sample_40[,4,i]<-sample(catch4,40,replace=FALSE,prob=NULL)
  sample_40[,5,i]<-sample(catch5,40,replace=FALSE,prob=NULL)
  sample_40[,6,i]<-sample(catch6,40,replace=FALSE,prob=NULL)

	sample_40.all[i,]<-c(sample_40[,1,i],sample_40[,2,i],sample_40[,3,i],sample_40[,4,i],sample_40[,5,i],sample_40[,6,i])

  sample_40.tab[i,1]<-length(which(sample_40.all[i,]==1))
  sample_40.tab[i,2]<-length(which(sample_40.all[i,]==2))
  sample_40.tab[i,3]<-length(which(sample_40.all[i,]==3))
  sample_40.tab[i,4]<-length(which(sample_40.all[i,]==4))
  sample_40.tab[i,5]<-length(which(sample_40.all[i,]==5))
  sample_40.tab[i,6]<-length(which(sample_40.all[i,]==6))
  sample_40.tab[i,7]<-length(which(sample_40.all[i,]==7))
  sample_40.tab[i,8]<-length(which(sample_40.all[i,]==8))
  sample_40.tab[i,9]<-length(which(sample_40.all[i,]==9))
  sample_40.tab[i,10]<-length(which(sample_40.all[i,]==10))

	catch.est40[i,]<-as.vector(sample_40.tab[i,]/sum(sample_40.tab[i,]))
}

catch_40w<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_40w[i,1]<-mean(catch.est40[,i])
catch_40w[i,2]<-mean(catch.est40[,i])-(1.96*sd(catch.est40[,i]))
catch_40w[i,3]<-mean(catch.est40[,i])+(1.96*sd(catch.est40[,i]))
catch_40w[i,4]<-sd(catch.est40[,i])/mean(catch.est40[,i])
}

## 60 fish per week


sample_60<-array(NA, dim=c(60,6,1000))
sample_60.all<-matrix(NA,nrow=1000,ncol=360)
sample_60.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est60<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_60[,1,i]<-sample(catch1,60,replace=FALSE,prob=NULL)
  sample_60[,2,i]<-sample(catch2,60,replace=FALSE,prob=NULL)
  sample_60[,3,i]<-sample(catch3,60,replace=FALSE,prob=NULL)
  sample_60[,4,i]<-sample(catch4,60,replace=FALSE,prob=NULL)
  sample_60[,5,i]<-sample(catch5,60,replace=FALSE,prob=NULL)
  sample_60[,6,i]<-sample(catch6,60,replace=FALSE,prob=NULL)

	sample_60.all[i,]<-c(sample_60[,1,i],sample_60[,2,i],sample_60[,3,i],sample_60[,4,i],sample_60[,5,i],sample_60[,6,i])

  sample_60.tab[i,1]<-length(which(sample_60.all[i,]==1))
  sample_60.tab[i,2]<-length(which(sample_60.all[i,]==2))
  sample_60.tab[i,3]<-length(which(sample_60.all[i,]==3))
  sample_60.tab[i,4]<-length(which(sample_60.all[i,]==4))
  sample_60.tab[i,5]<-length(which(sample_60.all[i,]==5))
  sample_60.tab[i,6]<-length(which(sample_60.all[i,]==6))
  sample_60.tab[i,7]<-length(which(sample_60.all[i,]==7))
  sample_60.tab[i,8]<-length(which(sample_60.all[i,]==8))
  sample_60.tab[i,9]<-length(which(sample_60.all[i,]==9))
  sample_60.tab[i,10]<-length(which(sample_60.all[i,]==10))

	catch.est60[i,]<-as.vector(sample_60.tab[i,]/sum(sample_60.tab[i,]))
}

catch_60w<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_60w[i,1]<-mean(catch.est60[,i])
catch_60w[i,2]<-mean(catch.est60[,i])-(1.96*sd(catch.est60[,i]))
catch_60w[i,3]<-mean(catch.est60[,i])+(1.96*sd(catch.est60[,i]))
catch_60w[i,4]<-sd(catch.est60[,i])/mean(catch.est60[,i])
}

## 100 fish per week

sample_100<-array(NA, dim=c(100,6,1000))
sample_100.all<-matrix(NA,nrow=1000,ncol=600)
sample_100.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est100<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_100[,1,i]<-sample(catch1,100,replace=FALSE,prob=NULL)
  sample_100[,2,i]<-sample(catch2,100,replace=FALSE,prob=NULL)
  sample_100[,3,i]<-sample(catch3,100,replace=FALSE,prob=NULL)
  sample_100[,4,i]<-sample(catch4,100,replace=FALSE,prob=NULL)
  sample_100[,5,i]<-sample(catch5,100,replace=FALSE,prob=NULL)
  sample_100[,6,i]<-sample(catch6,100,replace=FALSE,prob=NULL)

	sample_100.all[i,]<-c(sample_100[,1,i],sample_100[,2,i],sample_100[,3,i],sample_100[,4,i],sample_100[,5,i],sample_100[,6,i])

  sample_100.tab[i,1]<-length(which(sample_100.all[i,]==1))
  sample_100.tab[i,2]<-length(which(sample_100.all[i,]==2))
  sample_100.tab[i,3]<-length(which(sample_100.all[i,]==3))
  sample_100.tab[i,4]<-length(which(sample_100.all[i,]==4))
  sample_100.tab[i,5]<-length(which(sample_100.all[i,]==5))
  sample_100.tab[i,6]<-length(which(sample_100.all[i,]==6))
  sample_100.tab[i,7]<-length(which(sample_100.all[i,]==7))
  sample_100.tab[i,8]<-length(which(sample_100.all[i,]==8))
  sample_100.tab[i,9]<-length(which(sample_100.all[i,]==9))
  sample_100.tab[i,10]<-length(which(sample_100.all[i,]==10))

	catch.est100[i,]<-as.vector(sample_100.tab[i,]/sum(sample_100.tab[i,]))
}

catch_100w<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_100w[i,1]<-mean(catch.est100[,i])
catch_100w[i,2]<-mean(catch.est100[,i])-(1.96*sd(catch.est100[,i]))
catch_100w[i,3]<-mean(catch.est100[,i])+(1.96*sd(catch.est100[,i]))
catch_100w[i,4]<-sd(catch.est100[,i])/mean(catch.est100[,i])
}


##########
### for populations of variable size (harvest rate is controlled below)

mean.pop<-8.5172
pop.var<-1 #### need to change this before running the code below
n.pops<-10

pop.sizes<-c(770,4327,1234,13655,17335,3633,5599,3292,4561,2910)
pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100

run.weeks<-matrix(NA,nrow=6,ncol=n.pops)

for (i in 1:n.pops){
	run.weeks[,i]<-round(pop.timing[,i]*pop.sizes[i])
	}

week1<-c(rep(1,run.weeks[1,1]),rep(2,run.weeks[1,2]),rep(3,run.weeks[1,3]),rep(4,run.weeks[1,4]),
	rep(5,run.weeks[1,5]),rep(6,run.weeks[1,6]),rep(7,run.weeks[1,7]),rep(8,run.weeks[1,8]),rep(9,run.weeks[1,9]),
	rep(10,run.weeks[1,10]))

week2<-c(rep(1,run.weeks[2,1]),rep(2,run.weeks[2,2]),rep(3,run.weeks[2,3]),rep(4,run.weeks[2,4]),
	rep(5,run.weeks[2,5]),rep(6,run.weeks[2,6]),rep(7,run.weeks[2,7]),rep(8,run.weeks[2,8]),rep(9,run.weeks[2,9]),
	rep(10,run.weeks[2,10]))

week3<-c(rep(1,run.weeks[3,1]),rep(2,run.weeks[3,2]),rep(3,run.weeks[3,3]),rep(4,run.weeks[3,4]),
	rep(5,run.weeks[3,5]),rep(6,run.weeks[3,6]),rep(7,run.weeks[3,7]),rep(8,run.weeks[3,8]),rep(9,run.weeks[3,9]),
	rep(10,run.weeks[3,10]))

week4<-c(rep(1,run.weeks[4,1]),rep(2,run.weeks[4,2]),rep(3,run.weeks[4,3]),rep(4,run.weeks[4,4]),
	rep(5,run.weeks[4,5]),rep(6,run.weeks[4,6]),rep(7,run.weeks[4,7]),rep(8,run.weeks[4,8]),rep(9,run.weeks[4,9]),
	rep(10,run.weeks[4,10]))

week5<-c(rep(1,run.weeks[5,1]),rep(2,run.weeks[5,2]),rep(3,run.weeks[5,3]),rep(4,run.weeks[5,4]),
	rep(5,run.weeks[5,5]),rep(6,run.weeks[5,6]),rep(7,run.weeks[5,7]),rep(8,run.weeks[5,8]),rep(9,run.weeks[5,9]),
	rep(10,run.weeks[5,10]))

week6<-c(rep(1,run.weeks[6,1]),rep(2,run.weeks[6,2]),rep(3,run.weeks[6,3]),rep(4,run.weeks[6,4]),
	rep(5,run.weeks[6,5]),rep(6,run.weeks[6,6]),rep(7,run.weeks[6,7]),rep(8,run.weeks[6,8]),rep(9,run.weeks[6,9]),
	rep(10,run.weeks[6,10]))

##### harvest numbers for each week - assumes constant 10% harvest rate

harvest.rate<-0.10
catch<-rep(NA,6)

for (i in 1:6){
  catch[i]<-sum(run.weeks[i,1:10])*harvest.rate
}

### the total number of sockeye harvested

####### weekly sampling ########

weeks<-6
n.pops<-10

  ## the catch process  
  catch1<-sample(week1,catch[1],replace=FALSE,prob=NULL)
  catch.tab1<-table(catch1)
  catch2<-sample(week2,catch[2],replace=FALSE,prob=NULL)
  catch.tab2<-table(catch2)
  catch3<-sample(week3,catch[3],replace=FALSE,prob=NULL)
  catch.tab3<-table(catch3)
  catch4<-sample(week4,catch[4],replace=FALSE,prob=NULL)
  catch.tab4<-table(catch4)
  catch5<-sample(week5,catch[5],replace=FALSE,prob=NULL)
  catch.tab5<-table(catch5)
  catch6<-sample(week6,catch[6],replace=FALSE,prob=NULL)
  catch.tab6<-table(catch6)
  
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  catch.tab_all<-table(catch.all)


  
#### sampling from the catch for each week
sample_10v<-array(NA, dim=c(10,6,1000))
sample_10v.all<-matrix(NA,nrow=1000,ncol=60)
sample_10v.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est10v<-matrix(nrow=1000,ncol=n.pops)


## 10 fish per week
for (i in 1:1000){
  sample_10v[,1,i]<-sample(catch1,10,replace=FALSE,prob=NULL)
  sample_10v[,2,i]<-sample(catch2,10,replace=FALSE,prob=NULL)
  sample_10v[,3,i]<-sample(catch3,10,replace=FALSE,prob=NULL)
  sample_10v[,4,i]<-sample(catch4,10,replace=FALSE,prob=NULL)
  sample_10v[,5,i]<-sample(catch5,10,replace=FALSE,prob=NULL)
  sample_10v[,6,i]<-sample(catch6,10,replace=FALSE,prob=NULL)

	sample_10v.all[i,]<-c(sample_10v[,1,i],sample_10v[,2,i],sample_10v[,3,i],sample_10v[,4,i],sample_10v[,5,i],sample_10v[,6,i])

  sample_10v.tab[i,1]<-length(which(sample_10v.all[i,]==1))
  sample_10v.tab[i,2]<-length(which(sample_10v.all[i,]==2))
  sample_10v.tab[i,3]<-length(which(sample_10v.all[i,]==3))
  sample_10v.tab[i,4]<-length(which(sample_10v.all[i,]==4))
  sample_10v.tab[i,5]<-length(which(sample_10v.all[i,]==5))
  sample_10v.tab[i,6]<-length(which(sample_10v.all[i,]==6))
  sample_10v.tab[i,7]<-length(which(sample_10v.all[i,]==7))
  sample_10v.tab[i,8]<-length(which(sample_10v.all[i,]==8))
  sample_10v.tab[i,9]<-length(which(sample_10v.all[i,]==9))
  sample_10v.tab[i,10]<-length(which(sample_10v.all[i,]==10))

	catch.est10v[i,]<-as.vector(sample_10v.tab[i,]/sum(sample_10v.tab[i,]))
}


catch_10vw<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_10vw[i,1]<-mean(catch.est10v[,i])
catch_10vw[i,2]<-mean(catch.est10v[,i])-(1.96*sd(catch.est10v[,i]))
catch_10vw[i,3]<-mean(catch.est10v[,i])+(1.96*sd(catch.est10v[,i]))
catch_10vw[i,4]<-sd(catch.est10v[,i])/mean(catch.est10v[,i])
}

## 20 fish per week

sample_20v<-array(NA, dim=c(20,6,1000))
sample_20v.all<-matrix(NA,nrow=1000,ncol=120)
sample_20v.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est20v<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_20v[,1,i]<-sample(catch1,20,replace=FALSE,prob=NULL)
  sample_20v[,2,i]<-sample(catch2,20,replace=FALSE,prob=NULL)
  sample_20v[,3,i]<-sample(catch3,20,replace=FALSE,prob=NULL)
  sample_20v[,4,i]<-sample(catch4,20,replace=FALSE,prob=NULL)
  sample_20v[,5,i]<-sample(catch5,20,replace=FALSE,prob=NULL)
  sample_20v[,6,i]<-sample(catch6,20,replace=FALSE,prob=NULL)

	sample_20v.all[i,]<-c(sample_20v[,1,i],sample_20v[,2,i],sample_20v[,3,i],sample_20v[,4,i],sample_20v[,5,i],sample_20v[,6,i])

  sample_20v.tab[i,1]<-length(which(sample_20v.all[i,]==1))
  sample_20v.tab[i,2]<-length(which(sample_20v.all[i,]==2))
  sample_20v.tab[i,3]<-length(which(sample_20v.all[i,]==3))
  sample_20v.tab[i,4]<-length(which(sample_20v.all[i,]==4))
  sample_20v.tab[i,5]<-length(which(sample_20v.all[i,]==5))
  sample_20v.tab[i,6]<-length(which(sample_20v.all[i,]==6))
  sample_20v.tab[i,7]<-length(which(sample_20v.all[i,]==7))
  sample_20v.tab[i,8]<-length(which(sample_20v.all[i,]==8))
  sample_20v.tab[i,9]<-length(which(sample_20v.all[i,]==9))
  sample_20v.tab[i,10]<-length(which(sample_20v.all[i,]==10))

	catch.est20v[i,]<-as.vector(sample_20v.tab[i,]/sum(sample_20v.tab[i,]))
}

catch_20vw<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_20vw[i,1]<-mean(catch.est20v[,i])
catch_20vw[i,2]<-mean(catch.est20v[,i])-(1.96*sd(catch.est20v[,i]))
catch_20vw[i,3]<-mean(catch.est20v[,i])+(1.96*sd(catch.est20v[,i]))
catch_20vw[i,4]<-sd(catch.est20v[,i])/mean(catch.est20v[,i])
}

## 40 fish per week


sample_40v<-array(NA, dim=c(40,6,1000))
sample_40v.all<-matrix(NA,nrow=1000,ncol=240)
sample_40v.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est40v<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_40v[,1,i]<-sample(catch1,40,replace=FALSE,prob=NULL)
  sample_40v[,2,i]<-sample(catch2,40,replace=FALSE,prob=NULL)
  sample_40v[,3,i]<-sample(catch3,40,replace=FALSE,prob=NULL)
  sample_40v[,4,i]<-sample(catch4,40,replace=FALSE,prob=NULL)
  sample_40v[,5,i]<-sample(catch5,40,replace=FALSE,prob=NULL)
  sample_40v[,6,i]<-sample(catch6,40,replace=FALSE,prob=NULL)

	sample_40v.all[i,]<-c(sample_40v[,1,i],sample_40v[,2,i],sample_40v[,3,i],sample_40v[,4,i],sample_40v[,5,i],sample_40v[,6,i])

  sample_40v.tab[i,1]<-length(which(sample_40v.all[i,]==1))
  sample_40v.tab[i,2]<-length(which(sample_40v.all[i,]==2))
  sample_40v.tab[i,3]<-length(which(sample_40v.all[i,]==3))
  sample_40v.tab[i,4]<-length(which(sample_40v.all[i,]==4))
  sample_40v.tab[i,5]<-length(which(sample_40v.all[i,]==5))
  sample_40v.tab[i,6]<-length(which(sample_40v.all[i,]==6))
  sample_40v.tab[i,7]<-length(which(sample_40v.all[i,]==7))
  sample_40v.tab[i,8]<-length(which(sample_40v.all[i,]==8))
  sample_40v.tab[i,9]<-length(which(sample_40v.all[i,]==9))
  sample_40v.tab[i,10]<-length(which(sample_40v.all[i,]==10))

	catch.est40v[i,]<-as.vector(sample_40v.tab[i,]/sum(sample_40v.tab[i,]))
}

catch_40vw<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_40vw[i,1]<-mean(catch.est40v[,i])
catch_40vw[i,2]<-mean(catch.est40v[,i])-(1.96*sd(catch.est40v[,i]))
catch_40vw[i,3]<-mean(catch.est40v[,i])+(1.96*sd(catch.est40v[,i]))
catch_40vw[i,4]<-sd(catch.est40v[,i])/mean(catch.est40v[,i])
}

## 60 fish per week

sample_60v<-array(NA, dim=c(60,6,1000))
sample_60v.all<-matrix(NA,nrow=1000,ncol=360)
sample_60v.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est60v<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_60v[,1,i]<-sample(catch1,60,replace=FALSE,prob=NULL)
  sample_60v[,2,i]<-sample(catch2,60,replace=FALSE,prob=NULL)
  sample_60v[,3,i]<-sample(catch3,60,replace=FALSE,prob=NULL)
  sample_60v[,4,i]<-sample(catch4,60,replace=FALSE,prob=NULL)
  sample_60v[,5,i]<-sample(catch5,60,replace=FALSE,prob=NULL)
  sample_60v[,6,i]<-sample(catch6,60,replace=FALSE,prob=NULL)

	sample_60v.all[i,]<-c(sample_60v[,1,i],sample_60v[,2,i],sample_60v[,3,i],sample_60v[,4,i],sample_60v[,5,i],sample_60v[,6,i])

  sample_60v.tab[i,1]<-length(which(sample_60v.all[i,]==1))
  sample_60v.tab[i,2]<-length(which(sample_60v.all[i,]==2))
  sample_60v.tab[i,3]<-length(which(sample_60v.all[i,]==3))
  sample_60v.tab[i,4]<-length(which(sample_60v.all[i,]==4))
  sample_60v.tab[i,5]<-length(which(sample_60v.all[i,]==5))
  sample_60v.tab[i,6]<-length(which(sample_60v.all[i,]==6))
  sample_60v.tab[i,7]<-length(which(sample_60v.all[i,]==7))
  sample_60v.tab[i,8]<-length(which(sample_60v.all[i,]==8))
  sample_60v.tab[i,9]<-length(which(sample_60v.all[i,]==9))
  sample_60v.tab[i,10]<-length(which(sample_60v.all[i,]==10))

	catch.est60v[i,]<-as.vector(sample_60v.tab[i,]/sum(sample_60v.tab[i,]))
}

catch_60vw<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_60vw[i,1]<-mean(catch.est60v[,i])
catch_60vw[i,2]<-mean(catch.est60v[,i])-(1.96*sd(catch.est60v[,i]))
catch_60vw[i,3]<-mean(catch.est60v[,i])+(1.96*sd(catch.est60v[,i]))
catch_60vw[i,4]<-sd(catch.est60v[,i])/mean(catch.est60v[,i])
}

## 100 fish per week

sample_100v<-array(NA, dim=c(100,6,1000))
sample_100v.all<-matrix(NA,nrow=1000,ncol=600)
sample_100v.tab<-matrix(nrow=1000,ncol=n.pops)
catch.est100v<-matrix(nrow=1000,ncol=n.pops)

for (i in 1:1000){
  sample_100v[,1,i]<-sample(catch1,100,replace=FALSE,prob=NULL)
  sample_100v[,2,i]<-sample(catch2,100,replace=FALSE,prob=NULL)
  sample_100v[,3,i]<-sample(catch3,100,replace=FALSE,prob=NULL)
  sample_100v[,4,i]<-sample(catch4,100,replace=FALSE,prob=NULL)
  sample_100v[,5,i]<-sample(catch5,100,replace=FALSE,prob=NULL)
  sample_100v[,6,i]<-sample(catch6,100,replace=FALSE,prob=NULL)

	sample_100v.all[i,]<-c(sample_100v[,1,i],sample_100v[,2,i],sample_100v[,3,i],sample_100v[,4,i],sample_100v[,5,i],sample_100v[,6,i])

  sample_100v.tab[i,1]<-length(which(sample_100v.all[i,]==1))
  sample_100v.tab[i,2]<-length(which(sample_100v.all[i,]==2))
  sample_100v.tab[i,3]<-length(which(sample_100v.all[i,]==3))
  sample_100v.tab[i,4]<-length(which(sample_100v.all[i,]==4))
  sample_100v.tab[i,5]<-length(which(sample_100v.all[i,]==5))
  sample_100v.tab[i,6]<-length(which(sample_100v.all[i,]==6))
  sample_100v.tab[i,7]<-length(which(sample_100v.all[i,]==7))
  sample_100v.tab[i,8]<-length(which(sample_100v.all[i,]==8))
  sample_100v.tab[i,9]<-length(which(sample_100v.all[i,]==9))
  sample_100v.tab[i,10]<-length(which(sample_100v.all[i,]==10))

	catch.est100v[i,]<-as.vector(sample_100v.tab[i,]/sum(sample_100v.tab[i,]))
}

catch_100vw<-matrix(NA,nrow=n.pops,ncol=4)
for (i in 1:n.pops){
catch_100vw[i,1]<-mean(catch.est100v[,i])  # this is actually just estimating the proportion of catch that the stock contributes
catch_100vw[i,2]<-mean(catch.est100v[,i])-(1.96*sd(catch.est100v[,i]))
catch_100vw[i,3]<-mean(catch.est100v[,i])+(1.96*sd(catch.est100v[,i]))
catch_100vw[i,4]<-sd(catch.est100v[,i])/mean(catch.est100v[,i])
}

#### estimating standard CIs for weekly sampling

catch_CIs.w<-matrix(NA,ncol=4,nrow=5) # mean, total CI width, lci, uci

#10 weekly samples
catch_CIs.w[1,1]<-mean(catch_10w[,1]/catch_10w[,1])
catch_CIs.w[1,2]<-mean((catch_10w[,3]-catch_10w[,2])/mean(catch_10w[,1]))
catch_CIs.w[1,3]<-catch_CIs.w[1,1]-(0.5*catch_CIs.w[1,2])
catch_CIs.w[1,4]<-catch_CIs.w[1,1]+(0.5*catch_CIs.w[1,2])
#20 weekly samples
catch_CIs.w[2,1]<-mean(catch_20w[,1]/catch_20w[,1])
catch_CIs.w[2,2]<-mean((catch_20w[,3]-catch_20w[,2])/mean(catch_20w[,1]))
catch_CIs.w[2,3]<-catch_CIs.w[2,1]-(0.5*catch_CIs.w[2,2])
catch_CIs.w[2,4]<-catch_CIs.w[2,1]+(0.5*catch_CIs.w[2,2])
#40 weekly samples
catch_CIs.w[3,1]<-mean(catch_40w[,1]/catch_40w[,1])
catch_CIs.w[3,2]<-mean((catch_40w[,3]-catch_40w[,2])/mean(catch_40w[,1]))
catch_CIs.w[3,3]<-catch_CIs.w[3,1]-(0.5*catch_CIs.w[3,2])
catch_CIs.w[3,4]<-catch_CIs.w[3,1]+(0.5*catch_CIs.w[3,2])
#60 weekly samples
catch_CIs.w[4,1]<-mean(catch_60w[,1]/catch_60w[,1])
catch_CIs.w[4,2]<-mean((catch_60w[,3]-catch_60w[,2])/mean(catch_60w[,1]))
catch_CIs.w[4,3]<-catch_CIs.w[4,1]-(0.5*catch_CIs.w[4,2])
catch_CIs.w[4,4]<-catch_CIs.w[4,1]+(0.5*catch_CIs.w[4,2])
#100 weekly samples
catch_CIs.w[5,1]<-mean(catch_100w[,1]/catch_100w[,1])
catch_CIs.w[5,2]<-mean((catch_100w[,3]-catch_100w[,2])/mean(catch_100w[,1]))
catch_CIs.w[5,3]<-catch_CIs.w[5,1]-(0.5*catch_CIs.w[5,2])
catch_CIs.w[5,4]<-catch_CIs.w[5,1]+(0.5*catch_CIs.w[5,2])

catch_CIs.Vw<-matrix(NA,ncol=4,nrow=5)

#10 weekly samples
catch_CIs.Vw[1,1]<-mean(catch_10vw[,1]/catch_10vw[,1])
catch_CIs.Vw[1,2]<-mean((catch_10vw[,3]-catch_10vw[,2])/mean(catch_10vw[,1]))
catch_CIs.Vw[1,3]<-catch_CIs.Vw[1,1]-(0.5*catch_CIs.Vw[1,2])
catch_CIs.Vw[1,4]<-catch_CIs.Vw[1,1]+(0.5*catch_CIs.Vw[1,2])
#20 weekly samples
catch_CIs.Vw[2,1]<-mean(catch_20vw[,1]/catch_20vw[,1])
catch_CIs.Vw[2,2]<-mean((catch_20vw[,3]-catch_20vw[,2])/mean(catch_20vw[,1]))
catch_CIs.Vw[2,3]<-catch_CIs.Vw[2,1]-(0.5*catch_CIs.Vw[2,2])
catch_CIs.Vw[2,4]<-catch_CIs.Vw[2,1]+(0.5*catch_CIs.Vw[2,2])
#40 weekly samples
catch_CIs.Vw[3,1]<-mean(catch_40vw[,1]/catch_40vw[,1])
catch_CIs.Vw[3,2]<-mean((catch_40vw[,3]-catch_40vw[,2])/mean(catch_40vw[,1]))
catch_CIs.Vw[3,3]<-catch_CIs.Vw[3,1]-(0.5*catch_CIs.Vw[3,2])
catch_CIs.Vw[3,4]<-catch_CIs.Vw[3,1]+(0.5*catch_CIs.Vw[3,2])
#60 weekly samples
catch_CIs.Vw[4,1]<-mean(catch_60vw[,1]/catch_60vw[,1])
catch_CIs.Vw[4,2]<-mean((catch_60vw[,3]-catch_60vw[,2])/mean(catch_60vw[,1]))
catch_CIs.Vw[4,3]<-catch_CIs.Vw[4,1]-(0.5*catch_CIs.Vw[4,2])
catch_CIs.Vw[4,4]<-catch_CIs.Vw[4,1]+(0.5*catch_CIs.Vw[4,2])
#100 weekly samples
catch_CIs.Vw[5,1]<-mean(catch_100vw[,1]/catch_100vw[,1])
catch_CIs.Vw[5,2]<-mean((catch_100vw[,3]-catch_100vw[,2])/mean(catch_100vw[,1]))
catch_CIs.Vw[5,3]<-catch_CIs.Vw[5,1]-(0.5*catch_CIs.Vw[5,2])
catch_CIs.Vw[5,4]<-catch_CIs.Vw[5,1]+(0.5*catch_CIs.Vw[5,2])

#### estimating and plotting CV for estimates v. sample number

mean.W<-rep(NA,5)
mean.W[1]<-mean(catch_10w[,4])
mean.W[2]<-mean(catch_20w[,4])
mean.W[3]<-mean(catch_40w[,4])
mean.W[4]<-mean(catch_60w[,4])
mean.W[5]<-mean(catch_100w[,4])

mean.Wv<-rep(NA,5)
mean.Wv[1]<-mean(catch_10vw[,4])
mean.Wv[2]<-mean(catch_20vw[,4])
mean.Wv[3]<-mean(catch_40vw[,4])
mean.Wv[4]<-mean(catch_60vw[,4])
mean.Wv[5]<-mean(catch_100vw[,4])



