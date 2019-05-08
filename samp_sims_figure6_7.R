##########################################################################  
  #####  CATCH MONITORING SIMULATION 1 - equal pop sizes and single sampling event
  ### for populations of even size (harvest rate is controlled below)
  
  mean.pop<-8.5172
  pop.var<-0
  n.pops<-10  
  harvest.rate<-0.20  ### True harvest rate - 
  samp.yr<-c(60,120,240,360,600) ### a vector of the different sample numbers taken in yearly sampling
  
  catch.true<-array(dim=c(1000,10,length(samp.yr))) ### true catch numbers for each model run
  harv.rate<-array(dim=c(1000,10,length(samp.yr))) ### true harvest rates for each model run
  
  annual.samp<-array(dim=c(1000,10,length(samp.yr)))
  catch.est.yr<-array(dim=c(1000,10,length(samp.yr))) # est. catch proportions for each stock
  total.est.yr<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock
  total.est.yr2<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock w/uncertainty in catch CV=0.2
  total.est.yr5<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock w/uncertainty in catch CV=0.5
  harv.rate.yr0<-array(dim=c(1000,10,length(samp.yr))) # estimated harvest rate CV = 0 for escape and total catch 
  harv.rate.yr1<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr2<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr3<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr4<-array(dim=c(1000,10,length(samp.yr))) # estimated harvest rate CV = 0.5 for escape and total catch 

##### Running the model ofr each sample number (s), and 1000 iterations (i)
  
for(s in 1:length(samp.yr)){
  
  for (i in 1:1000){ #### running the models 1000 times
  
  ### weekly abundance for each population w/ differing levels of uncertainty
  pop.sizes<-round(rlnorm(n.pops,mean.pop,pop.var))
  pop.sizesCV.2<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.2)))
  pop.sizesCV.5<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.5)))
  pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100 ## the 100 in the rmultinom statement controls the variance
  
  ######   these are all the fish coming through the fishing area for each week
  run.weeks<-matrix(NA,nrow=6,ncol=n.pops)
  
  for (n in 1:n.pops){
    run.weeks[,n]<-round(pop.timing[,n]*pop.sizes[n])
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
  
  ###################################################
  ### PART 2a - simulating the catch process - total catch
  
  CV.2<-rnorm(1,0,0.2) ### observation error CV = 0.2
  CV.5<-rnorm(1,0,0.5) ### observation error CV = 0.5
  catch<-rep(NA,6) # BC added this May 1, 2019
  catch.estCV.2<-rep(NA,6)
  catch.estCV.5<-rep(NA,6)
  
  for (j in 1:6){
  catch[j]<-sum(run.weeks[j,])*harvest.rate
  catch.estCV.2[j]<-abs(round(catch[j]+(catch[j]*CV.2)))
  catch.estCV.5[j]<-abs(round(catch[j]+(catch[j]*CV.5)))
  }

  total.catch<-sum(catch)
  total.catch_CV.2<-sum(catch.estCV.2) ### this is the estimated total harvest - including obs error (CV = 0.2)
  total.catch_CV.5<-sum(catch.estCV.5) ### this is the estimated total harvest - inlcuding obs error (CV = 0.5)
  
  ############################################################################
  ##### PART 2b - the weekly catch of individual fish (e.g. different stocks)
  ## the catch process - This is the true catch numbers.   
    
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
  
  ### combining stock-ID for all the fish caught (5000 total) 
  catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
  ### Making a table of stock specific catch
  catch.tab_all<-table(catch.all)
  
  catch.true[i,,s]<-as.vector(catch.tab_all)
  harv.rate[i,,s]<-catch.true[i,,s]/pop.sizes
  
  ##########################################
  ## PART 3 - the catch sampling process  

  catch.wks<-list(catch2,catch3,catch4)
  samples.yr<-sample(catch.wks[[sample(1:3,1)]],samp.yr[s],replace=FALSE,prob=NULL)

  annual.samp[i,1,s]<-length(which(samples.yr==1))
  annual.samp[i,2,s]<-length(which(samples.yr==2))
  annual.samp[i,3,s]<-length(which(samples.yr==3))
  annual.samp[i,4,s]<-length(which(samples.yr==4))
  annual.samp[i,5,s]<-length(which(samples.yr==5))
  annual.samp[i,6,s]<-length(which(samples.yr==6))
  annual.samp[i,7,s]<-length(which(samples.yr==7))
  annual.samp[i,8,s]<-length(which(samples.yr==8))
  annual.samp[i,9,s]<-length(which(samples.yr==9))
  annual.samp[i,10,s]<-length(which(samples.yr==10))
  
  ## contribution of each population in the catch
  catch.est.yr[i,,s]<-as.vector(annual.samp[i,,s]/sum(annual.samp[i,,s])) 
  
  # total estimated harvest for each pop.
  total.est.yr[i,,s]<-round(catch.est.yr[i,,s]*total.catch) 
  total.est.yr2[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.2)
  total.est.yr5[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.5)
  
  # total estimated harvest rate for each pop.
  harv.rate.yr0[i,,s]<-total.est.yr[i,,s]/pop.sizes
  harv.rate.yr1[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.2 # catch CV = 0.2; escape CV = 0.2
  harv.rate.yr2[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.5 # catch CV = 0.2; escape CV = 0.5
  harv.rate.yr3[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.2 # catch CV = 0.5; escape CV = 0.2
  harv.rate.yr4[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.5 # catch CV = 0.5; escape CV = 0.5
  
  }

}

  ##### sweeet!!! we are now generating data
  
###############################################################################  
##### estimating the mean & sd of catch contrib. - just reflects sample sizes not obs error

comp.yr1<-array(dim=c(n.pops,4,length(samp.yr))) ## catch composition for single sample and even pop sizes
  
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      comp.yr1[i,1,j]<-mean(catch.est.yr[,i,j])
      comp.yr1[i,2,j]<-mean(catch.est.yr[,i,j])-(1.96*sd(catch.est.yr[,i,j]))
      comp.yr1[i,3,j]<-mean(catch.est.yr[,i,j])+(1.96*sd(catch.est.yr[,i,j]))
      comp.yr1[i,4,j]<-sd(catch.est.yr[,i,j])/mean(catch.est.yr[,i,j])
      
    }
  }
  
##### estimating the mean and sd of harvest rate estimates for our model 

  rate.yr1.0<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr1.1<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr1.2<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr1.3<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr1.4<-array(dim=c(n.pops,4,length(samp.yr)))

  
### assuming perfect knowledge of total catch and escapement    

for(j in 1:length(samp.yr)){      
  for(i in 1:10){
    rate.yr1.0[i,1,j]<-mean(harv.rate.yr0[,i,j])
    rate.yr1.0[i,2,j]<-mean(harv.rate.yr0[,i,j])-(1.96*sd(harv.rate.yr0[,i,j]))
    rate.yr1.0[i,3,j]<-mean(harv.rate.yr0[,i,j])+(1.96*sd(harv.rate.yr0[,i,j]))
    rate.yr1.0[i,4,j]<-sd(harv.rate.yr0[,i,j])/mean(harv.rate.yr0[,i,j])
    
  }
}
  
  ### CV = 0.2 for catch and escapement 
  harv.rate.yr1[which(harv.rate.yr1>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr1.1[i,1,j]<-mean(harv.rate.yr1[,i,j])
      rate.yr1.1[i,2,j]<-mean(harv.rate.yr1[,i,j])-(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr1.1[i,3,j]<-mean(harv.rate.yr1[,i,j])+(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr1.1[i,4,j]<-sd(harv.rate.yr1[,i,j])/mean(harv.rate.yr1[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch; CV = 0.5 for escapement 
  harv.rate.yr2[which(harv.rate.yr2>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr1.2[i,1,j]<-mean(harv.rate.yr2[,i,j])
      rate.yr1.2[i,2,j]<-mean(harv.rate.yr2[,i,j])-(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr1.2[i,3,j]<-mean(harv.rate.yr2[,i,j])+(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr1.2[i,4,j]<-sd(harv.rate.yr2[,i,j])/mean(harv.rate.yr2[,i,j])
      
    }
  }
  
  ### CV = 0.5 for catch; CV = 0.2 for escapement 
  harv.rate.yr3[which(harv.rate.yr3>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr1.3[i,1,j]<-mean(harv.rate.yr3[,i,j])
      rate.yr1.3[i,2,j]<-mean(harv.rate.yr3[,i,j])-(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr1.3[i,3,j]<-mean(harv.rate.yr3[,i,j])+(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr1.3[i,4,j]<-sd(harv.rate.yr3[,i,j])/mean(harv.rate.yr3[,i,j])
      
    }
  }
  
  
  ### CV = 0.5 for catch; CV = 0.5 for escapement 
  harv.rate.yr4[which(harv.rate.yr4>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr1.4[i,1,j]<-mean(harv.rate.yr4[,i,j])
      rate.yr1.4[i,2,j]<-mean(harv.rate.yr4[,i,j])-(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr1.4[i,3,j]<-mean(harv.rate.yr4[,i,j])+(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr1.4[i,4,j]<-sd(harv.rate.yr4[,i,j])/mean(harv.rate.yr4[,i,j])
      
    }
  }  
  
  
  ##########################################################################  
  #####  CATCH MONITORING SIMULATION 2 - variable pop sizes and single sampling event

  n.pops<-10  
  harvest.rate<-0.20  ### True harvest rate - 
  samp.yr<-c(60,120,240,360,600) ### a vector of the different sample numbers taken in yearly sampling
  
  catch.true<-array(dim=c(1000,10,length(samp.yr))) ### true catch numbers for each model run
  harv.rate<-array(dim=c(1000,10,length(samp.yr))) ### true harvest rates for each model run
  
  annual.samp<-array(dim=c(1000,10,length(samp.yr)))
  catch.est.yr<-array(dim=c(1000,10,length(samp.yr))) # est. catch proportions for each stock
  total.est.yr<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock
  total.est.yr2<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock w/uncertainty in catch CV=0.2
  total.est.yr5<-array(dim=c(1000,10,length(samp.yr))) # est. catch totals for each stock w/uncertainty in catch CV=0.5
  harv.rate.yr0<-array(dim=c(1000,10,length(samp.yr))) # estimated harvest rate CV = 0 for escape and total catch 
  harv.rate.yr1<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr2<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr3<-array(dim=c(1000,10,length(samp.yr))) 
  harv.rate.yr4<-array(dim=c(1000,10,length(samp.yr))) # estimated harvest rate CV = 0.5 for escape and total catch 
  
  ##### Running the model ofr each sample number (s), and 1000 iterations (i)
  
  for(s in 1:length(samp.yr)){
    
    for (i in 1:1000){ #### running the models 1000 times
      
      ### weekly abundance for each population w/ differing levels of uncertainty
      pop.sizes<-c(500,1000,2000,3000,4000,6000,8000,10000,15000,20000)
      pop.sizesCV.2<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.2)))
      pop.sizesCV.5<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.5)))
      pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100 ## the 100 in the rmultinom statement controls the variance
      
      ######   these are all the fish coming through the fishing area for each week
      run.weeks<-matrix(NA,nrow=6,ncol=n.pops)
      
      for (n in 1:n.pops){
        run.weeks[,n]<-round(pop.timing[,n]*pop.sizes[n])
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
      
      ###################################################
      ### PART 2a - simulating the catch process - total catch
      
      CV.2<-rnorm(1,0,0.2) ### observation error CV = 0.2
      CV.5<-rnorm(1,0,0.5) ### observation error CV = 0.5
      catch.estCV.2<-rep(NA,6)
      catch.estCV.5<-rep(NA,6)
      
      for (j in 1:6){
        catch[j]<-sum(run.weeks[j,])*harvest.rate
        catch.estCV.2[j]<-abs(round(catch[j]+(catch[j]*CV.2)))
        catch.estCV.5[j]<-abs(round(catch[j]+(catch[j]*CV.5)))
      }
      
      total.catch<-sum(catch)
      total.catch_CV.2<-sum(catch.estCV.2) ### this is the estimated total harvest - including obs error (CV = 0.2)
      total.catch_CV.5<-sum(catch.estCV.5) ### this is the estimated total harvest - inlcuding obs error (CV = 0.5)
      
      ############################################################################
      ##### PART 2b - the weekly catch of individual fish (e.g. different stocks)
      ## the catch process - This is the true catch numbers.   
      
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
      
      ### combining stock-ID for all the fish caught (5000 total) 
      catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
      ### Making a table of stock specific catch
      catch.tab_all<-table(catch.all)
      
      catch.true[i,,s]<-as.vector(catch.tab_all)
      harv.rate[i,,s]<-catch.true[i,,s]/pop.sizes
      
      ##########################################
      ## PART 3 - the catch sampling process  
      
      catch.wks<-list(catch2,catch3,catch4)
      samples.yr<-sample(catch.wks[[sample(1:3,1)]],samp.yr[s],replace=FALSE,prob=NULL)
      
      annual.samp[i,1,s]<-length(which(samples.yr==1))
      annual.samp[i,2,s]<-length(which(samples.yr==2))
      annual.samp[i,3,s]<-length(which(samples.yr==3))
      annual.samp[i,4,s]<-length(which(samples.yr==4))
      annual.samp[i,5,s]<-length(which(samples.yr==5))
      annual.samp[i,6,s]<-length(which(samples.yr==6))
      annual.samp[i,7,s]<-length(which(samples.yr==7))
      annual.samp[i,8,s]<-length(which(samples.yr==8))
      annual.samp[i,9,s]<-length(which(samples.yr==9))
      annual.samp[i,10,s]<-length(which(samples.yr==10))
      
      ## contribution of each population in the catch
      catch.est.yr[i,,s]<-as.vector(annual.samp[i,,s]/sum(annual.samp[i,,s])) 
      
      # total estimated harvest for each pop.
      total.est.yr[i,,s]<-round(catch.est.yr[i,,s]*total.catch) 
      total.est.yr2[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.2)
      total.est.yr5[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.5)
      
      # total estimated harvest rate for each pop.
      harv.rate.yr0[i,,s]<-total.est.yr[i,,s]/pop.sizes
      harv.rate.yr1[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.2 # catch CV = 0.2; escape CV = 0.2
      harv.rate.yr2[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.5 # catch CV = 0.2; escape CV = 0.5
      harv.rate.yr3[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.2 # catch CV = 0.5; escape CV = 0.2
      harv.rate.yr4[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.5 # catch CV = 0.5; escape CV = 0.5
      
    }
    
  }  
  
  
  ###############################################################################  
  ##### estimating the mean & sd of catch contrib. - just reflects sample sizes not obs error
  
  comp.yr2<-array(dim=c(n.pops,4,length(samp.yr))) ## catch composition for single sample and even pop sizes
  
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      comp.yr2[i,1,j]<-mean(catch.est.yr[,i,j])
      comp.yr2[i,2,j]<-mean(catch.est.yr[,i,j])-(1.96*sd(catch.est.yr[,i,j]))
      comp.yr2[i,3,j]<-mean(catch.est.yr[,i,j])+(1.96*sd(catch.est.yr[,i,j]))
      comp.yr2[i,4,j]<-sd(catch.est.yr[,i,j])/mean(catch.est.yr[,i,j])
      
    }
  }
  
  ##### estimating the mean and sd of harvest rate estimates for our model 
  
  rate.yr2.0<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr2.1<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr2.2<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr2.3<-array(dim=c(n.pops,4,length(samp.yr)))
  rate.yr2.4<-array(dim=c(n.pops,4,length(samp.yr)))
  
  
  ### assuming perfect knowledge of total catch and escapement    
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr2.0[i,1,j]<-mean(harv.rate.yr0[,i,j])
      rate.yr2.0[i,2,j]<-mean(harv.rate.yr0[,i,j])-(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr2.0[i,3,j]<-mean(harv.rate.yr0[,i,j])+(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr2.0[i,4,j]<-sd(harv.rate.yr0[,i,j])/mean(harv.rate.yr0[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch and escapement 
  harv.rate.yr1[which(harv.rate.yr1>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr2.1[i,1,j]<-mean(harv.rate.yr1[,i,j])
      rate.yr2.1[i,2,j]<-mean(harv.rate.yr1[,i,j])-(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr2.1[i,3,j]<-mean(harv.rate.yr1[,i,j])+(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr2.1[i,4,j]<-sd(harv.rate.yr1[,i,j])/mean(harv.rate.yr1[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch; CV = 0.5 for escapement 
  harv.rate.yr2[which(harv.rate.yr2>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr2.2[i,1,j]<-mean(harv.rate.yr2[,i,j])
      rate.yr2.2[i,2,j]<-mean(harv.rate.yr2[,i,j])-(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr2.2[i,3,j]<-mean(harv.rate.yr2[,i,j])+(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr2.2[i,4,j]<-sd(harv.rate.yr2[,i,j])/mean(harv.rate.yr2[,i,j])
      
    }
  }
  
  ### CV = 0.5 for catch; CV = 0.2 for escapement 
  harv.rate.yr3[which(harv.rate.yr3>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr2.3[i,1,j]<-mean(harv.rate.yr3[,i,j])
      rate.yr2.3[i,2,j]<-mean(harv.rate.yr3[,i,j])-(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr2.3[i,3,j]<-mean(harv.rate.yr3[,i,j])+(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr2.3[i,4,j]<-sd(harv.rate.yr3[,i,j])/mean(harv.rate.yr3[,i,j])
      
    }
  }
  
  
  ### CV = 0.5 for catch; CV = 0.5 for escapement 
  harv.rate.yr4[which(harv.rate.yr4>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(samp.yr)){      
    for(i in 1:10){
      rate.yr2.4[i,1,j]<-mean(harv.rate.yr4[,i,j])
      rate.yr2.4[i,2,j]<-mean(harv.rate.yr4[,i,j])-(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr2.4[i,3,j]<-mean(harv.rate.yr4[,i,j])+(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr2.4[i,4,j]<-sd(harv.rate.yr4[,i,j])/mean(harv.rate.yr4[,i,j])
      
    }
  }  


##########################################################################
######    Weekly sampling
##########
### for populations of constant size (harvest rate is controlled below)
  mean.pop<-8.5172
  pop.var<-0
  n.pops<-10  
  harvest.rate<-0.20  ### True harvest rate - 
  no.wk<-c(10,20,40,60,100) ### a vector of the different sample numbers taken in yearly sampling
  
  catch.true<-array(dim=c(1000,10,length(no.wk))) ### true catch numbers for each model run
  harv.rate<-array(dim=c(1000,10,length(no.wk))) ### true harvest rates for each model run
  
  weekly.samp<-array(dim=c(1000,10,length(no.wk))) 
  catch.est.yr<-array(dim=c(1000,10,length(no.wk))) # est. catch proportions for each stock
  total.est.yr<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock
  total.est.yr2<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock w/uncertainty in catch CV=0.2
  total.est.yr5<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock w/uncertainty in catch CV=0.5
  harv.rate.yr0<-array(dim=c(1000,10,length(no.wk))) # estimated harvest rate CV = 0 for escape and total catch 
  harv.rate.yr1<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr2<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr3<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr4<-array(dim=c(1000,10,length(no.wk))) # estimated harvest rate CV = 0.5 for escape and total catch 
  

  ##### Running the model ofr each sample number (s), and 1000 iterations (i)
  
  for(s in 1:length(no.wk)){
    
    for (i in 1:1000){ #### running the models 1000 times
      
      ### weekly abundance for each population w/ differing levels of uncertainty
      pop.sizes<-round(rlnorm(n.pops,mean.pop,pop.var))
      pop.sizesCV.2<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.2)))
      pop.sizesCV.5<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.5)))
      pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100 ## the 100 in the rmultinom statement controls the variance
      
      ######   these are all the fish coming through the fishing area for each week
      run.weeks<-matrix(NA,nrow=6,ncol=n.pops)
      
      for (n in 1:n.pops){
        run.weeks[,n]<-round(pop.timing[,n]*pop.sizes[n])
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
      
      ###################################################
      ### PART 2a - simulating the catch process - total catch
      
      CV.2<-rnorm(1,0,0.2) ### observation error CV = 0.2
      CV.5<-rnorm(1,0,0.5) ### observation error CV = 0.5
      catch.estCV.2<-rep(NA,6)
      catch.estCV.5<-rep(NA,6)
      
      for (j in 1:6){
        catch[j]<-sum(run.weeks[j,])*harvest.rate
        catch.estCV.2[j]<-abs(round(catch[j]+(catch[j]*CV.2)))
        catch.estCV.5[j]<-abs(round(catch[j]+(catch[j]*CV.5)))
      }
      
      total.catch<-sum(catch)
      total.catch_CV.2<-sum(catch.estCV.2) ### this is the estimated total harvest - including obs error (CV = 0.2)
      total.catch_CV.5<-sum(catch.estCV.5) ### this is the estimated total harvest - inlcuding obs error (CV = 0.5)
      
      ############################################################################
      ##### PART 2b - the weekly catch of individual fish (e.g. different stocks)
      ## the catch process - This is the true catch numbers.   
      
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
      
      ### combining stock-ID for all the fish caught (5000 total) 
      catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
      ### Making a table of stock specific catch
      catch.tab_all<-table(catch.all)
      
      catch.true[i,,s]<-as.vector(catch.tab_all)
      harv.rate[i,,s]<-catch.true[i,,s]/pop.sizes

      ##########################################
      ## PART 3 - the catch sampling process  
      
      samples.wk<-matrix(nrow=6,ncol=no.wk[s])
      catch.wks<-list(catch1,catch2,catch3,catch4,catch5,catch6)

    ### this is our weekly sampling process with 6 weeks and sample numbers = no.wk[s]        
    for(w in 1:length(catch.wks)){
      samples.wk[w,]<-sample(catch.wks[[w]],no.wk[s],replace=FALSE,prob=NULL)
    }

      weekly.samp[i,1,s]<-length(which(samples.wk==1))
      weekly.samp[i,2,s]<-length(which(samples.wk==2))
      weekly.samp[i,3,s]<-length(which(samples.wk==3))
      weekly.samp[i,4,s]<-length(which(samples.wk==4))
      weekly.samp[i,5,s]<-length(which(samples.wk==5))
      weekly.samp[i,6,s]<-length(which(samples.wk==6))
      weekly.samp[i,7,s]<-length(which(samples.wk==7))
      weekly.samp[i,8,s]<-length(which(samples.wk==8))
      weekly.samp[i,9,s]<-length(which(samples.wk==9))
      weekly.samp[i,10,s]<-length(which(samples.wk==10))
      
      ## contribution of each population in the catch
      catch.est.yr[i,,s]<-as.vector(weekly.samp[i,,s]/sum(weekly.samp[i,,s])) 
      
      # total estimated harvest for each pop.
      total.est.yr[i,,s]<-round(catch.est.yr[i,,s]*total.catch) 
      total.est.yr2[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.2)
      total.est.yr5[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.5)
      
      # total estimated harvest rate for each pop.
      harv.rate.yr0[i,,s]<-total.est.yr[i,,s]/pop.sizes
      harv.rate.yr1[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.2 # catch CV = 0.2; escape CV = 0.2
      harv.rate.yr2[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.5 # catch CV = 0.2; escape CV = 0.5
      harv.rate.yr3[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.2 # catch CV = 0.5; escape CV = 0.2
      harv.rate.yr4[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.5 # catch CV = 0.5; escape CV = 0.5
      
    }
    
  }  
  
  ###############################################################################  
  ##### estimating the mean & sd of catch contrib. - just reflects sample sizes not obs error
  
  comp.yr3<-array(dim=c(n.pops,4,length(no.wk))) ## catch composition for single sample and even pop sizes
  
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      comp.yr3[i,1,j]<-mean(catch.est.yr[,i,j])
      comp.yr3[i,2,j]<-mean(catch.est.yr[,i,j])-(1.96*sd(catch.est.yr[,i,j]))
      comp.yr3[i,3,j]<-mean(catch.est.yr[,i,j])+(1.96*sd(catch.est.yr[,i,j]))
      comp.yr3[i,4,j]<-sd(catch.est.yr[,i,j])/mean(catch.est.yr[,i,j])
      
    }
  }
  
  comp.yr3[,,5]
  ##### estimating the mean and sd of harvest rate estimates for our model 
  
  rate.yr3.0<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr3.1<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr3.2<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr3.3<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr3.4<-array(dim=c(n.pops,4,length(no.wk)))
  
  
  ### assuming perfect knowledge of total catch and escapement    
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr3.0[i,1,j]<-mean(harv.rate.yr0[,i,j])
      rate.yr3.0[i,2,j]<-mean(harv.rate.yr0[,i,j])-(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr3.0[i,3,j]<-mean(harv.rate.yr0[,i,j])+(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr3.0[i,4,j]<-sd(harv.rate.yr0[,i,j])/mean(harv.rate.yr0[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch and escapement 
  harv.rate.yr1[which(harv.rate.yr1>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr3.1[i,1,j]<-mean(harv.rate.yr1[,i,j])
      rate.yr3.1[i,2,j]<-mean(harv.rate.yr1[,i,j])-(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr3.1[i,3,j]<-mean(harv.rate.yr1[,i,j])+(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr3.1[i,4,j]<-sd(harv.rate.yr1[,i,j])/mean(harv.rate.yr1[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch; CV = 0.5 for escapement 
  harv.rate.yr2[which(harv.rate.yr2>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr3.2[i,1,j]<-mean(harv.rate.yr2[,i,j])
      rate.yr3.2[i,2,j]<-mean(harv.rate.yr2[,i,j])-(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr3.2[i,3,j]<-mean(harv.rate.yr2[,i,j])+(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr3.2[i,4,j]<-sd(harv.rate.yr2[,i,j])/mean(harv.rate.yr2[,i,j])
      
    }
  }
  
  ### CV = 0.5 for catch; CV = 0.2 for escapement 
  harv.rate.yr3[which(harv.rate.yr3>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr3.3[i,1,j]<-mean(harv.rate.yr3[,i,j])
      rate.yr3.3[i,2,j]<-mean(harv.rate.yr3[,i,j])-(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr3.3[i,3,j]<-mean(harv.rate.yr3[,i,j])+(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr3.3[i,4,j]<-sd(harv.rate.yr3[,i,j])/mean(harv.rate.yr3[,i,j])
      
    }
  }
  
  
  ### CV = 0.5 for catch; CV = 0.5 for escapement 
  harv.rate.yr4[which(harv.rate.yr4>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr3.4[i,1,j]<-mean(harv.rate.yr4[,i,j])
      rate.yr3.4[i,2,j]<-mean(harv.rate.yr4[,i,j])-(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr3.4[i,3,j]<-mean(harv.rate.yr4[,i,j])+(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr3.4[i,4,j]<-sd(harv.rate.yr4[,i,j])/mean(harv.rate.yr4[,i,j])
      
    }
  }  
  
  
  
  ##########################################################################
  ######    Weekly sampling
  ######
  #####     for populations of variable size (harvest rate is controlled below)
  
  n.pops<-10  
  harvest.rate<-0.20  ### True harvest rate - 
  no.wk<-c(10,20,40,60,100) ### a vector of the different sample numbers taken in yearly sampling
  
  catch.true<-array(dim=c(1000,10,length(no.wk))) ### true catch numbers for each model run
  harv.rate<-array(dim=c(1000,10,length(no.wk))) ### true harvest rates for each model run
  
  weekly.samp<-array(dim=c(1000,10,length(no.wk))) 
  catch.est.yr<-array(dim=c(1000,10,length(no.wk))) # est. catch proportions for each stock
  total.est.yr<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock
  total.est.yr2<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock w/uncertainty in catch CV=0.2
  total.est.yr5<-array(dim=c(1000,10,length(no.wk))) # est. catch totals for each stock w/uncertainty in catch CV=0.5
  harv.rate.yr0<-array(dim=c(1000,10,length(no.wk))) # estimated harvest rate CV = 0 for escape and total catch 
  harv.rate.yr1<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr2<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr3<-array(dim=c(1000,10,length(no.wk))) 
  harv.rate.yr4<-array(dim=c(1000,10,length(no.wk))) # estimated harvest rate CV = 0.5 for escape and total catch 
  
  
  ##### Running the model ofr each sample number (s), and 1000 iterations (i)
  
  for(s in 1:length(no.wk)){
    
    for (i in 1:1000){ #### running the models 1000 times
      
      ### weekly abundance for each population w/ differing levels of uncertainty
   
      pop.sizes<-c(500,1000,2000,3000,4000,6000,8000,10000,15000,20000)
      pop.sizesCV.2<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.2)))
      pop.sizesCV.5<-round(pop.sizes*(rlnorm(length(pop.sizes),0,0.5)))
      pop.timing<-rmultinom(n.pops,100,c(0.1,0.2,0.3,0.2,0.15,0.05))/100 ## the 100 in the rmultinom statement controls the variance
      
      ######   these are all the fish coming through the fishing area for each week
      run.weeks<-matrix(NA,nrow=6,ncol=n.pops)
      
      for (n in 1:n.pops){
        run.weeks[,n]<-round(pop.timing[,n]*pop.sizes[n])
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
      
      ###################################################
      ### PART 2a - simulating the catch process - total catch
      
      CV.2<-rnorm(1,0,0.2) ### observation error CV = 0.2
      CV.5<-rnorm(1,0,0.5) ### observation error CV = 0.5
      catch.estCV.2<-rep(NA,6)
      catch.estCV.5<-rep(NA,6)
      
      for (j in 1:6){
        catch[j]<-sum(run.weeks[j,])*harvest.rate
        catch.estCV.2[j]<-abs(round(catch[j]+(catch[j]*CV.2)))
        catch.estCV.5[j]<-abs(round(catch[j]+(catch[j]*CV.5)))
      }
      
      total.catch<-sum(catch)
      total.catch_CV.2<-sum(catch.estCV.2) ### this is the estimated total harvest - including obs error (CV = 0.2)
      total.catch_CV.5<-sum(catch.estCV.5) ### this is the estimated total harvest - inlcuding obs error (CV = 0.5)
      
      ############################################################################
      ##### PART 2b - the weekly catch of individual fish (e.g. different stocks)
      ## the catch process - This is the true catch numbers.   
      
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
      
      ### combining stock-ID for all the fish caught (5000 total) 
      catch.all<-c(catch1,catch2,catch3,catch4,catch5,catch6)
      ### Making a table of stock specific catch
      catch.tab_all<-table(catch.all)
      
      catch.true[i,,s]<-as.vector(catch.tab_all)
      harv.rate[i,,s]<-catch.true[i,,s]/pop.sizes
      
      ##########################################
      ## PART 3 - the catch sampling process  
      
      samples.wk<-matrix(nrow=6,ncol=no.wk[s])
      catch.wks<-list(catch1,catch2,catch3,catch4,catch5,catch6)
      
      ### this is our weekly sampling process with 6 weeks and sample numbers = no.wk[s]        
      for(w in 1:length(catch.wks)){
        samples.wk[w,]<-sample(catch.wks[[w]],no.wk[s],replace=FALSE,prob=NULL)
      }
      
      weekly.samp[i,1,s]<-length(which(samples.wk==1))
      weekly.samp[i,2,s]<-length(which(samples.wk==2))
      weekly.samp[i,3,s]<-length(which(samples.wk==3))
      weekly.samp[i,4,s]<-length(which(samples.wk==4))
      weekly.samp[i,5,s]<-length(which(samples.wk==5))
      weekly.samp[i,6,s]<-length(which(samples.wk==6))
      weekly.samp[i,7,s]<-length(which(samples.wk==7))
      weekly.samp[i,8,s]<-length(which(samples.wk==8))
      weekly.samp[i,9,s]<-length(which(samples.wk==9))
      weekly.samp[i,10,s]<-length(which(samples.wk==10))
      
      ## contribution of each population in the catch
      catch.est.yr[i,,s]<-as.vector(weekly.samp[i,,s]/sum(weekly.samp[i,,s])) 
      
      # total estimated harvest for each pop.
      total.est.yr[i,,s]<-round(catch.est.yr[i,,s]*total.catch) 
      total.est.yr2[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.2)
      total.est.yr5[i,,s]<-round(catch.est.yr[i,,s]*total.catch_CV.5)
      
      # total estimated harvest rate for each pop.
      harv.rate.yr0[i,,s]<-total.est.yr[i,,s]/pop.sizes
      harv.rate.yr1[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.2 # catch CV = 0.2; escape CV = 0.2
      harv.rate.yr2[i,,s]<-total.est.yr2[i,,s]/pop.sizesCV.5 # catch CV = 0.2; escape CV = 0.5
      harv.rate.yr3[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.2 # catch CV = 0.5; escape CV = 0.2
      harv.rate.yr4[i,,s]<-total.est.yr5[i,,s]/pop.sizesCV.5 # catch CV = 0.5; escape CV = 0.5
      
    }
    
  }  

  ###############################################################################  
  ##### estimating the mean & sd of catch contrib. - just reflects sample sizes not obs error
  
  comp.yr4<-array(dim=c(n.pops,4,length(no.wk))) ## catch composition for single sample and even pop sizes
  
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      comp.yr4[i,1,j]<-mean(catch.est.yr[,i,j])
      comp.yr4[i,2,j]<-mean(catch.est.yr[,i,j])-(1.96*sd(catch.est.yr[,i,j]))
      comp.yr4[i,3,j]<-mean(catch.est.yr[,i,j])+(1.96*sd(catch.est.yr[,i,j]))
      comp.yr4[i,4,j]<-sd(catch.est.yr[,i,j])/mean(catch.est.yr[,i,j])
      
    }
  }
  
  comp.yr4[,,5]
  ##### estimating the mean and sd of harvest rate estimates for our model 
  
  rate.yr4.0<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr4.1<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr4.2<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr4.3<-array(dim=c(n.pops,4,length(no.wk)))
  rate.yr4.4<-array(dim=c(n.pops,4,length(no.wk)))
  
  
  ### assuming perfect knowledge of total catch and escapement    
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr4.0[i,1,j]<-mean(harv.rate.yr0[,i,j])
      rate.yr4.0[i,2,j]<-mean(harv.rate.yr0[,i,j])-(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr4.0[i,3,j]<-mean(harv.rate.yr0[,i,j])+(1.96*sd(harv.rate.yr0[,i,j]))
      rate.yr4.0[i,4,j]<-sd(harv.rate.yr0[,i,j])/mean(harv.rate.yr0[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch and escapement 
  harv.rate.yr1[which(harv.rate.yr1>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr4.1[i,1,j]<-mean(harv.rate.yr1[,i,j])
      rate.yr4.1[i,2,j]<-mean(harv.rate.yr1[,i,j])-(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr4.1[i,3,j]<-mean(harv.rate.yr1[,i,j])+(1.96*sd(harv.rate.yr1[,i,j]))
      rate.yr4.1[i,4,j]<-sd(harv.rate.yr1[,i,j])/mean(harv.rate.yr1[,i,j])
      
    }
  }
  
  ### CV = 0.2 for catch; CV = 0.5 for escapement 
  harv.rate.yr2[which(harv.rate.yr2>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr4.2[i,1,j]<-mean(harv.rate.yr2[,i,j])
      rate.yr4.2[i,2,j]<-mean(harv.rate.yr2[,i,j])-(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr4.2[i,3,j]<-mean(harv.rate.yr2[,i,j])+(1.96*sd(harv.rate.yr2[,i,j]))
      rate.yr4.2[i,4,j]<-sd(harv.rate.yr2[,i,j])/mean(harv.rate.yr2[,i,j])
      
    }
  }
  
  ### CV = 0.5 for catch; CV = 0.2 for escapement 
  harv.rate.yr3[which(harv.rate.yr3>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr4.3[i,1,j]<-mean(harv.rate.yr3[,i,j])
      rate.yr4.3[i,2,j]<-mean(harv.rate.yr3[,i,j])-(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr4.3[i,3,j]<-mean(harv.rate.yr3[,i,j])+(1.96*sd(harv.rate.yr3[,i,j]))
      rate.yr4.3[i,4,j]<-sd(harv.rate.yr3[,i,j])/mean(harv.rate.yr3[,i,j])
      
    }
  }
  
  
  ### CV = 0.5 for catch; CV = 0.5 for escapement 
  harv.rate.yr4[which(harv.rate.yr4>1)]<-0.95 ## setting harvest rates above 1 to 0.95
  
  for(j in 1:length(no.wk)){      
    for(i in 1:10){
      rate.yr4.4[i,1,j]<-mean(harv.rate.yr4[,i,j])
      rate.yr4.4[i,2,j]<-mean(harv.rate.yr4[,i,j])-(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr4.4[i,3,j]<-mean(harv.rate.yr4[,i,j])+(1.96*sd(harv.rate.yr4[,i,j]))
      rate.yr4.4[i,4,j]<-sd(harv.rate.yr4[,i,j])/mean(harv.rate.yr4[,i,j])
      
    }
  }  
  

####################################################################
  #####  Plotting uncertainty in catch composition 

  ### fixed population size (5000)  
  
  comp.equal<-matrix(nrow=5,ncol=2)
  comp.yr1[,,1]

  for (i in 1:5){
  comp.equal[i,1]<-mean(comp.yr1[,4,i]) ## single sample
  comp.equal[i,2]<-mean(comp.yr3[,4,i]) ## weekly sample
  }
  
  ### variable population size
  comp.var<-matrix(nrow=10,ncol=10)

  for (i in 1:5){
  comp.var[,i]<-comp.yr2[,4,i]
  comp.var[,i+5]<-comp.yr4[,4,i]
  }
max(comp.var)



library(viridis)

colors <- rev(viridis(10))
  
jpeg(filename = "figures/figure_6.jpeg",
     width = 2600, height = 1500, units = "px", pointsize = 9,
     bg = "white", res = 600)

samples<-c(60,120,240,360,600)

par(mfrow=c(1,2))    
par(mar=c(2,1,1,0),oma=c(2,4,1,1))
### single sample
for(i in 1:10){
  plot(comp.var[i,1:5],type="l",ylim=c(0,1.6),axes=FALSE,xlab="",ylab="",col= colors[i])
  par(new=TRUE)
}
plot(comp.equal[,1],type="l",las=1,xlab="",ylab="",lwd=2,ylim=c(0,1.6),axes=FALSE)    

axis(1,at=c(1,2,3,4,5),labels=samples,cex.axis=0.9)
axis(2,las=1,at=c(0,0.5,1.0,1.5,2.0),cex.axis=0.9)
text(2.5,1.55,"(a) annual sampling",cex=0.8)
text(4,0.83,"smallest population",cex=0.55, col=colors[1])
text(4,0.10,"largest population",cex=0.55, col=colors[10])
mtext(side=2,"Catch composition",line=3.5)
mtext(side=2,"uncertainty (CV)",adj=0.5,line=2.5)
mtext(side=1,"Number of samples",line=0.5,outer=T)
box(col="grey")

### weekly sample
for(i in 1:10){
  plot(comp.var[i,6:10],type="l",ylim=c(0,1.6),axes=FALSE,xlab="",ylab="",col=colors[i])
  par(new=TRUE)
}
plot(comp.equal[,2],type="l",las=1,xlab="",ylab="",lwd=2,ylim=c(0,1.6),axes=FALSE)   
text(2.5,1.55,"(b) weekly sampling",cex=0.8)

axis(1,at=c(1,2,3,4,5),labels=samples,cex.axis=0.9)
axis(2,las=1,at=c(0,0.5,1.0,1.5,2.0),labels=FALSE)
box(col="grey")

dev.off()  

#########################################################
#### plotting harvest rate data

harv.equal<-matrix(nrow=5,ncol=10)

rate.yr1.0
rate.yr3.0

for(i in 1:5){
harv.equal[i,1]<-mean(rate.yr1.0[,4,i])
harv.equal[i,2]<-mean(rate.yr3.0[,4,i])
harv.equal[i,3]<-mean(rate.yr1.1[,4,i])
harv.equal[i,4]<-mean(rate.yr3.1[,4,i])
harv.equal[i,5]<-mean(rate.yr1.2[,4,i])
harv.equal[i,6]<-mean(rate.yr3.2[,4,i])
harv.equal[i,7]<-mean(rate.yr1.3[,4,i])
harv.equal[i,8]<-mean(rate.yr3.3[,4,i])
harv.equal[i,9]<-mean(rate.yr1.4[,4,i])
harv.equal[i,10]<-mean(rate.yr3.4[,4,i])
}

harv.var<-array(dim=c(5,15,2))

harv.var[,,1]                
                
                
for(i in 1:5){
  harv.var[i,1,1]<-rate.yr2.0[1,4,i]
  harv.var[i,2,1]<-rate.yr2.0[9,4,i]
  harv.var[i,3,1]<-mean(rate.yr2.0[,4,i])
  harv.var[i,4,1]<-rate.yr2.1[1,4,i]
  harv.var[i,5,1]<-rate.yr2.1[9,4,i]
  harv.var[i,6,1]<-mean(rate.yr2.1[,4,i])
  harv.var[i,7,1]<-rate.yr2.2[1,4,i]
  harv.var[i,8,1]<-rate.yr2.2[9,4,i]
  harv.var[i,9,1]<-mean(rate.yr2.2[,4,i])
  harv.var[i,10,1]<-rate.yr2.3[1,4,i]
  harv.var[i,11,1]<-rate.yr2.3[9,4,i]
  harv.var[i,12,1]<-mean(rate.yr2.3[,4,i]) 
  harv.var[i,13,1]<-rate.yr2.4[1,4,i]
  harv.var[i,14,1]<-rate.yr2.4[9,4,i]
  harv.var[i,15,1]<-mean(rate.yr2.4[,4,i]) 

}


for(i in 1:5){
  harv.var[i,1,2]<-rate.yr4.0[1,4,i]
  harv.var[i,2,2]<-rate.yr4.0[9,4,i]
  harv.var[i,3,2]<-mean(rate.yr4.0[,4,i])
  harv.var[i,4,2]<-rate.yr4.1[1,4,i]
  harv.var[i,5,2]<-rate.yr4.1[9,4,i]
  harv.var[i,6,2]<-mean(rate.yr4.1[,4,i])
  harv.var[i,7,2]<-rate.yr4.2[1,4,i]
  harv.var[i,8,2]<-rate.yr4.2[9,4,i]
  harv.var[i,9,2]<-mean(rate.yr4.2[,4,i])
  harv.var[i,10,2]<-rate.yr4.3[1,4,i]
  harv.var[i,11,2]<-rate.yr4.3[9,4,i]
  harv.var[i,12,2]<-mean(rate.yr4.3[,4,i]) 
  harv.var[i,13,2]<-rate.yr4.4[1,4,i]
  harv.var[i,14,2]<-rate.yr4.4[9,4,i]
  harv.var[i,15,2]<-mean(rate.yr4.4[,4,i]) 
  
}


jpeg(filename = "figures/figure_7.jpeg",
     width = 2400, height = 2400, units = "px", pointsize = 9,
     bg = "white", res = 600)

colors<-viridis(5)
colors1<-viridis(3)
colors2<-viridis(3, alpha=0.2)


single<-c(1,3,5,7,9)
week<-c(2,4,6,8,10)

par(mfrow=c(2,2))
par(mar=c(0,1,1,0),oma=c(4,4,1,1))

for(i in 1:5){
  plot(harv.equal[,single[i]],type="l",ylim=c(0,1.5),xlim=c(1,5),axes=FALSE,xlab="",ylab="",col=colors[i],lwd=2)
  par(new=TRUE)

}
plot(1,cex=0,axes=FALSE,xlab="",ylab="",ylim=c(0,1.5),xlim=c(1,5))
axis(1,at=c(1,2,3,4,5),labels=FALSE)
axis(2,las=2,at=c(0,0.5,1,1.5,2))
mtext(side=3,"Annual sampling", cex=1,adj=0.5)
text(1.25,1.45,"(a)")
mtext(side=2,"Harvest rate uncertianty (CV)",line=2.5,outer=T)
box(col="grey")

legend("topright", title="",pch=15,c("No observation error","Catch CV = 0.2; Size CV = 0.2","Catch CV = 0.2; Size CV = 0.5","Catch CV = 0.5; Size CV = 0.2","Catch CV = 0.5; Size CV = 0.5"),col=colors,cex=0.70,inset=0.03,bty="n")

for(i in 1:5){
  plot(harv.equal[,week[i]],type="l",ylim=c(0,1.5),xlim=c(1,5),axes=FALSE,xlab="",ylab="",col=colors[i],lwd=2)
  par(new=TRUE)
  
}
plot(1,cex=0,axes=FALSE,xlab="",ylab="",ylim=c(0,1.5),xlim=c(1,5))
axis(1,at=c(1,2,3,4,5),labels=FALSE)
axis(2,las=2,at=c(0,0.5,1,1.5,2),labels=FALSE)
mtext(side=3,"Weekly sampling", cex=1,adj=0.5)
text(1.25,1.45,"(b)")
box(col="grey")

#### annual sampling variable population size
gr<-c(1,4,13)

for (i in 1:3){
  for(j in 0:1){
    plot(harv.var[,gr[i]+j,1],ylim=c(0,1.8),type="l",col=colors1[i],axes=FALSE,xlab="",ylab="",lty=2)
    par(new=TRUE)
    plot(harv.var[,gr[i]+2,1],ylim=c(0,1.8),type="l",col=colors1[i],axes=FALSE,xlab="",ylab="",lwd=2)
    polygon(c(seq(1,5),rev(seq(1,5))),c(harv.var[,gr[i]+j,1],rev(harv.var[,gr[i]+2,1])),col=colors2[i],border=NA)
    par(new=TRUE)
    
    }
}

harv.var

plot(1,cex=0,axes=FALSE,xlab="",ylab="",ylim=c(0,1.8),xlim=c(1,5))
axis(1,at=c(1,2,3,4,5),labels=c(60,120,240,360,600))
axis(2,las=2,at=c(0,0.5,1,1.5,2),labels=c(0,0.5,1,1.5,2))
text(1.25,1.75,"(c)")
mtext(side=1,"Number of samples",outer=T,line=2.5)
box(col="grey")

#### weekly sampling variable population size
for (i in 1:3){
  for(j in 0:1){
    plot(harv.var[,gr[i]+j,2],ylim=c(0,1.8),type="l",col=colors1[i],axes=FALSE,xlab="",ylab="",lty=2)
    par(new=TRUE)
    plot(harv.var[,gr[i]+2,2],ylim=c(0,1.8),type="l",col=colors1[i],axes=FALSE,xlab="",ylab="",lwd=2)
    polygon(c(seq(1,5),rev(seq(1,5))),c(harv.var[,gr[i]+j,2],rev(harv.var[,gr[i]+2,2])),col=colors2[i],border=NA)
    par(new=TRUE)
  }
}

plot(1,cex=0,axes=FALSE,xlab="",ylab="",ylim=c(0,1.8),xlim=c(1,5))
axis(1,at=c(1,2,3,4,5),labels=c(60,120,240,360,600))
axis(2,las=2,at=c(0,0.5,1,1.5,2),labels=FALSE)
text(1.25,1.75,"(d)")
box(col="grey")

dev.off()






