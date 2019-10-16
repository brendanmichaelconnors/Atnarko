# read in posteriors and simulation output

#2 x catch uncertianty at end of time series
forward_res <- readRDS("outputs/forward_sims.static_out.2timesmixedstock.May142019")
outcomes.res.1 <- apply(forward_res,c(1,2,4),mean,na.rm=T)

#2 x catch uncertianty at end of time series
forward_res <- readRDS("outputs/forward_sims.static_out.2timesuncertain.May142019")
outcomes.res.2 <- apply(forward_res,c(1,2,4),mean,na.rm=T)


trans_col.2<-rgb(0,255,0,max=255,alpha=35)
trans_col.3<-rgb(255,0,0,max=255,alpha=35)

harvest_rate <- seq(0,0.4,length.out=21)

# generate plot
jpeg("figures/figure_S1.jpeg",width=7, height=3.5,units="in",res=400,bg = "transparent")
#dev.new(width=8, height=4)
par(new=T)

split.screen(rbind(
c(0,0.5,0,1), #1
c(0.5,1,0,1) #2
))

screen(1)

par(mar=c(3,3,1,0),oma=c(1,1,0.5,0.25))

plot(harvest_rate ,outcomes.res.1[,2,]*100,yaxt="n",ylab=,xaxt="n",ylim=c(0,100),type="l",lwd=2,lty=3,col="red")
points(harvest_rate ,outcomes.res.1[,1,]*100,lwd=2,type="l",lty=2,col="dark green")

polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res.1[,2,]*100,rev(outcomes.res.1[,1,]*100)),col=trans_col.3,border=NA)
polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res.1[,1,]*100,seq(0,0,length.out=length(outcomes.res.1[,1,]*100))),col=trans_col.2,border=NA)

axis(2,las=2, cex.axis =0.9)
axis(1,at=c(0,0.1,0.2,0.3,0.4),labels=c("0","10","20","30","40"), cex.axis =0.9)
mtext("Chance of exceeding limit ",2,line=3.15)
mtext("reference point / recovery goal (%)",2,line=2.25)
mtext("Harvest rate (%)",1,line=2)
text(0.015,96,"(a)")
box(col="grey")


screen(2)

par(mar=c(3,3,1,0),oma=c(1,1,0.5,0.25))

plot(harvest_rate,outcomes.res.2[,2,]*100,yaxt="n",ylab=,xaxt="n",ylim=c(0,100),type="l",lwd=2,lty=3,col="red")
points(harvest_rate,outcomes.res.2[,1,]*100,lwd=2,type="l",lty=2,col="dark green")

polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res.2[,2,]*100,rev(outcomes.res.2[,1,]*100)),col=trans_col.3,border=NA)
polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res.2[,1,]*100,seq(0,0,length.out=length(outcomes.res.2[,1,]*100))),col=trans_col.2,border=NA)


axis(2,las=2, cex.axis =0.9)
axis(1,at=c(0,0.1,0.2,0.3,0.4),labels=c("0","10","20","30","40"), cex.axis =0.9)
mtext("Harvest rate (%)",1,line=2)
text(0.015,96,"(b)")
box(col="grey")

dev.off()