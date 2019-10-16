# read in posteriors and simulation output

#BASELINE
sr_model.mcmc <- readRDS("outputs/Atnarko_posteriors.baseline.May142019.mcmc")
forward_sims <- readRDS("outputs/forward_sims.temp_out.baseline.May142019")
forward_res <- readRDS("outputs/forward_sims.static_out.baseline.May142019")

posteriors = as.matrix(sr_model.mcmc, chain=F)

outcomes.sims <- apply(forward_sims,c(1,2,4),quantile,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=T)
outcomes.res <- apply(forward_res,c(1,2,4),mean,na.rm=T)

# some housekeeping
years <- seq(2005,2017)

nms = dimnames(posteriors)
#yy <- posteriors[substr(nms, 1, 2) == "S["]
#spw <- yy[,4:46]

spw <- posteriors[,593:635]

S <- spw[,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
S.summ <- apply(S,2,quantile,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

S.summ[5,43] <- outcomes.sims[5,7,1,1]

trans_col.1<-rgb(0,0,255,max=255,alpha=15)
trans_col.2<-rgb(0,255,0,max=255,alpha=35)
trans_col.3<-rgb(255,0,0,max=255,alpha=35)

harvest_rate <- seq(0,0.4,length.out=21)

# generate plot
jpeg("figures/figure_5.jpeg",width=8, height=3.5,units="in",res=400,bg = "transparent")
#dev.new(width=8, height=4)
par(new=T)

split.screen(rbind(
c(0,0.55,0,1), #1
c(0.57,1,0,1) #2
))

screen(1)

par(mar=c(3,3,1,0.8),oma=c(1,0.5,0.5,0.5))

plot(years,S.summ[5,31:43],type="l",lwd=2,ylim=c(0,100000),xlim=c(2010,2036.5),yaxt="n",xaxt="n")
axis(2,las=2,at=c(0,20000,40000,60000,80000,100000),labels=c("0","20","40","60","80","100"),cex.axis=0.9)
axis(1,at=c(2005,2015,2025,2035), cex.axis =0.9)

mtext("Spawners (000s)",2,line=2.2,cex=1)
mtext("Year",1,line=2.1,cex=1)

ablineclip(lty=2,col="dark green",x1=2017,x2=2037,y1=15000,h=15000,lwd=2)
ablineclip(h=4000,lty=3,col="red",x1=2017,x2=2037,lwd=2)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.sims[1,7:27,1,10],rev(outcomes.sims[9,7:27,1,10])),col=trans_col.1,border=NA)
polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.sims[2,7:27,1,10],rev(outcomes.sims[8,7:27,1,10])),col=trans_col.1,border=NA)
polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.sims[3,7:27,1,10],rev(outcomes.sims[7,7:27,1,10])),col=trans_col.1,border=NA)
polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.sims[4,7:27,1,10],rev(outcomes.sims[9,7:27,1,10])),col=trans_col.1,border=NA)


points(seq(2017,2037),outcomes.sims[5,7:27,1,10],type="l",col="blue",lwd=2)

polygon(c(years,rev(years)),c(S.summ[1,31:43],rev(S.summ[9,31:43])),col=grey(0.8),border=NA)
polygon(c(years,rev(years)),c(S.summ[2,31:43],rev(S.summ[8,31:43])),col=grey(0.7),border=NA)
polygon(c(years,rev(years)),c(S.summ[3,31:43],rev(S.summ[7,31:43])),col=grey(0.6),border=NA)
polygon(c(years,rev(years)),c(S.summ[4,31:43],rev(S.summ[6,31:43])),col=grey(0.5),border=NA)
points(years,S.summ[5,31:43],type="l",lwd=2)

text(2009,96000,"(a)",pos=4)

legend(2009,90000,legend=c("Recovery goal","Limit reference point"),lty=c(2,3),lwd=2,col=c("dark green","red"),text.col=c("dark green","red"),bty="n",cex=0.8)

box(col="grey")

screen(2)

par(mar=c(3,3,1,0),oma=c(1,1,0.5,0.25))

plot(harvest_rate ,outcomes.res[,2,]*100,yaxt="n",ylab=,xaxt="n",ylim=c(0,100),type="l",lwd=2,lty=3,col="red")
points(harvest_rate ,outcomes.res[,1,]*100,lwd=2,type="l",lty=2,col="dark green")

polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res[,2,]*100,rev(outcomes.res[,1,]*100)),col=trans_col.3,border=NA)
polygon(c(harvest_rate,rev(harvest_rate)),c(outcomes.res[,1,]*100,seq(0,0,length.out=length(outcomes.res[,1,]*100))),col=trans_col.2,border=NA)


axis(2,las=2, cex.axis =0.9)
axis(1,at=c(0,0.1,0.2,0.3,0.4),labels=c("0","10","20","30","40"), cex.axis =0.9)
mtext("Chance of exceeding limit ",2,line=3.35)
mtext("reference point / recovery goal (%)",2,line=2.35)
mtext("Harvest rate (%)",1,line=2)
text(0.015,96,"(b)")
box(col="grey")

dev.off()