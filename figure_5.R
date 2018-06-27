colors<-c("magenta","mediumorchid","chartreuse3","cyan4","royalblue3")#"red")

jpeg(filename = "figures/Figure_5.jpeg",
     width = 5, height = 5, units = "in",
     bg = "white", res = 400)
#dev.new(width=5, height=5)

m<-rbind(c(1,1,1,1),c(1,1,1,1),c(2,2,3,3),c(2,2,3,3))
layout(m)
par(oma=c(3,3,1,1),mar=c(2,1,0,0))

#plot(1:n.pops,harv.rate2,xaxt="n",pch=2,cex=1.2,xlim=c(0,11),ylim=c(0,0.6),bty="l",ylab="Harvest rate",xlab="Population size")
#par(new=TRUE)

plotCI(seq(0.8,1.2,0.1),catch_CIs.V[,1],li=catch_CIs.V[,3],ui=catch_CIs.V[,4],col=colors,xlim=c(0.5,2.5),ylim=c(-0.2,2.5),bty="l",
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="", cex=1.2, lwd=2,pch=16)
par(new=TRUE)
plotCI(seq(1.8,2.2,0.1),catch_CIs.Vw[,1],li=catch_CIs.Vw[,3],ui=catch_CIs.Vw[,4],col=colors,xlim=c(0.5,2.5),ylim=c(-0.2,2.5),bty="l",
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="", cex=1.2, lwd=2,pch=16)

mtext("Estimated harvest rate (%)", side=2, adj=0.5, line=2.5)
text(0.5,2.29,"(a)",cex=1.2)
axis(2,las=1,at=c(0,0.5,1,1.5,2,2.5),labels=c("0","5","10","15","20","25"))
axis(1,at=c(1,2),labels=c("Annual sample", "Weekly sample"),las=1)
legend("topright", title="Total samples",pch=16,c("60","120","240","360","600"),col=colors,cex=1,inset=0.01,bty="n")

##############
#### plotting weekly versus annual for all pops

S.weekly<-c(60,120,240,360,600)
S.annual<-c(60,120,240,360,600)

#par(mfrow=c(1,2))
#par(oma=c(3,3,1,1),mar=c(2,1,0,0))

plot(S.weekly,mean.W,type="l",xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="CV",xlab="Sample no.",col="black",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.weekly,mean.W,pch=16,xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="CV",xlab="Sample no.",col="black",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.annual,mean.10,type="l",xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="gray50",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.annual,mean.10,pch=17,xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="gray50",lwd=2,las=1,axes=FALSE)

legend("topright", title="",pch=c(16,17),c("Weekly","Annual"),col=c("black","gray50"),cex=1,inset=0.1,bty="n")
text(20,0.55,"(b)",cex=1.2)
text(180,0,"Population size = constant",cex=0.8,font=3)
axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6), labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),las=2)
axis(1)
mtext("CV", side=2,adj=0.5,line=2.5)

plot(S.weekly,mean.Wv,type="l",xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="black",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.weekly,mean.Wv,pch=16,xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="black",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.annual,mean.10v,type="l",xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="gray50",lwd=2,las=1,axes=FALSE)
par(new=TRUE)
plot(S.annual,mean.10v,pch=17,xlim=c(0,600),ylim=c(0,0.6),bty="l",ylab="",xlab="",col="gray50",lwd=2,las=1,axes=FALSE)

text(180,0,"Population size = variable",cex=0.8,font=3)
text(20,0.55,"(c)",cex=1.2)
axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6), labels=FALSE)
axis(1)
mtext("Number of samples", side=1,adj=-1.1,line=2.5)

dev.off()