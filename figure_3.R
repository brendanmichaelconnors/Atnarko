jpeg("figure_3.jpg",width=8, height=3,units="in",res=400,bg = "transparent")
#dev.new(width=8, height=3)

split.screen(rbind(
c(0,0.40,0,1), #1
c(0.40,1,0,1) #2
))

screen(1)

par(mar=c(3,4,1,0.8),oma=c(0.5,0.5,0.5,0.5))


plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=R[5,(a.max + 1):nRyrs]/1000,li=R[4,(a.max + 1):nRyrs]/1000,pch=16,col="white",yaxt="n",xaxt="n",sfrac=0,ylab="Recruits (000s)",xlab="Spawners (000s)",xlim=c(0,65),ylim=c(0,123))
axis(2,las=2)
axis(1,las=1,at=c(0,15,30,45,60))

spw=seq(0,70000,length.out=100)

for(i in 1:100){
	r <- sample(seq(1,10000),1,replace=T)
	a <- posteriors[r,2]
	b <- posteriors[r,3]
	points(spw/1000,(a*spw*exp(-b*spw))/1000,type="l",col=grey(0.8))
}

plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=R[5,(a.max + 1):nRyrs]/1000,li=R[4,(a.max + 1):nRyrs]/1000,pch=16,col=grey(0.2),err="y",yaxt="n",xaxt="n",sfrac=0,add=T)
plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=S[5,1:(Y-a.min)]/1000,li=S[4,1:(Y-a.min)]/1000,pch=16,col=grey(0.2),err="x",yaxt="n",xaxt="n",sfrac=0,add=T)
text(2,119,"(a)")
mtext("Spawners (000s)",1,line=2.2)

screen(2)

par( mar=c(3,4,1,0.8),oma=c(0.5,0.5,0.5,0.5))

plot(seq(1973,2011),resid[3,]+0.9282193,type="l",ylim=c(-4,3),yaxt="n",ylab="")
polygon(c(seq(1973,2011),rev(seq(1973,2011))),c(resid[5,]+0.9282193,rev(resid[4,]+0.9282193)),col="light grey",border=NA)
points(seq(1973,2011),resid[3,]+0.9282193,type="l",lwd=2)
axis(2,las=2)
mtext("Brood year",1,line=2.2)
mtext("Productivity index",2,line=2.5)
text(1973,2.8,"(b)")

dev.off()








