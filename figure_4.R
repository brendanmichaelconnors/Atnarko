# load posteriors
sr_model.mcmc <- readRDS("outputs/Atnarko_posteriors.baseline.May142019.mcmc")
posteriors = as.matrix(sr_model.mcmc, chain=F)
R = post.summ(sr_model.mcmc, "R[")
S = post.summ(sr_model.mcmc, "S[")
resid = post.summ(sr_model.mcmc, "log.resid[")

# re-index spawners, recruits, etc. 
S <- S[,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
R <- R[,c(1,12,23,34,42,43,44,45,46,2:11,13:22,24:33,35:41)]
resid <- resid[,c(1,12,23,34,36:39,2:11,13:22,24:33,35)]

Y = 43             # number of calendar years observed
a.min = 3                 # minimum age class in data set
a.max = 6                 # maximum age class in data set
A = a.max - a.min + 1     # number of age classes
nRyrs = Y + A - 1         # number of recruitment years (see model code for details)

#color pallette

point_col <- viridis(40)

jpeg("figures/figure_4.jpg",width=8, height=3,units="in",res=400,bg = "transparent")
#dev.new(width=8, height=3)

split.screen(rbind(
c(0,0.40,0,1), #1
c(0.25,0.35,0.8,0.9), #2
c(0.40,1,0,1) #2
))

screen(1)

par(mar=c(3,4,1,0.8),oma=c(0.5,0.5,0.5,0.5))


plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=R[5,(a.max + 1):nRyrs]/1000,li=R[4,(a.max + 1):nRyrs]/1000,pch=16,col="white",yaxt="n",xaxt="n",sfrac=0,gap=0,ylab="Recruits (000s)",xlab="Spawners (000s)",xlim=c(0,65),ylim=c(0,123))
axis(2,las=2)
axis(1,las=1,at=c(0,15,30,45,60))

spw=seq(0,70000,length.out=100)
SR_pred <-matrix(NA,100,10000)

for(i in 1:10000){
	r <- sample(seq(1,10000),1,replace=T)
	a <- posteriors[r,1]
	b <- posteriors[r,2]
	SR_pred[,i] <- (a*spw*exp(-b*spw))/1000
}

SR_pred_int <- apply(SR_pred,c(1),quantile,probs=c(0.1,0.5,0.9),na.rm=T)
polygon(c(spw/1000,rev(spw/1000)),c(SR_pred_int[1,],rev(SR_pred_int[3,])),col=adjustcolor( "grey", alpha.f = 0.3),lty=2)

plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=R[5,(a.max + 1):nRyrs]/1000,li=R[4,(a.max + 1):nRyrs]/1000,pch=16,,col= point_col,err="y",yaxt="n",xaxt="n",sfrac=0,gap=0,add=T, barcol = point_col,cex=0.7)
plotCI(S[3,1:(Y-a.min)]/1000,R[3,(a.max + 1):nRyrs]/1000,ui=S[5,1:(Y-a.min)]/1000,li=S[4,1:(Y-a.min)]/1000,pch=16,col= point_col,err="x",yaxt="n",xaxt="n",sfrac=0,gap=0,add=T,barcol = point_col,cex=0.7)

points(spw/1000, SR_pred_int[2,],type="l",lwd=2)


text(2,119,"(a)")
mtext("Spawners (000s)",1,line=2.2)
box(col="grey")

screen(2)
par(mar=c(1,1,0,0))

legend.scale(c(0,1),col= point_col,axis.args = list(at = c(0, 1), las = 1, labels =c("1973","2011"),cex.axis=0.55, tck=-0.25,padj=-3.5))

screen(3)

par( mar=c(3,4,1,0.8),oma=c(0.5,0.5,0.5,0.5))

plot(seq(1973,2011),exp(resid[3,]+ 1.052617),type="l",ylim=c(0,20),yaxt="n",ylab="")
polygon(c(seq(1973,2011),rev(seq(1973,2011))),c(exp(resid[5,]+ 1.052617),rev(exp(resid[4,]+ 1.052617))),col=adjustcolor( "grey", alpha.f = 0.3),lty=2,border=F)
points(seq(1973,2011),exp(resid[3,]+ 1.052617),type="l",lwd=2)
axis(2,las=2)
mtext("Brood year",1,line=2.2)
mtext("Productivity",2,line=3)
mtext("(recruits/spawner)",2.1,line=2)
text(1973,19.5,"(b)")
abline(h=1,lty=2)
box(col="grey")

dev.off()








