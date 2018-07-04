# a little housekeeping
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colorss <- jet.colors(21)
legend_image <- as.raster(matrix(rev(jet.colors(21)),ncol=1))

years <- seq(2005,2017)

xx <- apply(posteriors,2,quantile,probs=c(0.025,0.5,0.975))
nms = names(posteriors)
yy <- posteriors[substr(nms, 1, 2) == "S."]
spw <- yy[,4:46]
S <- spw[1,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
S.summ <- apply(S,2,quantile,probs=c(0.025,0.5,0.975))

S.summ[2,43] <- outcomes.1[7,1,1]

# generate plot
jpeg("figures/Figure_4.new.jpeg",width=8, height=6,units="in",res=400,bg = "transparent")
#dev.new(width=8, height=6)
par(new=T)

split.screen(rbind(
c(0,0.5,0.5,1), #1
c(0.5,1,0.5,1), #1
c(0,0.5,0,0.5), #2
c(0.5,1,0,0.5) #3

))

screen(1)

par(mar=c(0.5,3,1,0.8),oma=c(5,2,0.5,0.5))

plot(years,S.summ[2,31:43],type="l",lwd=3,ylim=c(0,100000),xlim=c(2010,2037),yaxt="n",xaxt="n")
axis(2,las=2,at=c(0,20000,40000,60000,80000,100000),labels=c("0","20","40","60","80","100"))
axis(1,at=c(2005,2015,2025,2035))

mtext("Spawners (000s)",2,line=2.9,cex=1)
mtext("Year",1,line=2.1,cex=1,adj=1.2)

ablineclip(lty=2,col="dark green",x1=2017,x2=2037,y1=15000,h=15000,lwd=2)
ablineclip(h=4000,lty=3,col="red",x1=2017,x2=2037,lwd=2)

trans_col.1<-rgb(0,0,127,max=255,alpha=45)
polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower[7:27,1,1],rev(outcomes.1.upper[7:27,1,1])),col=trans_col.1,border=NA)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower25[7:27,1,1],rev(outcomes.1.upper75[7:27,1,1])),col=trans_col.1,border=NA)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower10[7:27,1,1],rev(outcomes.1.upper90[7:27,1,1])),col=trans_col.1,border=NA)

points(seq(2017,2037),outcomes.1[7:27,1,1],type="l",col=colorss[1],lwd=3)

text(2009,96000,"(a)",pos=4)

legend(2009,90000,legend=c("Recovery goal","Limit reference point"),lty=c(2,3),lwd=2,col=c("dark green","red"),text.col=c("dark green","red"),bty="n",cex=0.7)

screen(2)

par(mar=c(0.5,3,1,0.8),oma=c(5,2,0.5,0.5))

plot(years,S.summ[2,31:43],type="l",lwd=3,ylim=c(0,100000),xlim=c(2010,2037),yaxt="n",xaxt="n")

axis(2,las=2,at=c(0,20000,40000,60000,80000,100000),labels=c("0","20","40","60","80","100"))
axis(1,at=c(2005,2015,2025,2035))


ablineclip(lty=2,col="dark green",x1=2017,x2=2037,y1=15000,h=15000,lwd=2)
ablineclip(h=4000,lty=3,col="red",x1=2017,x2=2037,lwd=2)

trans_col.2<-rgb(127,0,0,max=255,alpha=45)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower[7:27,1,21],rev(outcomes.1.upper[7:27,1,21])),col=trans_col.2,border=NA)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower25[7:27,1,21],rev(outcomes.1.upper75[7:27,1,21])),col=trans_col.2,border=NA)

polygon(c(seq(2017,2037),rev(seq(2017,2037))),c(outcomes.1.lower10[7:27,1,21],rev(outcomes.1.upper90[7:27,1,21])),col=trans_col.2,border=NA)

points(seq(2017,2037),outcomes.1[7:27,1,21],type="l",col=colorss[21],lwd=3)

text(2009,96000,"(b)",pos=4)

screen(3)

par(mar=c(3.5,4.5,1,0.8),oma=c(0.5,0.5,0.5,0.5))

b<-barplot(outcomes.4[,2,]*100,yaxt="n",ylab=,xaxt="n",col=colorss,ylim=c(0,115))
axis(2,las=2)
axis(1,at=c(b[1],b[6],b[11],b[16],b[21]),labels=c("0","10","20","30","40"))
mtext("Chance of exceeding",2,line=3.75)
mtext("limit reference point (%)",2,line=2.75)
mtext("     Harvest rate (%)",1,line=-1,outer=T)
text(0.5,110,"(c)")

screen(4)
par(mar=c(3.5,4.5,1,0.8),oma=c(0.5,0.5,0.5,0.5))

b<-barplot(outcomes.4[,1,]*100,yaxt="n",ylab=,xaxt="n",col=colorss,ylim=c(0,115))
axis(2,las=2)
axis(1,at=c(b[1],b[6],b[11],b[16],b[21]),labels=c("0","10","20","30","40"))
mtext("Chance of exceeding",2,line=3.75)
mtext("recovery goal (%)",2,line=2.75)
text(0.5,110,"(d)")

rasterImage(legend_image,16.5,65,19,90)
text(18.5,65,"0%",pos=4,cex=0.8)
text(18.5,77.5,"20%",pos=4,cex=0.8)
text(18.5,89,"40%",pos=4,cex=0.8)
text(14.,98,"Harvest rate",pos=4,cex=0.8)

dev.off()