data<-read.table("data/catch_data.txt",header=T)
dat.matrix<-read.table("data/catch_matrix.txt", header=T)
mat.short<-read.table("data/catch_matrix.short.txt", header=F)

mar.short<-as.matrix(mat.short[,2:44])

tot.inc<-seq(0,120000,15000)
labs<-c(0,15,30,45,60,75,90,105,120)
count<-seq(0,40,5)
years<-seq(1975,2020,5)

jpeg(filename = "figures/Figure_2.jpeg", width = 7, height = 3.5, units = "in", bg = "white", res = 400)
#dev.new(width=7, height=3.5)

par(mar=c(4,4,1,4))
b<-barplot(mar.short,yaxt="n",xaxt="n",xlab="",ylab="",ylim=c(0,120000))
axis(side=2, las=2, at=tot.inc, labels=labs,line=-0.25)
mtext("Sockeye (000s)",side=2,adj=0.5,line=2.5)
axis(side=1, at=c(0.7,6.7,12.7,18.7,24.7,30.7,36.7,42.7,48.7), labels=c(1975,1980,1985,1990,1995,2000,2005,2010,2015))
mtext("Year",side=1,line=2.5)

par(new=TRUE,mar=c(4,4.25,1,4.25))
plot(harvest_rate~year, xlab="",ylab="",data=data,yaxt="n",xaxt="n", col="red",type="l",lwd=2,bty="l", ylim=c(0,1))
axis(side=4,at=c(0,0.2,0.4,0.6,0.8,1),labels=c("0","20","40","60","80","100"),las=1)
mtext("Harvest (%)",side=4,adj=0.5,line=2.5)


colors<-gray.colors(3, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)
legend(2005,0.9,c("Escapement","Commercial","FSC"),col=colors,pch=15,cex=0.9,bty="n")

dev.off()





