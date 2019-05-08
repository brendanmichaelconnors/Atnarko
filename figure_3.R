
age_file <-read.delim("data/age_comps.updatedFeb282019.txt",header=T)
colnames(age_file) <- c("year","3","4","5","6","samples")
xxx <- age_file[,-6]
age_comp <- gather(xxx,age,proportion,c("3","4","5","6"))
age_file$samples

jpeg("figures/Figure_3.jpeg",width=6, height=2.5, units="in",res=800)

ggplot() +
	geom_bar(aes(x=year,y=proportion*100,fill=age), data = age_comp, stat="identity")+
	scale_fill_manual(values=viridis(4))+
	annotate("text", x = 1975:2017, y = 105, label = age_file$samples,size=2.35,angle = 45)+
	coord_cartesian(ylim=c(4,105),xlim=c(1975,2016))+
	scale_x_continuous("Year",breaks = c(1975,1985,1995,2005,2015))+ 
	scale_y_continuous("Percent of samples",breaks = c(0,20,40,60,80,100))+
	theme_bw()+
	theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"),	
			 legend.key.size=unit(0.5,"cm"),
			 panel.border = element_rect(color = "grey", fill=NA, size=0.75))

dev.off()
