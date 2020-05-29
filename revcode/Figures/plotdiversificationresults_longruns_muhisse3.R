library("ggplot2")
library("wesanderson")
library("viridis")
library("inlmisc")
~/Dropbox/nestdivmodel/revcode/results/MuHiSSE_threestate.log
### Colors
cols<-c(wes_palette("Zissou1"),wes_palette("Darjeeling1"),wes_palette("GrandBudapest2"))
cols2<-c(inlmisc::GetColors(9, scheme = "muted"),inlmisc::GetColors(9, scheme = "light"))

mypalette<-c("#fee090","#ffffbf","#313695","#abd9e9","#a50026","#d73027")





#mypalette<-c(cols[3],cols[1],cols[5],cols[7],cols[9],cols[11],cols[12],"darkgoldenrod1","blue3","firebrick3")
#cols2<- c("#7b3294","#c2a5cf","#a6dba0","#008837","#ffffbf")
#cols3<-c("#ece2f0", "#67a9cf","#e31a1c","#fd8d3c","#02818a","#014636")
#cols4<-c(cols[4],"#fdb863","#542788","#b2abd2","#f7f7f7",cols[2])
#sampling<-seq(1,250000,100)

setwd("~/Dropbox/nestdivmodel/revcode/Figures")
source("multiplot.R")

### Muhisse 3 state without restrictions

 ### Plots for diversification rates MuHiSSE 3-state
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuHiSSE_threestate.log", header=TRUE)
output.sse<-output.sse[-seq(1,1000,1),] #cutting burn-in- 12407 MCMC steps


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("Dome A","Cup A","Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5,output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate2)))

p1.1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)

p2.1<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette)

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)

p4.1<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)+xlim(0,0.5)

p5.1<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.1<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1.1,p3.1,p5.1,p2.1,p4.1,p6.1, cols=2)





### Muhisse 3 state with minimum 0.8 extinction and extra with exponential with parameter 0.5

 ### Plots for diversification rates MuHiSSE 3-state
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/MuHiSSE3min_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,1000,1),] #cutting burn-in- 12407 MCMC steps


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("Dome A","Cup A","Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5,output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate2)))

p1.2<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)

p2.2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette)

p3.2<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)

p4.2<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette)+xlim(0.8,1.2)

p5.2<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.2<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1.2,p3.2,p5.2,p2.2,p4.2,p6.2, cols=2)

