library("ggplot2")
library("wesanderson")
library("viridis")

### Colors
cols<-wes_palette("Zissou1")

mypalette<-c(cols[3],cols[1],cols[5])
#cols2<- c("#7b3294","#c2a5cf","#a6dba0","#008837","#ffffbf")
#cols3<-c("#ece2f0", "#67a9cf","#e31a1c","#fd8d3c","#02818a","#014636")
#cols4<-c(cols[4],"#fdb863","#542788","#b2abd2","#f7f7f7",cols[2])
#sampling<-seq(1,250000,100)
### Plots for diversification rates Bisse DP with diploidization
setwd("~/Dropbox/nestdivmodel/revcode/Figures")
source("multiplot.R")
### First run in Rosana's laptop of MuSSE with 3 states
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE_threestate_ZFlaptop.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))
multiplot(p1,p3.1,p5,p2,p4, cols=2)

############################################################################################
### Cluster run 3 days about 72,000 generations

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE_threestate.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))
multiplot(p1,p3.1,p5,p2,p4, cols=2)


##########


############################################################################################
### Plots for diversification rates Hisse DP with diploidization~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_polydip250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_polydip250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$Q.1,output.sse$Q.2) ,Type=rep(c("Polyploidy","Diploidization"),each=length(output.sse$Q.1)))

hidden.rate<-data.frame(dens=output.sse$R1 ,Type=rep("AB=BA",length(output.sse$R1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))
multiplot(p1,p3.3,p5,p2,p4,p6, cols=2)
##########
##############################################################################

############################################################################################
### Plots for diversification rates Hisse DP with diploidization and asymmetrical hidden rates~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPasym100K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$Q.1,output.sse$Q.2) ,Type=rep(c("Diploidization","Polyploidy"),each=length(output.sse$Q.1)))

hidden.rate<-data.frame(dens=c(output.sse$R.1,output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p3.4<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-2,1.7)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,4)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.4,p5,p2,p4,p6, cols=2)
##########
############################################################################################


############################################################################################
### Plots for diversification rates Hisse DP with diploidization and asymmetrical rates for all ~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPallasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPallasym100K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12A, output.sse$rate_12B,output.sse$rate_12B,output.sse$rate_21B) ,Type=rep(c("Polyploidy A","Polyploidy B","Diploidization A","Diploidization B"),each=length(output.sse$rate_12A)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p3.5<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-2,1.7)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,4)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =cols4[c(1,2,5,6)])+xlim(0,0.25)

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.5,p5,p2,p4,p6, cols=2)
##########
############################################################################################

##############
### Plots for diversification rates Hisse DP without diploidization~/Dropbox/solploidypersonal/hissenodip250K/output/HiSSE_polynodip250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hissenodip250K/output/HiSSE_polynodip250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=(output.sse$Q.1) ,Type=rep("Polyploidization",length(output.sse$Q.1)))
hidden.rate<-data.frame(dens=output.sse$R.1 ,Type=rep("AB=BA",length(output.sse$rate_1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,2)

p3.6<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-0.5,1.5)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,2)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4]))+xlim(0,0.15)


p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))+xlim(0,0.15)
multiplot(p1,p3.6,p5,p2,p4,p6, cols=2)
##########

############################################################################################
### Plots for diversification rates Hisse DP without diploidization and asymmetrical hidden rates~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hissenodip250K/output/HiSSE_polynodipasymmetry250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=output.sse$Q.1 ,Type=rep("Polyploidy"),length(output.sse$Q.1))

hidden.rate<-data.frame(dens=c(output.sse$R.1,output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p3.7<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-2,1.7)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,4)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.7,p5,p2,p4,p6, cols=2)
##########
############################################################################################


############################################################################################
### Plots for diversification rates Hisse DP without diploidization and asymmetrical rates for all ~/Dropbox/solploidypersonal/hisse250K/output/HiSSE_DPallasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hissenodip250K/output/HiSSE_DPnodipallasym100K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("Diploid A","Polyploid_A", "Diploid_B", "Polyploid_B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12A, output.sse$rate_12B) ,Type=rep(c("Polyploidy A","Polyploidy B"),each=length(output.sse$rate_12A)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p3.8<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-2,2.3)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,4)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual(values =cols4[c(1,2)])+xlim(0,0.2)

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density") +geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.8,p5,p2,p4,p6, cols=2)
##########


############################################################################################
### Plots for diversification rates Bisse Breeding systems ~/Dropbox/solploidypersonal/bisseSInoreturn250K/output/BiSSE_sinoreturn250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/bisseSInoreturn250K/output/BiSSE_sinoreturn250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("SC","SI"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC","SI"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("C","I"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("SC","SI"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=output.sse$rate_21,Type=rep("SI to SC",length(output.sse$rate_21)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))


p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols2[1],cols2[4]))

p3.9<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols2[5]))
multiplot(p1,p3.9,p5,p2,p4, cols=2)
##########

############################################################################################
### Plots for diversification rates Bisse Breeding systems with return ~/Dropbox/solploidypersonal/bisseSI250K/output/BiSSE_selfincomp250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/bisseSI250K/output/BiSSE_selfincomp250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("SC","SI"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC","SI"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("C","I"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("SC","SI"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("SC to SI","SI to SC"),each=length(output.sse$rate_21)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[2],cols2[4]))


p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols2[2],cols2[4]))

p3.10<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[2],cols2[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[2],cols2[4]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols2[5]))
multiplot(p1,p3.10,p5,p2,p4, cols=2)
##########


############################################################################################
### Plots for diversification rates Hisse for breeding systems ~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoret250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoret250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=(output.sse$Q.2) ,Type=rep("SI to SC",length(output.sse$Q.2)))
hidden.rate<-data.frame(dens=output.sse$R.1 ,Type=rep("AB=BA",length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p3.11<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =  c(cols2[1],cols2[2],cols2[4],cols2[3]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[5]))


p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))
multiplot(p1,p3.11,p5,p2,p4,p6, cols=2)
##########
############################################################################################
### Plots for diversification rates Hisse IC and asymmetrical hidden rates~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoretasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoretasym100K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=output.sse$Q.2 ,Type=rep("SI to SC"),length(output.sse$Q.2))

hidden.rate<-data.frame(dens=c(output.sse$R.1,output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,3)

p3.12<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(-2,1.7)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,4)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))
multiplot(p1,p3.12,p5,p2,p4,p6, cols=2)
##########
############################################################################################


############################################################################################
### Plots for diversification rates Hisse IC and all asymmetrical rates~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoretasym100K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/hisseSInoreturn250K/output/HiSSE_sinoretallasym100K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_21A,output.sse$rate_21B) ,Type=rep(c("SI to SC A","SI to SC B")),each=length(output.sse$rate_21A))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,3)

p3.13<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(-1.5,2)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))+xlim(0,3)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual(values =c("yellow",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))
multiplot(p1,p3.13,p5,p2,p4,p6, cols=2)
##########
##############################################################################


############################################################################################~
### Plots for diversification rates Musse DP and breeding systems~/Dropbox/solploidypersonal/mussefull250k/output/MuSSE_ploidysi250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/mussefull250k/output/MuSSE_ploidysi250K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","Diploidization","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p3.14<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[2],cols[4],"pink",cols2[5]))

multiplot(p1,p3.14,p5,p2,p4, cols=2) 
#################################################################################################
### Plots for diversification rates Musse DP and breeding systems no diploidization~/Dropbox/solploidypersonal/mussenodip250k/output/MuSSE_ploidysi250K.log

output.sse<-read.table("~/Dropbox/solploidypersonal/mussenodip250k/output/MuSSE_ploidysinodip250K.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("SC-Diploid","Polyploid", "SI-Diploid"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p3.15<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

multiplot(p1,p3.15,p5,p2,p4, cols=2)
#########


############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems~/Dropbox/solploidypersonal/muhisse250k/output/MuHiSSE_ploidysi250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/muhisse250k/output/MuHiSSE_ploidysi250K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","Diploidization","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=output.sse$R.1 ,Type=rep("AB=BA",length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,1.5)

p3.16<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(-1,1.5)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))

multiplot(p1,p3.16,p5,p2,p4,p6, cols=2)

########

############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems  with asymmetric hidden states
output.sse<-read.table("~/Dropbox/solploidypersonal/muhisse250K/output/MuHiSSE_asym100K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","Diploidization","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))


hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,3)

p3.17<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(-2,2)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,3.5)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.17,p5,p2,p4,p6, cols=2)

########

############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems  with all asymmetric rates
output.sse<-read.table("~/Dropbox/solploidypersonal/muhisse250K/output/MuHiSSE_allasym100K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates1<-data.frame(dens=c(output.sse$rate_12A,output.sse$rate_21A,output.sse$rate_31A,output.sse$rate_32A) ,Type=rep(c("Polyploidization from SC A","Diploidization A","SI to SC A","Polyploidization from SI A"),each=length(output.sse$rate_12A)))


trait.rates2<-data.frame(dens=c(output.sse$rate_12B,output.sse$rate_21B,output.sse$rate_31B,output.sse$rate_32B) ,Type=rep(c("Polyploidization from SC B","Diploidization B","SI to SC B","Polyploidization from SI B"),each=length(output.sse$rate_12B)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate2)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p3.18<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(-1.5,2)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,3.5)


p5.1<-ggplot(trait.rates1, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5]))+xlim(0,0.6)

p5.2<-ggplot(trait.rates2, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5]))+xlim(0,0.6)

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))



multiplot(p1,p3.18,p5.1, p5.2,p2,p4,p6, cols=2)




############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems no diploidization ~/Dropbox/solploidypersonal/muhisse250k/output/MuHiSSE_ploidysi250K.log
output.sse<-read.table("~/Dropbox/solploidypersonal/muhissenodip250k/output/MuHiSSEnodip_ploidysi250K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=output.sse$R.1 ,Type=rep("AB=BA",length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p3.19<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))

multiplot(p1,p3.19,p5,p2,p4,p6, cols=2)

########

############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems no diploidization with asymmetric hidden states
output.sse<-read.table("~/Dropbox/solploidypersonal/muhissenodip250K/output/MuHiSSEnodip_asym100K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p3.20<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(-1.5,2)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,3.5)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.20,p5,p2,p4,p6, cols=2)

########

############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems no diploidization with all asymmetric rates
output.sse<-read.table("~/Dropbox/solploidypersonal/muhissenodip250K/output/MuHiSSEnodip_allasym100K.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3, output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC-D A","Polyploid A", "SI-Diploid A","SC-D B","Polyploid B", "SI-Diploid B"),each=length(output.sse$speciation.1)))

trait.rates1<-data.frame(dens=c(output.sse$rate_12A,output.sse$rate_31A,output.sse$rate_32A) ,Type=rep(c("Polyploidization from SC A","SI to SC A","Polyploidization from SI A"),each=length(output.sse$rate_12A)))

trait.rates2<-data.frame(dens=c(output.sse$rate_12B,output.sse$rate_31B,output.sse$rate_32B) ,Type=rep(c("Polyploidization from SC B","SI to SC B","Polyploidization from SI B"),each=length(output.sse$rate_12B)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate2)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,2)

p3.21<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(-1.5,2)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3]))+xlim(0,3.5)


p5.1<-ggplot(trait.rates1, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

p5.2<- ggplot(trait.rates2, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.21,p5.1, p5.2,p2,p4,p6, cols=2)

########

# All net diversification plots


######NO DIPLOIDIZATION
# With symmetrical hidden
multiplot(p3.2, p3.9, p3.15, p3.6, p3.11, p3.19, cols=2)

# With asymmetrical hidden
multiplot(p3.2, p3.9, p3.15, p3.7, p3.12, p3.20, cols=2)

# With all asymmetrical
multiplot(p3.2, p3.9, p3.15, p3.8, p3.13, p3.21, cols=2)

# Effect of asymmetry
multiplot(p3.6, p3.11, p3.19, p3.7, p3.12, p3.20,p3.8, p3.13, p3.21,cols=3)


##### WITH DIPLOIDIZATION
# With symmetrical hidden
multiplot(p3.1, p3.9, p3.14, p3.3, p3.11, p3.16, cols=2)

# With asymmetrical hidden
multiplot(p3.1, p3.9, p3.14, p3.4, p3.12, p3.17, cols=2)

# With all asymetrical
multiplot(p3.1, p3.9, p3.14, p3.5, p3.13, p3.18, cols=2)

# Effect of asymmetry
multiplot(p3.3, p3.11, p3.16, p3.4, p3.12, p3.17,p3.5, p3.13, p3.18,cols=3)


##### Character independent models



# Character independent DP 
output.sse<-read.table("~/Dropbox/solploidypersonal/cid/output/cidDP.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("Extinction A","Extinction B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("Speciation A", "Speciation B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c(" Net div A","Net div B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Rel div A","Rel div B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12, output.sse$rate_21) ,Type=rep(c("Polyploidy", "Diploidization"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p3.22<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =  c("yellow","gray"))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4], cols[2]))


p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.22,p5,p2,p4,p6, cols=2)


# Character independent DP  no diploidization
output.sse<-read.table("~/Dropbox/solploidypersonal/cid/output/cidDPnodip.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("Extinction A","Extinction B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("Speciation A", "Speciation B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c(" Net div A","Net div B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Rel div A","Rel div B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12) ,Type=rep("Polyploidy",length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p3.23<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =  c("yellow","gray"))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4]))


p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.23,p5,p2,p4,p6, cols=2)

#################
# Character independent IC
output.sse<-read.table("~/Dropbox/solploidypersonal/cid/output/cidIC.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("Extinction A","Extinction B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("Speciation A","Speciation B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Net div A","Net div B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Rel div A","Rel div B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=(output.sse$Q.2) ,Type=rep("SI to SC",length(output.sse$Q.2)))

hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p3.24<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =  c("yellow","gray"))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[5]))


p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))

multiplot(p1,p3.24,p5,p2,p4,p6, cols=2)


#####


### Character independent with diploidization for 3 state
output.sse<-read.table("~/Dropbox/solploidypersonal/cid/output/cid3.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.6),Type=rep(c("Extinction A", "Extinctiontion B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.6),Type=rep(c("Speciation A", "Speciation B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("Net div A", "Net div B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("Rel div A", "Rel div B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","Diploidization","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))


hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p3.25<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.25,p5,p2,p4,p6, cols=2)


###########
#### Character independent without diploidization for 3 state

output.sse<-read.table("~/Dropbox/solploidypersonal/cid/output/cid3nodip.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.6),Type=rep(c("Extinction A", "Extinctiontion B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.6),Type=rep(c("Speciation A", "Speciation B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1, output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("Net div A", "Net div B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("Rel div A", "Rel div B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_31,output.sse$rate_32) ,Type=rep(c("Polyploidization from SC","SI to SC","Polyploidization from SI"),each=length(output.sse$rate_12)))


hidden.rate<-data.frame(dens=c(output.sse$R.1, output.sse$R.2) ,Type=rep(c("alpha","beta"),each=length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p3.25<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("yellow","gray"))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink",cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c("mediumseagreen","deeppink"))

multiplot(p1,p3.25,p5,p2,p4,p6, cols=2)

############### Lumped models


output.sse<-read.table("~/Dropbox/solploidypersonal/lumped/output/lumpedDP_2.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("Diploid","Polyploid"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("Diploid","Polyploid"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("Diploid","Polyploid"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("Diploid","Polyploid"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$p0,output.sse$rate_31) ,Type=rep(c("Polyploidization from SC and SI","SI to SC"),each=length(output.sse$rate_31)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[2],cols3[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[2],cols3[3]))

p3.26<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[2],cols3[3]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[2],cols3[3]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink"))

multiplot(p1,p3.26,p5,p2,p4, cols=2)



########## Lumped muhisse no diploidization

output.sse<-read.table("~/Dropbox/solploidypersonal/lumped/output/lumpedhiddenDP_2.log", header=TRUE)


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.4,output.sse$extinction.5),Type=rep(c("Diploid A","Polyploid A", "Diploid B","Polyploid B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.4,output.sse$speciation.5),Type=rep(c("Diploid A","Polyploid A", "Diploid B","Polyploid B"),each=length(output.sse$speciation.1)))

#sse.speciation1<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("SC-D A","Polyploid A"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5),Type=rep(c("Diploid A","Polyploid A", "Diploid B","Polyploid B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5),Type=rep(c("Diploid A","Polyploid A", "Diploid B", "Polyploid B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$p0,output.sse$rate_31) ,Type=rep(c("Polyploidization from SC and SI","SI to SC"),each=length(output.sse$rate_31)))

hidden.rate<-data.frame(dens=(output.sse$R.1) ,Type=rep("AB=BA",length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))=xlim(0,3)

p3.27<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(-0.5,1.5)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))+xlim(0,3.5)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],"pink"))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =cols[10])

multiplot(p1,p3.27,p5,p2,p4,p6, cols=2)



###########################

output.sse<-read.table("~/Dropbox/solploidypersonal/lumped/output/lumpedIC_2.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("C","I"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("C","I"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("C","I"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("C","I"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q0,output.sse$rate_12) ,Type=rep(c("SI to SC","Polyploidy from SC"),each=length(output.sse$rate_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))

p3.28<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[4]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols2[5]))

multiplot(p1,p3.28,p5,p2,p4, cols=2)



##### Lumped Hisse IC no dip
output.sse<-read.table("~/Dropbox/solploidypersonal/lumped/output/lumpedhiddenIC_2.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3,output.sse$extinction.4,output.sse$extinction.6),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.6),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3, output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("SC A","SI A", "SC B", "SI B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q0,output.sse$rate_12) ,Type=rep(c("SI to SC","Polyploidy from SC"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=output.sse$R.1 ,Type=rep("AB=BA",length(output.sse$R.1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p3.29<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =  c(cols2[1],cols2[2],cols2[4],cols2[3]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols2[1],cols2[2],cols2[4],cols2[3]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols2[5]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = (cols[10]))+xlim(0,0.05)

multiplot(p1,p3.29,p5,p2,p4,p6, cols=2)


multiplot(p3.2,p3.6,p3.26,p3.27,p3.15,p3.20, cols=3)

multiplot(p3.9, p3.11,p3.28,p3.29,p3.15,p3.20, cols=3)


multiplot(p3.23, p3.24, p3.25, p3.6, p3.11, p3.20,cols=2)

multiplot(p3.22, p3.25, p3.4, p3.17, cols=2)
