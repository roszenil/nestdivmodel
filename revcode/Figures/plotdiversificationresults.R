library("ggplot2")
library("wesanderson")
library("viridis")
library("inlmisc")

### Colors
cols<-c(wes_palette("Zissou1"),wes_palette("Darjeeling1"),wes_palette("GrandBudapest2"))
cols2<-c(inlmisc::GetColors(9, scheme = "muted"),inlmisc::GetColors(9, scheme = "light"))


mypalette<-c(cols[3],cols[1],cols[5],cols[7],cols[9],cols[11],cols[12],"darkgoldenrod1","blue3","firebrick3")
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

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE_threestate.log", header=TRUE) #72790 MCMC steps after burn-in
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
### CID-2 for MuSSE (this is the incorrect model to compare)
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/cid_threestate.log", header=TRUE) # 14808 
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.4),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.4),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.4/output.sse$speciation.4),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


hidden.rates<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2),Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[4:5])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[4:5])

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[4:5])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[4:5])

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =viridis(6))

p6<-ggplot(hidden.rates, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[6:7])
multiplot(p1,p3.3,p5,p2,p4,p6, cols=2)
##########
##############################################################################

############################################################################################
### Plots for diversification rates MuHiSSE 3-state
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuHiSSE_threestate.log", header=TRUE)
output.sse<-output.sse[-seq(1,500,1),] #cutting burn-in- 12407 MCMC steps


sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4,output.sse$extinction.5,output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4,output.sse$speciation.5,output.sse$speciation.6),Type=rep(c("Dome A","Cup A","Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4,output.sse$speciation.5-output.sse$extinction.5,output.sse$speciation.6-output.sse$extinction.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.4/output.sse$speciation.4,output.sse$extinction.5/output.sse$speciation.5,output.sse$extinction.6/output.sse$speciation.6),Type=rep(c("Dome A","Cup A", "Cavity A", "Dome B","Cup B","Cavity B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1,output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate2)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(mypalette[1],mypalette[8],mypalette[2],mypalette[9],mypalette[3],mypalette[10]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(mypalette[1],mypalette[8],mypalette[2],mypalette[9],mypalette[3],mypalette[10]))

p3.4<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(mypalette[1],mypalette[8],mypalette[2],mypalette[9],mypalette[3],mypalette[10]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(mypalette[1],mypalette[8],mypalette[2],mypalette[9],mypalette[3],mypalette[10]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("mediumseagreen","deeppink"))
multiplot(p1,p3.4,p5,p2,p4,p6, cols=2)
##########
############################################################################################
### First run in Rosana's laptop of MuSSE with 6 states
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE_sixstate_ZFlaptop.log", header=TRUE)
output.sse<-output.sse[-seq(1,500,1),]
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

mypalette2<-as.character(cols2)#viridis(18)
trait.rates1<-data.frame(dens=c(output.sse$q_12,output.sse$q_13,output.sse$q_14,output.sse$q_16),Type=rep(c("q_12","q_13","q_14","q_16"),each=length(output.sse$q_12)))

trait.rates2<-data.frame(dens=c(output.sse$q_21,output.sse$q_31,output.sse$q_41,output.sse$q_61),Type=rep(c("q_21","q_31","q_41","q_61"),each=length(output.sse$q_21)))

trait.rates3<-data.frame(dens=c(output.sse$q_21,output.sse$q_23,output.sse$q_24,output.sse$q_25),Type=rep(c("q_21","q_23","q_24","q_25"),each=length(output.sse$q_21)))

trait.rates4<-data.frame(dens=c(output.sse$q_12,output.sse$q_32,output.sse$q_42,output.sse$q_52),Type=rep(c("q_12","q_32","q_42","q_52"),each=length(output.sse$q_12)))

trait.rates5<-
data.frame(dens=c(output.sse$q_31,output.sse$q_32,output.sse$q_35,output.sse$q_36),Type=rep(c("q_31","q_32","q_35","q_36"),each=length(output.sse$q_31)))

trait.rates6<-
data.frame(dens=c(output.sse$q_13,output.sse$q_23,output.sse$q_53,output.sse$q_63),Type=rep(c("q_13","q_23","q_53","q_63"),each=length(output.sse$q_13)))

trait.rates7<-
data.frame(dens=c(output.sse$q_41,output.sse$q_42),Type=rep(c("q_41","q_42"),each=length(output.sse$q_41)))

trait.rates8<-
data.frame(dens=c(output.sse$q_14,output.sse$q_24),Type=rep(c("q_14","q_24"),each=length(output.sse$q_14)))


trait.rates9<-
data.frame(dens=c(output.sse$q_52,output.sse$q_53),Type=rep(c("q_52","q_53"),each=length(output.sse$q_52)))

trait.rates10<-
data.frame(dens=c(output.sse$q_25,output.sse$q_35),Type=rep(c("q_25","q_35"),each=length(output.sse$q_25)))


trait.rates11<-
data.frame(dens=c(output.sse$q_61,output.sse$q_63),Type=rep(c("q_61","q_63"),each=length(output.sse$q_61)))

trait.rates12<-
data.frame(dens=c(output.sse$q_16,output.sse$q_36),Type=rep(c("q_16","q_36"),each=length(output.sse$q_16)))



p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

multiplot(p1,p3.1,p2,p4, cols=2)

p5<-ggplot(trait.rates1, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[1:4])+xlim(0,0.01)

p6<-ggplot(trait.rates2, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(5,9,13,17)])+xlim(0,0.02)

p7<-ggplot(trait.rates3, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[5:8])

p8<-ggplot(trait.rates4, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(1,10,14,15)])+xlim(0,0.05)

p9<-ggplot(trait.rates5, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[9:12])

p10<-ggplot(trait.rates6, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(2,6,11,12)])+xlim (0,0.025)

p11<-ggplot(trait.rates7, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[13:14])+xlim(0,0.3)

p12<-ggplot(trait.rates8, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(3,7)])+xlim(0,0.01)

p13<-ggplot(trait.rates9, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[15:16])

p14<-ggplot(trait.rates10, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(8,11)])

p15<-ggplot(trait.rates11, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[17:18])+xlim(0,0.6)

p16<-ggplot(trait.rates12, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(aes(linetype=Type),alpha=0.7)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette2[c(4,12)])

multiplot(p5,p7,p9,p6,p8,p10,cols=2)
multiplot(p11,p13,p15,p12,p14,p16, cols=2)



### First run of the 3 states with the minimum of extinction 0.8 of speciation

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE3min.log", header=TRUE)
output.sse<-output.sse[-c(1:500),]

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

delta<-data.frame(dens=output.sse$extra,Type=rep("delta",each=length(output.sse$extra)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])+xlim(0.8,0.81)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = "purple")
multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)


### Second (more complete) run of the 3 states with the minimum of extinction 0.8 of speciation

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE3min-2.log", header=TRUE)
output.sse<-output.sse[-c(1:500),]

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

delta<-data.frame(dens=output.sse$extra,Type=rep("delta",each=length(output.sse$extra)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])+xlim(0.8,0.81)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = "purple")
multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)


### First run of the 3 states with the minimum of extinction 0.8 of speciation and extra for each state

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE3minv2.log", header=TRUE)
output.sse<-output.sse[-c(1:500),]

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])+xlim(0.8,0.81)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(3))

multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)


### First run of the 3 states with the minimum of extinction 0.8 of speciation and extra for each state

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results/MuSSE3minv3.log", header=TRUE)
output.sse<-output.sse[-c(1:500),]

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))

delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =mypalette[1:3])

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = mypalette[1:3])+xlim(0.8,0.81)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(3))


multiplot(p1,p3.1,p5,p2,p4,cols=2)
