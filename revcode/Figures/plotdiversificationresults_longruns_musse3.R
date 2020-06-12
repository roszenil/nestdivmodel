library("ggplot2")
library("wesanderson")
library("viridis")
library("inlmisc")
library("RColorBrewer")
### Colors
cols<-brewer.pal(11,"Spectral")


musse3palette<-cols[c(5,10,2)]

setwd("~/Dropbox/nestdivmodel/revcode/Figures")
source("multiplot.R")

### Plots for diversification
## Musse 3 state no intervention on extinction prior

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/MuSSE_threestate.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

multiplot(p1,p3,p5,p2,p4,cols=2)


### Plots for diversification

## Musse 3 state with a minimum of 0.8 x speciation + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)


output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3min_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.1<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(0.8,0.82)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.1<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.02)
multiplot(p1,p3.1,p5,p2,p4.1,p6.1, cols=2)


## Musse 3 state with a minimum of 0.8 x speciation + delta where delta ~ Gamma(alpha=0.5, beta=0.5) that is mean 1 and variance (2)

setwd("~/Dropbox/nestdivmodel/revcode/Figures")
source("multiplot.R")
### Long run MuSSE no restrictions on extinction
output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/MuSSE3min_gammavar2.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.2<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(0.8,0.81)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.2<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.0003)
multiplot(p1,p3.2,p5,p2,p4,p6.2, cols=2)

## Musse 3 state with a minimum of 0.8 x speciation + delta where delta ~ Gamma(alpha=0.25, beta=0.25) that is mean 1 and variance (4)

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/MuSSE3min_gammavar4.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(0.8,0.81)

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.3<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.0003)

multiplot(p1,p3.3,p5,p2,p4,p6.3, cols=2)

##Plotting all the net diversifications in the same scale
g1<-p3+xlim(0.025,0.14)
g2<-p3.1+xlim(0.025,0.14)
g3<-p3.2+xlim(0.025,0.14)
g4<-p3.3+xlim(0.025,0.14)

multiplot(g1,g2,g3,g4, cols=1)


## Plotting all the extras on the different assumptions
multiplot(p6.1,p6.2,p6.3)

############### Now same extra (Exp (2)) but different expectations of relative extinction
## p3.1 is the net div of rel.extinction=0.8

## Musse 3 state with a minimum of 0.7 x speciation + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)


output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3min07_exp.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.4<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(0.7,0.72)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.4<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.005)
multiplot(p1,p3.4,p5,p2,p4.4,p6.4, cols=2)

## Musse 3 state with a minimum of 0.9 x speciation + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)


output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3min09_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.5<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.5<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(0.9,0.92)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.5<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.005)
multiplot(p1,p3.5,p5,p2,p4.5,p6.5, cols=2)

## Musse 3 state with a minimum of 10 x speciation + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3min10_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.6<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.6<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(1,1.02)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.6<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.005)
multiplot(p1,p3.6,p5,p2,p4.6,p6.6, cols=2)

## Musse 3 state with a minimum of 1.1 x speciation + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3min11_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.7<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.7<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)+xlim(1.1,1.12)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.7<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.005)
multiplot(p1,p3.7,p5,p2,p4.7,p6.7, cols=2)

## Musse 3 state with different minima + delta where delta~ Exp(2) that is mean 0.5 and variance (0.025)

output.sse<-read.table("~/Dropbox/nestdivmodel/revcode/results_longruns/musse3mindiff_exp05.log", header=TRUE)
output.sse<-output.sse[-seq(1,5000,1),] #burn-in removal 5000 steps

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2, output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2, output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Dome","Cup","Cavity"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$q_12,output.sse$q_21,output.sse$q_23,output.sse$q_32,output.sse$q_13,output.sse$q_31),Type=rep(c("q_12","q_21","q_23","q_32","q_13","q_31"),each=length(output.sse$q_12)))


delta<-data.frame(dens=c(output.sse$extra.1,output.sse$extra.2,output.sse$extra.3),Type=rep(c("delta_1","delta_2","delta_3"),each=length(output.sse$extra.1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =musse3palette)

p3.8<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)

p4.8<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.8)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = musse3palette)


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = viridis(6))

p6.8<-ggplot(delta, aes(x=dens, fill=Type))+labs(title="Difference parameter",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("blue","white","red"))+xlim(0,0.005)
multiplot(p1,p3.8,p5,p2,p4.8,p6.8, cols=2)

g1<- p3+xlim(-0.1,0.14)
g2<- p3.4+xlim(-0.1,0.14)
g3<- p3.1+xlim(-0.1,0.14)
g4<- p3.5+xlim(-0.1,0.14)
g5<- p3.6+xlim(-0.1,0.14)
g6<- p3.7+xlim(-0.1,0.14)
g7<- p3.8+xlim(-0.1,0.14)

multiplot(g1,g2,g3,g4,g5,g6,cols=1)
multiplot(p4.8, p4.4, p4.1,g7,g2,g3,cols=2)


multiplot(p4,p4.4,p4.1,p4.5,p4.6,p4.7,p4.8, g1,g2,g3,g4,g5,g6,g7,cols=2)