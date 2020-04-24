library("ggplot2")
library("wesanderson")
library("RevGadgets")
#library("treeio")
library("ggtree")

cols<-c(wes_palette("Zissou1"),wes_palette("Darjeeling1"),wes_palette("GrandBudapest2"))
cols2<-c(inlmisc::GetTolColors(9, scheme = "muted"),inlmisc::GetTolColors(9, scheme = "light"))


mypalette<-c(cols[3],cols[1],cols[5],cols[7],cols[9],cols[11],cols[12],"darkgoldenrod1","blue3","firebrick3")
#sampling<-seq(1,250000,100)
### Plots for diversification rates Bisse DP with diploidization
source('~/Dropbox/nestdivmodel/revcode/Figures/plot_ancestral_states_2.R')


p = plot_ancestral_states("anc_states_summaryMuSSEthreestate.tree",summary_statistic="MAP", include_start_states=FALSE, tip_label_size=1, tip_label_offset=5,node_label_size=0.1,node_label_nudge_x=1,shoulder_label_size=2,alpha=.3,node_size_range=c(2, 5),state_colors=c("red","blue", "yellow"))
print(p)
cf <- coord_fixed()
cf$default <- TRUE


p2 = plot_ancestral_states_2("~/Dropbox/nestdivmodel/revcode/results/anc_states_summaryMuSSEthreestate.tree", 
                          size=0.4, include_start_states=FALSE, 
                          summary_statistic="MAP",tip_label_size=0.45, 
                          tip_label_offset=0.4,
                          #tip_label_offset=0.4,
                          tip_label_italics=TRUE,
                          node_label_size=FALSE,node_label_nudge_x=0.1,
                          shoulder_label_size=2,alpha=.3,tree_layout="circular")+cf+ coord_fixed(xlim = c(0, 40))

pdf("mussecircle.pdf")
p2
dev.off()


p2 = plot_ancestral_states("~/Dropbox/nestdivmodel/revcode/results/anc_states_summaryMuSSEthreestate.tree", 
                          size=0.4, include_start_states=FALSE, 
                          summary_statistic="MAP",tree_layout="circular")