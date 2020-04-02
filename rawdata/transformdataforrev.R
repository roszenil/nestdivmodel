library("ape")
library("phytools")
library("treeplyr")
passerine.tree<-read.tree(file="~/Dropbox/nestdivmodel/rawdata/passerine_tree_for_nest_musse_binary.tre")#3243 tips and 3242 internal nodes
plotTree(passerine.tree,type="fan", lwd=0.1, fsize=0.03)
passerine.data<-read.csv("~/Dropbox/nestdivmodel/rawdata/nest_states.csv",header=TRUE,stringsAsFactors=FALSE)
#removing brood
brood<-which(passerine.data[,2]=="brood parasite")#18
passerine.data2<-passerine.data[-brood,]

#Matching tree and data without brood
matched<-make.treedata(passerine.tree,passerine.data2) #3225

#Finding how many of each
dome<-which(matched$dat=="dome") #722
cup<-which(matched$dat=="cup") #1943
cavity<-which(matched$dat=="cavity")#458
dc<-which(matched$dat=="cup or dome") #29
cv<- which(matched$dat=="cavity or cup") #60
dv<-which(matched$dat=="cavity or dome") #13

#Creating the three state dataset for Rev (tsv file)
new.code<-rep(0,dim(matched$dat)[1])
new.code[dome]="0"
new.code[cup]="1"
new.code[cavity]="2"
new.code[dc]="(0 1)"
new.code[cv]="(1 2)"
new.code[dv]="(0 2)"

#Data with species name attached to it
new.code<-cbind(matched$phy$tip.label,new.code)
#Write the tree with 3225 taxa
write.nexus(matched$phy,file="~/Dropbox/nestdivmodel/revcode/basicdata/nesttree.nex")
#This is the tsv file for three states open in textfile and remove the quotes that are saved in R because they are strings.
write.table(new.code,file="~/Dropbox/nestdivmodel/revcode/basicdata/nestthreestate.tsv",sep="\t", row.names=FALSE)

#Creating the six state dataset for Rev (tsv file)
new.code<-rep(0,dim(matched$dat)[1])
new.code[dome]=0
new.code[cup]=1
new.code[cavity]=2
new.code[dc]=3
new.code[cv]=4
new.code[dv]=5

#Data with species name attached to it
new.code<-cbind(matched$phy$tip.label,new.code)
#This is the tsv file for three states open in textfile and remove the quotes that are saved in R because they are strings.
write.table(new.code,file="~/Dropbox/nestdivmodel/revcode/basicdata/nestsixstate.tsv",sep="\t", row.names=FALSE)

