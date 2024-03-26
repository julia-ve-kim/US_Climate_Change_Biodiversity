library(tidyverse)
library(clusterSEs)
library(lfe)
library(MASS)
library(plyr)
library(reshape2)
library(stargazer)
library(lubridate)
library(readr) 

#Commented code below creates the dataset "speciesdata.csv" from intermediate data files

# datfull=read.csv("Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Data/NSESA Full Dataset with NGrams April 2021.csv")
# 
# #identify species where NatureServe conservation status was determined after delisting
# datfull$Date.Final.Rule.to.Delist<-year(mdy(datfull$Date.Final.Rule.to.Delist))
# probs=unique(datfull$EGT_Species.Code[which(datfull$G_RANK_C_D>datfull$Date.Final.Rule.to.Delist)])
# #identify species that were listed more than 10 years after NatureServe assessment
# probs=c(probs,unique(datfull$EGT_Species.Code[which(datfull$G_RANK_C_D<(datfull$USFWS_Earliest.Date.Listed-10))]))
# save(probs,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/problemspecies.Rdat")
# 
# #fill in some missing family data
# fams=read.csv("All Listings with Family Info.csv")
# fams=fams%>%dplyr::select(Species.Code,Family)
# fams=unique(fams)
# 
# missingfams=unique(datfull$Species.Code[which(is.na(datfull$FAMILY))])
# for(i in 1:length(missingfams)){
#   fam=fams$Family[which(fams$Species.Code==as.character(missingfams[i]))]
#   #test whether we need to add a new level
#   if(!fam%in%levels(datfull$FAMILY)) levels(datfull$FAMILY)=c(levels(datfull$FAMILY),as.character(fam))
# 
#   datfull$FAMILY[which(datfull$Species.Code==missingfams[i])]=fam
# }
# 
# #read in biology data
# bio=read.csv("From Xiaoli/out_EcoEvoVar.csv")
# 
# #additional data for revision
# bio_adds=read.csv("From Xiaoli/missingNew_eev.csv")
# bio_adds=data.frame(X=NA,EGT_ID=bio_adds$code,sp=bio_adds$species,tax=NA,genus=NA,species=NA,ED=NA,EDGE=bio_adds$EDGE,maxSp=bio_adds$maxSp)
# bio=rbind(bio,bio_adds)
# 
# 
# #zeroes in number of species in genus data are actually NAs
# bio$maxSp[which(bio$maxSp==0)]=NA
# #calculate global endangered score (GE) from EDGE and ED formulas
# bio$GE=(bio$EDGE-log(bio$ED+1))/log(2)
# 
# datfull=merge(datfull,bio[,c(2,7:10)],all.x=T,all.y=F)
# 
# #merge in manually added ngenus for some listed species
# gen_add=read.csv("fill_missinggenusdata.csv")
# for(i in 1:dim(gen_add)[1]) datfull$maxSp[which(as.character(datfull$GENUS)==as.character(gen_add$x[i]))]=gen_add$ngenus[i]
# 
# #coalesce datfull$Group.Lumped and class_lumped
# datfull$Group.Lumped<-as.character(datfull$Group.Lumped)
# datfull$class_lumped<-as.character(datfull$class_lumped)
# datfull$Tax.Group <- ifelse(is.na(datfull$Group.Lumped), datfull$class_lumped, datfull$Group.Lumped)
# 
# dat=data.frame(code=datfull$EGT_Species.Code,name=datfull$GNAME,taxon=datfull$Tax.Group,family=datfull$FAMILY,order=datfull$TAXORDER,genus=datfull$GENUS,status_species=datfull$N_CON_STAT,status_global=datfull$G_CON_STAT,year=datfull$Year,spending=datfull$Exp_Total.Expenditures,region=datfull$Lead.Region,region_num=datfull$Number.of.Occurrence.Regions,priority=datfull$RR_Num.Min.Priority.Number,priority_threat=datfull$Degree.of.Threat,priority_potential=datfull$Recovery.Potential,priority_rarity=datfull$Rarity,conflict=datfull$RR_Conflict.Designation,status=datfull$Exp_Listing.Classification,datelisted=datfull$USFWS_Earliest.Date.Listed,ngenus=datfull$maxSp,evdist=datfull$ED,ge=datfull$GE,edge=datfull$EDGE,rangeareas=datfull$USFWS_Range.Area.KMSQ)
# 
# #new variable - from 1950 to 10 years before listing for listed species, mean 1950 - present for non-listed species
# ngrams<-datfull
# ngrams$Scientific=ngrams$Scientific_1950_Listed;ngrams$Scientific[which(is.na(ngrams$Scientific_1950_Listed))]=ngrams$Scientific_1950[which(is.na(ngrams$Scientific_1950_Listed))]
# ngrams$Common=ngrams$Common_1950_Listed;ngrams$Common[which(is.na(ngrams$Common_1950_Listed))]=ngrams$Common_1950[which(is.na(ngrams$Common_1950_Listed))]
# ngrams=data.frame(code=ngrams$EGT_Species.Code,year=ngrams$Year,ngram_science=ngrams$Scientific,ngram_common=ngrams$Common)
# 
# #Adding manually flagged
# manual<-read.csv("Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Data/Manual NGram Flagged Common Name.csv")
# ngrams<-left_join(ngrams, manual)
# ngrams<-distinct(ngrams)
# ngrams$CommonNameFlag[which(is.na(ngrams$CommonNameFlag))]=0
# 
# ngrams$ngram_common[which(ngrams$ngram_common>0.8)]=NA
# #ngram NAs are actually true zeroes - not high enough frequency to return an ngram
# ngrams$ngram_common[which(is.na(ngrams$ngram_common))]=0;ngrams$ngram_science[which(is.na(ngrams$ngram_science))]=0
# ngrams$CommonNameFlag[which(is.na(ngrams$CommonNameFlag))]=0
# 
# dat=merge(dat,ngrams,all.x=TRUE,all.y=FALSE)
# 
# #some ngram NAs for species that return no results because of low frequency can be recoded as 0
# dat$ngram_common[which(is.na(dat$ngram_common))]=0
# dat$ngram_science[which(is.na(dat$ngram_science))]=0
# library(tidyverse)
library(clusterSEs)
library(lfe)
library(MASS)
library(plyr)
library(reshape2)
library(stargazer)
library(lubridate)
library(readr) 

#Commented code below creates the dataset "speciesdata.csv" from intermediate data files

# datfull=read.csv("Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Data/NSESA Full Dataset with NGrams April 2021.csv")
# 
# #identify species where NatureServe conservation status was determined after delisting
# datfull$Date.Final.Rule.to.Delist<-year(mdy(datfull$Date.Final.Rule.to.Delist))
# probs=unique(datfull$EGT_Species.Code[which(datfull$G_RANK_C_D>datfull$Date.Final.Rule.to.Delist)])
# #identify species that were listed more than 10 years after NatureServe assessment
# probs=c(probs,unique(datfull$EGT_Species.Code[which(datfull$G_RANK_C_D<(datfull$USFWS_Earliest.Date.Listed-10))]))
# save(probs,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/problemspecies.Rdat")
# 
# #fill in some missing family data
# fams=read.csv("All Listings with Family Info.csv")
# fams=fams%>%dplyr::select(Species.Code,Family)
# fams=unique(fams)
# 
# missingfams=unique(datfull$Species.Code[which(is.na(datfull$FAMILY))])
# for(i in 1:length(missingfams)){
#   fam=fams$Family[which(fams$Species.Code==as.character(missingfams[i]))]
#   #test whether we need to add a new level
#   if(!fam%in%levels(datfull$FAMILY)) levels(datfull$FAMILY)=c(levels(datfull$FAMILY),as.character(fam))
# 
#   datfull$FAMILY[which(datfull$Species.Code==missingfams[i])]=fam
# }
# 
# #read in biology data
# bio=read.csv("From Xiaoli/out_EcoEvoVar.csv")
# 
# #additional data for revision
# bio_adds=read.csv("From Xiaoli/missingNew_eev.csv")
# bio_adds=data.frame(X=NA,EGT_ID=bio_adds$code,sp=bio_adds$species,tax=NA,genus=NA,species=NA,ED=NA,EDGE=bio_adds$EDGE,maxSp=bio_adds$maxSp)
# bio=rbind(bio,bio_adds)
# 
# 
# #zeroes in number of species in genus data are actually NAs
# bio$maxSp[which(bio$maxSp==0)]=NA
# #calculate global endangered score (GE) from EDGE and ED formulas
# bio$GE=(bio$EDGE-log(bio$ED+1))/log(2)
# 
# datfull=merge(datfull,bio[,c(2,7:10)],all.x=T,all.y=F)
# 
# #merge in manually added ngenus for some listed species
# gen_add=read.csv("fill_missinggenusdata.csv")
# for(i in 1:dim(gen_add)[1]) datfull$maxSp[which(as.character(datfull$GENUS)==as.character(gen_add$x[i]))]=gen_add$ngenus[i]
# 
# #coalesce datfull$Group.Lumped and class_lumped
# datfull$Group.Lumped<-as.character(datfull$Group.Lumped)
# datfull$class_lumped<-as.character(datfull$class_lumped)
# datfull$Tax.Group <- ifelse(is.na(datfull$Group.Lumped), datfull$class_lumped, datfull$Group.Lumped)
# 
# dat=data.frame(code=datfull$EGT_Species.Code,name=datfull$GNAME,taxon=datfull$Tax.Group,family=datfull$FAMILY,order=datfull$TAXORDER,genus=datfull$GENUS,status_species=datfull$N_CON_STAT,status_global=datfull$G_CON_STAT,year=datfull$Year,spending=datfull$Exp_Total.Expenditures,region=datfull$Lead.Region,region_num=datfull$Number.of.Occurrence.Regions,priority=datfull$RR_Num.Min.Priority.Number,priority_threat=datfull$Degree.of.Threat,priority_potential=datfull$Recovery.Potential,priority_rarity=datfull$Rarity,conflict=datfull$RR_Conflict.Designation,status=datfull$Exp_Listing.Classification,datelisted=datfull$USFWS_Earliest.Date.Listed,ngenus=datfull$maxSp,evdist=datfull$ED,ge=datfull$GE,edge=datfull$EDGE,rangeareas=datfull$USFWS_Range.Area.KMSQ)
# 
# #new variable - from 1950 to 10 years before listing for listed species, mean 1950 - present for non-listed species
# ngrams<-datfull
# ngrams$Scientific=ngrams$Scientific_1950_Listed;ngrams$Scientific[which(is.na(ngrams$Scientific_1950_Listed))]=ngrams$Scientific_1950[which(is.na(ngrams$Scientific_1950_Listed))]
# ngrams$Common=ngrams$Common_1950_Listed;ngrams$Common[which(is.na(ngrams$Common_1950_Listed))]=ngrams$Common_1950[which(is.na(ngrams$Common_1950_Listed))]
# ngrams=data.frame(code=ngrams$EGT_Species.Code,year=ngrams$Year,ngram_science=ngrams$Scientific,ngram_common=ngrams$Common)
# 
# #Adding manually flagged
# manual<-read.csv("Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Data/Manual NGram Flagged Common Name.csv")
# ngrams<-left_join(ngrams, manual)
# ngrams<-distinct(ngrams)
# ngrams$CommonNameFlag[which(is.na(ngrams$CommonNameFlag))]=0
# 
# ngrams$ngram_common[which(ngrams$ngram_common>0.8)]=NA
# #ngram NAs are actually true zeroes - not high enough frequency to return an ngram
# ngrams$ngram_common[which(is.na(ngrams$ngram_common))]=0;ngrams$ngram_science[which(is.na(ngrams$ngram_science))]=0
# ngrams$CommonNameFlag[which(is.na(ngrams$CommonNameFlag))]=0
# 
# dat=merge(dat,ngrams,all.x=TRUE,all.y=FALSE)
# 
# #some ngram NAs for species that return no results because of low frequency can be recoded as 0
# dat$ngram_common[which(is.na(dat$ngram_common))]=0
# dat$ngram_science[which(is.na(dat$ngram_science))]=0
# 
# #identify some problem common name ngrams based on ratio with science ngrams
# dat$CommonNameFlag[which(dat$ngram_common/dat$ngram_science>1000)]=1
# 
# # #normalize ngrams
# dat$ngram_common=(dat$ngram_common-mean(dat$ngram_common,na.rm=T))/sd(dat$ngram_common,na.rm=T)
# dat$ngram_science=(dat$ngram_science-mean(dat$ngram_science,na.rm=T))/sd(dat$ngram_science,na.rm=T)

# speciesdat=dat%>%
#   group_by(code)%>%
#   dplyr::summarize(taxon=taxon[1],family=family[1],order=order[1],name=name[1],listed=status[1],status=status_species[1],status_global=status_global[1],ngram_science=mean(ngram_science,na.rm=T),ngram_common_flag=CommonNameFlag[1],ngram_common=mean(ngram_common,na.rm=T),ngenus=ngenus[1],evdist=evdist[1],ge=ge[1],edge=edge[1])
# speciesdat$listed=ifelse(speciesdat$listed%in%levels(dat$status)[c(4:9,13)],1,0)

# speciesdat$probs=0;speciesdat$probs[probs]=1 #identify problem species from lines 15-19

# 
# save(speciesdat,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/speciesdata.Rdat")

#1. ----------first model - model binary listing decision---------------------

speciesdat= read.csv(here::here("data/raw_data/speciesdata.csv"))

#make figure 1b
pattern.type=c("blank","hdashes","Rsymbol_20","crosshatch","bricks","vdashes")
pattern.color=rep("black",6)
background.color=c("#E7298A","#D95F02","#E6AB02", "#1B9E77","#66A61E", "#7570B3")
toplot=speciesdat%>%
  filter(status%in%c(1,2,3,4,5,"UNK"))%>%
  add_count(taxon)%>%
  group_by(taxon,status)%>%
  dplyr::summarize(frac=n(),tot=n[1])%>%
  mutate(perc=frac/tot)

a=ggplot(toplot,aes(x=taxon,y=frac,fill=status))+geom_bar(stat="identity",position="fill",col="black")+theme_bw()
a=a+theme(text=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, face="bold"))+labs(x="Taxon",y="Proportion",fill="Status")
tots=toplot%>%dplyr::select(taxon,tot)%>%distinct()%>%drop_na(tot)
a=a+geom_text(tots,mapping=aes(x=taxon,y=1.05,label=tot),position="identity",inherit.aes = FALSE,size=7)
a=a+scale_fill_grey(start = 1, end = 0,labels=c("Critically Imperiled","Imperiled","Vulnerable","Apparently Secure","Secure","Unknown"))
a

# #remove long tail of common name ngrams
speciesdat$ngram_common[which(speciesdat$ngram_common>10)]=NA
speciesdat$ngram_science[which(speciesdat$ngram_science>10)]=NA

#remove ngrams with suspicious ratio
speciesdat$ngram_common[which(abs(speciesdat$ngram_common/speciesdat$ngram_science)>10)]=NA

if (!is.factor(speciesdat$taxon)) {
  speciesdat$taxon <- as.factor(speciesdat$taxon)
}

#use mammals as the dropped category
speciesdat$taxon=relevel(speciesdat$taxon,ref="Mammals")

#for species where Natureserve ranking was determined after delisting or before listing set listing to 0
speciesdat$listed[which(speciesdat$probs==1)]=0


#first do two step model - first stage listing decision, second stage spending decision

fit1=glm(listed~taxon+status+ngram_common+ngram_science+I(log(ngenus)),data=speciesdat[-which(speciesdat$ngram_common_flag==1),],family=binomial(link="logit"),x=TRUE)
fit1_ses_family=cluster.bs.glm(fit1,speciesdat[-which(speciesdat$ngram_common_flag==1),],~family,boot.reps=250,output.replicates = TRUE)

fit1_withevdist=glm(listed~taxon+status+ngram_common+ngram_science+I(log(ngenus))+I(log(evdist)),data=speciesdat[which(speciesdat$taxon%in%c("Reptiles","Mammals","Amphibians","Birds")&speciesdat$ngram_common_flag==0),],family=binomial(link="logit"))
fit1_withevdist_ses_family=cluster.bs.glm(fit1_withevdist,speciesdat[which(speciesdat$taxon%in%c("Reptiles","Mammals","Amphibians","Birds")&speciesdat$ngram_common_flag==0),],~family,boot.reps=250,output.replicates = TRUE)

save(fit1,fit1_ses_family,fit1_withevdist,fit1_withevdist_ses_family,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Second Revision/models.Rdat")

#check whether effects of conservation status are different from each other, not just from conservation status 1
statuslevels=c("status2","status3","status4","status5","statusExtinct","statusProb. Extinct","statusUNK")
comp=array(dim=list(length(statuslevels),length(statuslevels),2))
nboots=dim(fit1_ses_family$replicates)[1]
for(i in 1:length(statuslevels)){
  col1=which(colnames(fit1_ses_family$replicates)==statuslevels[i])
  for(j in 1:length(statuslevels)){
    if(i==j) next
    col2=which(colnames(fit1_ses_family$replicates)==statuslevels[j])
    comp[i,j,1]=mean(fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])
    comp[i,j,2]=ifelse(comp[i,j,1]>0,sum((fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])<0)/nboots*100,sum((fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])>0)/nboots*100)
  }
}

#convert values to probability of listing, with uncertainties for each taxon * conservation group
taxconsmeans=speciesdat[-which(is.na(speciesdat$status)),]%>%
  filter(ngram_common_flag==0)%>%
  group_by(taxon,status)%>%
  dplyr::summarize(ngram_common=quantile(ngram_common,probs=0.5,na.rm=T),ngram_science=quantile(ngram_science,probs=0.5,na.rm=T),ngenus=log(quantile(ngenus,probs=0.5,na.rm=T)))

#use coefficients from fit1 to find conditional probabilites of listing for taxon-status groups with confidence intervals
taxon=levels(speciesdat$taxon)
status=levels(speciesdat$status)

for(i in 1:length(taxon)){
  for(j in 1:length(status)){
    intercept=fit1$coefficients[1]+ifelse(i==1,0,fit1$coefficients[grep(taxon[i],names(fit1$coefficients))])+ifelse(j==1,0,ifelse(j==6,fit1$coefficients[grep("Extinct",names(fit1$coefficients))],fit1$coefficients[grep(status[j],names(fit1$coefficients))]))
    taxmeanstemp=taxconsmeans[which(taxconsmeans$taxon==taxon[i]),]
    taxmeanstemp=taxmeanstemp[ifelse(j==6,grep("Extinct",taxmeanstemp$status),grep(status[j],taxmeanstemp$status)),]
    fit=intercept+as.numeric(taxmeanstemp[3:5])%*%fit1$coefficients[17:19]
    fitprob=exp(fit)/(1+exp(fit))
    if(i==1&j==1) taxconfit=fitprob
    if(i>1|j>1) taxconfit=append(taxconfit,fitprob)
  }
}
taxconfit=data.frame(taxon=rep(taxon,each=length(status)),status=rep(status,length(taxon)),fitprob=taxconfit)

bounds=matrix(nrow=dim(taxconfit)[1],ncol=dim(fit1_ses_family$replicates)[1])
#get confidence intervals for predicted probabilities using bootstrapped estimates
for(k in 1:dim(fit1_ses_family$replicates)[1]){
  for(i in 1:length(taxon)){
    for(j in 1:length(status)){
      intercept=fit1_ses_family$replicates[k,1]+ifelse(i==1,0,fit1_ses_family$replicates[k,grep(taxon[i],colnames(fit1_ses_family$replicates))])+ifelse(j==1,0,ifelse(j==6,fit1_ses_family$replicates[k,grep("Extinct",colnames(fit1_ses_family$replicates))],fit1_ses_family$replicates[k,grep(status[j],colnames(fit1_ses_family$replicates))]))
      taxmeanstemp=taxconsmeans[which(taxconsmeans$taxon==taxon[i]),]
      taxmeanstemp=taxmeanstemp[ifelse(j==6,grep("Extinct",taxmeanstemp$status),grep(status[j],taxmeanstemp$status)),]
      fit=intercept+as.numeric(taxmeanstemp[3:5])%*%fit1$coefficients[17:19]
      fitprob=exp(fit)/(1+exp(fit))
      entry=which(taxconfit$taxon==taxon[i]&taxconfit$status==status[j])
      bounds[entry,k]=fitprob
    }
  }
}

taxconfit=cbind(taxconfit,t(apply(bounds,MARGIN=1,function(x) quantile(x,probs=c(0.025,0.975),na.rm=T))))
#turn into percent
taxconfit[,3:5]=taxconfit[,3:5]*100

colnames(taxconfit)=c("taxon","status","fitprob","pmin","pmax")

a=ggplot(taxconfit[-which(taxconfit$status%in%c("Extinct","Prob. Extinct")|taxconfit$status=="2 "),],aes(x=status,y=fitprob,ymin=pmin,ymax=pmax,group=taxon,col=taxon,pch=taxon))+geom_point(size=4)+geom_errorbar(width=0.2)
a=a+labs(x="Assessed Conservation Status",y="Predicted Probability of Listing",color=NULL,pch=NULL)+theme_bw(base_size=18)
a=a+scale_shape_manual(values=c(0,1,2,7,8,15,16,4,17))
x11()
a
# #identify some problem common name ngrams based on ratio with science ngrams
# dat$CommonNameFlag[which(dat$ngram_common/dat$ngram_science>1000)]=1
# 
# # #normalize ngrams
# dat$ngram_common=(dat$ngram_common-mean(dat$ngram_common,na.rm=T))/sd(dat$ngram_common,na.rm=T)
# dat$ngram_science=(dat$ngram_science-mean(dat$ngram_science,na.rm=T))/sd(dat$ngram_science,na.rm=T)

# speciesdat=dat%>%
#   group_by(code)%>%
#   dplyr::summarize(taxon=taxon[1],family=family[1],order=order[1],name=name[1],listed=status[1],status=status_species[1],status_global=status_global[1],ngram_science=mean(ngram_science,na.rm=T),ngram_common_flag=CommonNameFlag[1],ngram_common=mean(ngram_common,na.rm=T),ngenus=ngenus[1],evdist=evdist[1],ge=ge[1],edge=edge[1])
# speciesdat$listed=ifelse(speciesdat$listed%in%levels(dat$status)[c(4:9,13)],1,0)

# speciesdat$probs=0;speciesdat$probs[probs]=1 #identify problem species from lines 15-19

# 
# save(speciesdat,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/speciesdata.Rdat")

#1. ----------first model - model binary listing decision---------------------

speciesdat= read.csv(here::here("data/raw_data/speciesdata.csv"))

#make figure 1b
pattern.type=c("blank","hdashes","Rsymbol_20","crosshatch","bricks","vdashes")
pattern.color=rep("black",6)
background.color=c("#E7298A","#D95F02","#E6AB02", "#1B9E77","#66A61E", "#7570B3")
toplot=speciesdat%>%
  filter(status%in%c(1,2,3,4,5,"UNK"))%>%
  add_count(taxon)%>%
  group_by(taxon,status)%>%
  dplyr::summarize(frac=n(),tot=n[1])%>%
  mutate(perc=frac/tot)

a=ggplot(toplot,aes(x=taxon,y=frac,fill=status))+geom_bar(stat="identity",position="fill",col="black")+theme_bw()
a=a+theme(text=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, face="bold"))+labs(x="Taxon",y="Proportion",fill="Status")
tots=toplot%>%dplyr::select(taxon,tot)%>%distinct()%>%drop_na(tot)
a=a+geom_text(tots,mapping=aes(x=taxon,y=1.05,label=tot),position="identity",inherit.aes = FALSE,size=7)
a=a+scale_fill_grey(start = 1, end = 0,labels=c("Critically Imperiled","Imperiled","Vulnerable","Apparently Secure","Secure","Unknown"))
a

# #remove long tail of common name ngrams
speciesdat$ngram_common[which(speciesdat$ngram_common>10)]=NA
speciesdat$ngram_science[which(speciesdat$ngram_science>10)]=NA

#remove ngrams with suspicious ratio
speciesdat$ngram_common[which(abs(speciesdat$ngram_common/speciesdat$ngram_science)>10)]=NA

if (!is.factor(speciesdat$taxon)) {
  speciesdat$taxon <- as.factor(speciesdat$taxon)
}

#use mammals as the dropped category
speciesdat$taxon=relevel(speciesdat$taxon,ref="Mammals")

#for species where Natureserve ranking was determined after delisting or before listing set listing to 0
speciesdat$listed[which(speciesdat$probs==1)]=0


#first do two step model - first stage listing decision, second stage spending decision

fit1=glm(listed~taxon+status+ngram_common+ngram_science+I(log(ngenus)),data=speciesdat[-which(speciesdat$ngram_common_flag==1),],family=binomial(link="logit"),x=TRUE)
fit1_ses_family=cluster.bs.glm(fit1,speciesdat[-which(speciesdat$ngram_common_flag==1),],~family,boot.reps=250,output.replicates = TRUE)

fit1_withevdist=glm(listed~taxon+status+ngram_common+ngram_science+I(log(ngenus))+I(log(evdist)),data=speciesdat[which(speciesdat$taxon%in%c("Reptiles","Mammals","Amphibians","Birds")&speciesdat$ngram_common_flag==0),],family=binomial(link="logit"))
fit1_withevdist_ses_family=cluster.bs.glm(fit1_withevdist,speciesdat[which(speciesdat$taxon%in%c("Reptiles","Mammals","Amphibians","Birds")&speciesdat$ngram_common_flag==0),],~family,boot.reps=250,output.replicates = TRUE)

save(fit1,fit1_ses_family,fit1_withevdist,fit1_withevdist_ses_family,file="Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Second Revision/models.Rdat")

#check whether effects of conservation status are different from each other, not just from conservation status 1
statuslevels=c("status2","status3","status4","status5","statusExtinct","statusProb. Extinct","statusUNK")
comp=array(dim=list(length(statuslevels),length(statuslevels),2))
nboots=dim(fit1_ses_family$replicates)[1]
for(i in 1:length(statuslevels)){
  col1=which(colnames(fit1_ses_family$replicates)==statuslevels[i])
  for(j in 1:length(statuslevels)){
    if(i==j) next
    col2=which(colnames(fit1_ses_family$replicates)==statuslevels[j])
    comp[i,j,1]=mean(fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])
    comp[i,j,2]=ifelse(comp[i,j,1]>0,sum((fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])<0)/nboots*100,sum((fit1_ses_family$replicates[,col1]-fit1_ses_family$replicates[,col2])>0)/nboots*100)
  }
}

#convert values to probability of listing, with uncertainties for each taxon * conservation group
taxconsmeans=speciesdat[-which(is.na(speciesdat$status)),]%>%
  filter(ngram_common_flag==0)%>%
  group_by(taxon,status)%>%
  dplyr::summarize(ngram_common=quantile(ngram_common,probs=0.5,na.rm=T),ngram_science=quantile(ngram_science,probs=0.5,na.rm=T),ngenus=log(quantile(ngenus,probs=0.5,na.rm=T)))

#use coefficients from fit1 to find conditional probabilites of listing for taxon-status groups with confidence intervals
taxon=levels(speciesdat$taxon)
status=levels(speciesdat$status)

for(i in 1:length(taxon)){
  for(j in 1:length(status)){
    intercept=fit1$coefficients[1]+ifelse(i==1,0,fit1$coefficients[grep(taxon[i],names(fit1$coefficients))])+ifelse(j==1,0,ifelse(j==6,fit1$coefficients[grep("Extinct",names(fit1$coefficients))],fit1$coefficients[grep(status[j],names(fit1$coefficients))]))
    taxmeanstemp=taxconsmeans[which(taxconsmeans$taxon==taxon[i]),]
    taxmeanstemp=taxmeanstemp[ifelse(j==6,grep("Extinct",taxmeanstemp$status),grep(status[j],taxmeanstemp$status)),]
    fit=intercept+as.numeric(taxmeanstemp[3:5])%*%fit1$coefficients[17:19]
    fitprob=exp(fit)/(1+exp(fit))
    if(i==1&j==1) taxconfit=fitprob
    if(i>1|j>1) taxconfit=append(taxconfit,fitprob)
  }
}
taxconfit=data.frame(taxon=rep(taxon,each=length(status)),status=rep(status,length(taxon)),fitprob=taxconfit)

bounds=matrix(nrow=dim(taxconfit)[1],ncol=dim(fit1_ses_family$replicates)[1])
#get confidence intervals for predicted probabilities using bootstrapped estimates
for(k in 1:dim(fit1_ses_family$replicates)[1]){
  for(i in 1:length(taxon)){
    for(j in 1:length(status)){
      intercept=fit1_ses_family$replicates[k,1]+ifelse(i==1,0,fit1_ses_family$replicates[k,grep(taxon[i],colnames(fit1_ses_family$replicates))])+ifelse(j==1,0,ifelse(j==6,fit1_ses_family$replicates[k,grep("Extinct",colnames(fit1_ses_family$replicates))],fit1_ses_family$replicates[k,grep(status[j],colnames(fit1_ses_family$replicates))]))
      taxmeanstemp=taxconsmeans[which(taxconsmeans$taxon==taxon[i]),]
      taxmeanstemp=taxmeanstemp[ifelse(j==6,grep("Extinct",taxmeanstemp$status),grep(status[j],taxmeanstemp$status)),]
      fit=intercept+as.numeric(taxmeanstemp[3:5])%*%fit1$coefficients[17:19]
      fitprob=exp(fit)/(1+exp(fit))
      entry=which(taxconfit$taxon==taxon[i]&taxconfit$status==status[j])
      bounds[entry,k]=fitprob
    }
  }
}

taxconfit=cbind(taxconfit,t(apply(bounds,MARGIN=1,function(x) quantile(x,probs=c(0.025,0.975),na.rm=T))))
#turn into percent
taxconfit[,3:5]=taxconfit[,3:5]*100

colnames(taxconfit)=c("taxon","status","fitprob","pmin","pmax")

a=ggplot(taxconfit[-which(taxconfit$status%in%c("Extinct","Prob. Extinct")|taxconfit$status=="2 "),],aes(x=status,y=fitprob,ymin=pmin,ymax=pmax,group=taxon,col=taxon,pch=taxon))+geom_point(size=4)+geom_errorbar(width=0.2)
a=a+labs(x="Assessed Conservation Status",y="Predicted Probability of Listing",color=NULL,pch=NULL)+theme_bw(base_size=18)
a=a+scale_shape_manual(values=c(0,1,2,7,8,15,16,4,17))
x11()
a

stargazer(fit1,fit1_withevdist,se=list(apply(fit1_ses_family$replicates,MARGIN=2,FUN=sd),apply(fit1_withevdist_ses_family$replicates,MARGIN=2,FUN=sd)),type="html",covariate.labels = c("Amphibians","Birds","Fish","Fungi","Invertebrates","Plants","Protists","Reptiles","Imperilled","Vulnerable","Apparently Secure","Secure","Extinct","Probably Extinct","Unknown","Common Ngram","Scientific Ngram","Genus Size (logged)","Evol. Dist. (logged)",NA),dep.var.labels = "Listed",column.labels = c("All Taxa, No Ev. Dist","Few Taxa, inc Ev. Dist"),out="Writing Up and Presentations\\JAERE Submission\\JAERE Resubmission\\Second Revision\\table2.html",model.numbers = FALSE,single.row = TRUE)

#2. ----------------- second stage spending model, conditional on listing ----------------------

#commented code gives steps for creating the spending data frame (spendingdata.csv)

# dat_listed=dat[-which(is.na(dat$status)),];dat_listed$status=factor(dat_listed$status)
# levels(dat_listed$status)=c(NA,NA,NA,"E","E","E","E","E","E",NA,NA,NA,"T")
# dat_listed=dat_listed[,-which(colnames(dat_listed)%in%c("ngram_science","ngram_common"))]
# dat_listed=dat_listed[-which(is.na(dat_listed$status)),]
# 
# #get ngram data changing over time
# ngrams_listed=read.csv("Writing Up and Presentations/JAERE Submission/JAERE Resubmission/Data/Rolling Means Ngrams Dec 2020.csv")
# ngrams_listed=data.frame(code=ngrams_listed$EGT_Species.Code,year=ngrams_listed$YearRM,ngrams_science=ngrams_listed$Scientific_RM,ngrams_common=ngrams_listed$Common_RM)
# 
# dat_listed=merge(dat_listed,ngrams_listed,all.x=T)
# 
# #same transformation as before for ngrams - set NAs as minimum value and log transform
# min_common=min(dat_listed$ngrams_common[which(dat_listed$ngrams_common>0)])
# min_science=min(dat_listed$ngrams_science[which(dat_listed$ngrams_science>0)])
# 
# dat_listed$ngrams_common[which(is.na(dat_listed$ngrams_common)|dat_listed$ngrams_common==0)]=min_common
# dat_listed$ngrams_science[which(is.na(dat_listed$ngrams_science)|dat_listed$ngrams_science==0)]=min_science
# dat_listed$ngrams_common=log(dat_listed$ngrams_common);dat_listed$ngrams_science=log(dat_listed$ngrams_science)
# 
# dat_listed$ngrams_common[which(is.na(dat_listed$ngrams_common))]=0
# dat_listed$ngrams_science[which(is.na(dat_listed$ngrams_science))]=0
# dat_listed$ngrams_common=(dat_listed$ngrams_common-mean(dat_listed$ngrams_common,na.rm=T))/sd(dat_listed$ngrams_common,na.rm=T)
# dat_listed$ngrams_science=(dat_listed$ngrams_science-mean(dat_listed$ngrams_science,na.rm=T))/sd(dat_listed$ngrams_science,na.rm=T)
# 
# dat_listed$yearssincelisting=dat_listed$year-dat_listed$datelisted
# dat_listed$yearssincelisting[which(dat_listed$yearssincelisting<=0)]=NA
# 
# dat_listed$extinct=ifelse(dat_listed$status_species%in%c("Extinct","Prob. Extinct"),1,0)
# 
# #fill in a few na priority numbers with previous year - the vast majority do not change over time and this avoids dropping those species-years
# dat_listed=dat_listed%>%
#   group_by(code)%>%
#   arrange(year)%>%
#   tidyr::fill(priority,priority_threat,priority_potential,priority_rarity,.direction="downup")
# write.csv(dat_listed,"Writing Up and Presentations/JAERE Submission/Final Submission/spendingdat.csv")
load("spendingdat.csv")

dat_listed=read.csv("spendingdat.csv")

fit2=felm(I(log(spending))~status+extinct+taxon+region_num+I(log(rangeareas))+ngrams_science+ngrams_common+priority_threat+priority_rarity+priority_potential+conflict+I(log(yearssincelisting))+I(log(ngenus))|year|0|family,data=distinct(dat_listed[-which(dat_listed$spending==0|dat_listed$CommonNameFlag==1),]))

fit2_speciesfe=felm(I(log(spending))~status+ngrams_science+ngrams_common+priority_threat+priority_rarity+priority_potential+conflict+I(log(yearssincelisting))|year+code|0|family,data=distinct(dat_listed[-which(dat_listed$spending==0|dat_listed$CommonNameFlag==1),]))


#try out correlated random effects
library(lme4)
dat_listed$year=as.factor(dat_listed$year)
fit2_cre=lmer(I(log(spending))~taxon+status+extinct+priority_threat+priority_rarity+priority_potential+conflict+region_num+I(log(rangeareas))+ngrams_science+ngrams_common+I(log(yearssincelisting))+I(log(ngenus))+(1|family/code)+year,data=distinct(dat_listed[-which(dat_listed$spending==0|dat_listed$CommonNameFlag==1),]))

datused=dat[as.numeric(rownames(attributes(fit2_cre)$frame)),]
#make figure 1a
taxonspend=dat_listed%>%
  group_by(code)%>%
  dplyr::summarize(meanannualspending=mean(spending,na.rm=T),taxon=taxon[1])%>%
  group_by(taxon)%>%
  dplyr::summarize(meanspending=quantile(meanannualspending,0.5,na.rm=TRUE))
taxonspend$taxon=factor(taxonspend$taxon,levels=taxonspend$taxon[rev(order(taxonspend$meanspending))])
a=ggplot(taxonspend,aes(x=taxon,y=meanspending/1e6))+geom_bar(stat="identity",fill="black")+theme_bw()+labs(x="Taxon",y="Median Annual Recovery\nSpending per Species\n(Millions of Dollars)")
a=a+theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1, face="bold"))

stargazer(fit2_cre,fit2_speciesfe,type="html",dep.var.labels = "Annual Spending",column.labels = c("Full Model","Species FEs"),out="Writing Up and Presentations\\JAERE Submission\\JAERE Resubmission\\Second Revision\\table3.html",model.numbers = FALSE,single.row = TRUE,covariate.labels=c("Birds","Fish","Invertebrates","Mammals","Plants","Reptiles","Threatened","Extinct","Threat","Rarity","Potential","Conflict","FWS Regions","Range Area (logged)","Scientific Ngram","Common Ngram","Years Listed (logged)","Genus Size (logged)",1994:2016,"Constant"))

#3------Net Present Costs of Listing for CRE Model --------------
#discount totals using gamma discounting schedule 
ntime=100
rho=numeric(length=ntime)
for(i in 1:length(rho)) rho[i]=ifelse(i<6,4,ifelse(i<26,3,ifelse(i<76,2,ifelse(i<301,1,0))))
#estimate annual average probability of delisting
delisting=read.csv("delistings.csv")
delisting$Recovered[which(is.na(delisting$Recovered))]=0;delisting$Extinct[which(is.na(delisting$Extinct))]=0
delisting$Total=delisting$Recovered+delisting$Extinct
nspecies=dat_listed%>%group_by(year)%>%dplyr::summarise(nspecies=length(unique(code)))
nspecies$nspecies[which(nspecies$year%in%1994:1995)]=NA #lots of species missing in 1994-1995
delisting=merge(delisting,nspecies,by.x="Year",by.y="year")
delisting$rate=delisting$Total/delisting$nspecies
meanannualrate=mean(delisting$rate,na.rm=T)

rho=rho+meanannualrate*100

df=numeric(length=ntime);df[1]=1/(1+rho[1]/100)
for(i in 2:length(df)) df[i]=df[i-1]*1/(1+rho[i]/100)

#get means of covariates by taxon
taxonmeans=dat_listed%>%
  group_by(taxon)%>%
  dplyr::summarize(region_num=mean(region_num,na.rm=T),rangeareas=mean(rangeareas,na.rm=T),ngrams_science=mean(ngrams_science),ngrams_common=mean(ngrams_common),priority_threat=mean(priority_threat,na.rm=T),priority_rarity=mean(priority_rarity,na.rm=T),priority_potential=mean(priority_potential,na.rm=T),conflict=mean(conflict,na.rm=T),ngenus=mean(ngenus,na.rm=T))

npclisting_cre=data.frame(npc_milliondollars=numeric(),lower=numeric(),upper=numeric())
nsim=1000


z=getME(fit2_cre,"Zt")
samps=matrix(nrow=nsim,ncol=7)

datused=distinct(dat_listed[-which(dat_listed$spending==0|dat_listed$CommonNameFlag==1),])
for(i in 1:dim(taxonmeans)[1]){
  newdat=data.frame(status=rep("T",ntime),extinct=rep(0,ntime),yearssincelisting=1:ntime,year=rep("2015",ntime)) #chose 2015 for simulation as year FE is closest to average of the last 10 years
  taxdat=cbind(newdat,taxonmeans[i,])
  codefams=datused[which(datused$taxon==taxonmeans$taxon[i]),]
  codefams=unique(data.frame(as.character(codefams$code),as.character(codefams$family)))
  match=paste0(codefams[,1],":",codefams[,2])
  codefams=codefams[which(match%in%rownames(z)),]
  colnames(codefams)=c("code","family")
  fulldat=cbind(taxdat,codefams[1,])
  for(j in 2:dim(codefams)[1]){
    tempdat=cbind(taxdat,codefams[j,])
    fulldat=rbind(fulldat,tempdat)
  }
  sim=exp(simulate(fit2_cre,nsim=nsim,seed=1969,re.form=NULL,newdata=fulldat,allow.new.levels=FALSE))
  sim$year=as.factor(fulldat$yearssincelisting)
  yearsim=sim%>% #this averages over the species random effects for each taxon
    group_by(year)%>%
    dplyr::summarize(across(contains("sim"),~mean(.x,na.rm=T)))%>%
    arrange(year)
  
  samps[,i]=t(yearsim[,2:(nsim+1)])%*%df/1e6
  central=mean(samps[,i]);bounds=quantile(samps[,i],c(0.025,0.975))
  
  npclisting_cre=rbind(npclisting_cre,c(central,bounds))
  
}


#.4 ----------- extinction risk change with temperature -------

#urban extinction risk analysis
dat=read.csv("Urban Ext Risk Data/northamurbanextrisk.csv")

#reassign birdsmammalsbutterflies to birds and change weighting, based on distribution from initial study
dat$Weight[which(dat$Taxa=="birdsmammalsbutterflies")]=222
dat$Taxa[which(dat$Taxa=="birdsmammalsbutterflies")]="birds"
dat$Taxa=factor(dat$Taxa)

dat$TaxaShort=fct_collapse(dat$Taxa,vertebrates=c("amphibians","birds","fish","mammals","reptiles"))

mod1=felm(Percent~Pre.Ind.Rise*Threshold-Threshold-1|0|0|Author,data=dat,weight=dat$Weight)
mod2=felm(Percent~Pre.Ind.Rise*TaxaShort+Pre.Ind.Rise*Threshold-Threshold-TaxaShort-1|0|0|Author,data=dat,weight=dat$Weight)
waldtest(mod2,c(F,T,F,T,F)) #no evidence of different effect of temperature on extinction for different taxa

#calculate species at risk of extinction and confidence intervals
temp=c(0,1,2,3,4,5)
threshold=0.9

predex=matrix(nrow=length(temp),ncol=2);predex[1,]=c(0,0)
for(i in 2:length(temp)){
  xmat=c(temp[i],temp[i]*threshold)
  predex[i,1]=coefficients(mod1)%*%xmat
  predex[i,2]=sqrt(xmat%*%mod1$vcv%*%xmat)
}

predex=as.data.frame(predex);colnames(predex)=c("Extinction","SEs");predex$Temp=temp
a=ggplot(predex,aes(x=temp,y=Extinction*100,ymin=(Extinction-1.96*SEs)*100,ymax=(Extinction+1.96*SEs)*100))+geom_line(lwd=2)+geom_ribbon(alpha=0.2,fill="blue")+theme_bw(base_size = 18)+labs(x="Temperature Rise Above Pre-Industrial (Degrees C)",y="Species at Risk of Extinction (%)")
a

# 5. ------------Simulation of effect of climate change on listing and spending ----------------

nboots=1000

temp=c(1,2,3,4,5)
threshold=0.9

unknown=FALSE #variable indicating whether species with status currently unknown are included in analysis

speciessample=speciesdat[complete.cases(speciesdat[,c(9,10,12)]),]#this limits the spcies sampled to only those with all the covariates in the listing regression
speciessample=speciessample[-which(speciessample$status%in%c("Extinct","Prob. Extinct")),] #don't include species that are already extinct
speciessample_nounknowns=speciessample[-which(speciessample$status=="UNK"),]

reps_spending=array(dim=c(length(levels(speciessample$taxon)),nboots,length(temp)))
reps_listed=array(dim=c(length(levels(speciessample$taxon)),nboots,length(temp)))
reps_newstatusone=matrix(nrow=nboots,ncol=length(temp))

for(i in 1:nboots){
  #first draw effect of temperature on extinction risk
  tempeffect=mvrnorm(n=1,mu=mod1$coefficients,Sigma=mod1$vcv)
  #calculate extinction risk
  extrisk=tempeffect[1]*temp+tempeffect[2]*temp*threshold
  
  #for each delta t, draw species and calculate expected listing and spending changes
  for(j in 1:length(temp)){
    if(unknown==TRUE){
      speciesatrisk=sample(x=1:dim(speciessample)[1],size=extrisk[j]*dim(speciessample)[1],replace=FALSE)
      listingsample=speciessample[speciesatrisk,]
    }
    if(unknown==FALSE){
      speciesatrisk=sample(x=1:dim(speciessample_nounknowns)[1],size=extrisk[j]*dim(speciessample_nounknowns)[1],replace=FALSE)
      listingsample=speciessample_nounknowns[speciesatrisk,]
    }
    listingsample=listingsample[-which(listingsample$listed==1),]#remove species that are already listed
    #predict probability of listing using species covariates, after changing conservation status to 1
    reps_newstatusone[i,j]=sum(listingsample$status!=1)
    listingsample$status_post=1
    #sample parameters of listing regression
    listingcoefs=fit1_ses_family$replicates[sample(1:dim(fit1_ses_family$replicates)[1],size=1),]
    #get predicted probability of listing, given conservation status of 1
    intercepts=numeric(length=dim(listingsample)[1])
    for(k in 1:length(intercepts)) intercepts[k]=listingcoefs[1]+ifelse(listingsample$taxon[k]=="Mammals",0,listingcoefs[grep(listingsample$taxon[k],names(listingcoefs))])
    speciesvar=listingsample[,c(10,9,12)];speciesvar$ngenus=log(speciesvar$ngenus)
    fit_post=intercepts+as.matrix(speciesvar)%*%listingcoefs[17:19]
    probs_post=exp(fit_post)/(1+exp(fit_post))
    statuscoef=numeric(length=dim(listingsample)[1])
    for(k in 1:length(statuscoef)) statuscoef[k]=ifelse(listingsample$status[k]==1,0,listingcoefs[grep(listingsample$status[k],names(listingcoefs))])
    fit_pre=intercepts+as.matrix(speciesvar)%*%listingcoefs[17:19]+statuscoef
    probs_pre=exp(fit_pre)/(1+exp(fit_pre))
    
    #now sample spending conditional on listing and calculate expected spending
    spendingsamp=samps[sample(1:dim(samps)[1],size=1),]
    spendingsamp=data.frame(spending=spendingsamp,taxon=taxonmeans$taxon)
    listingsample$probchange=probs_post-probs_pre
    listingsample=merge(listingsample,spendingsamp)
    #summarize expected number of additional species listed and expected spending
    listingsample=listingsample%>%
      group_by(taxon,.drop=FALSE)%>%
      dplyr::summarize(nlisted=sum(probchange),expspending=sum(probchange*spending))
    reps_listed[,i,j]=listingsample$nlisted;reps_spending[,i,j]=listingsample$expspending
  }
  print(i)
}
save(reps_listed,reps_spending,reps_newstatusone,file=ifelse(unknown==TRUE,"reps_withunknowns.Rdat","reps_nounknowns.Rdat"))

dimnames(reps_listed)=list(levels(speciesdat$taxon),1:nboots,temp)
reps_listed=melt(reps_listed)
colnames(reps_listed)=c("taxon","rep","temp","listed")
listed=reps_listed%>%
  group_by(taxon,temp)%>%
  dplyr::summarize(mean=mean(listed),lower=quantile(listed,probs=0.025),upper=quantile(listed,probs=0.975))

zeroes=data.frame(taxon=levels(listed$taxon),temp=rep(0,length(levels(listed$taxon))),mean=rep(0,length(levels(listed$taxon))),lower=rep(0,length(levels(listed$taxon))),upper=rep(0,length(levels(listed$taxon))))
listed=bind_rows(list(listed,zeroes))

dimnames(reps_spending)=list(levels(speciesdat$taxon),1:nboots,temp)
reps_spending=melt(reps_spending)
colnames(reps_spending)=c("taxon","rep","temp","spending")
spending=reps_spending%>%
  group_by(temp,rep)%>%
  dplyr::summarize(spending=sum(spending))%>%
  group_by(temp)%>%
  dplyr::summarize(mean=mean(spending),lower=quantile(spending,probs=0.025),upper=quantile(spending,probs=0.975))

zeroes=data.frame(temp=0,mean=0,lower=0,upper=0)
spending=bind_rows(list(spending,zeroes))



spending_nounknowns=spending;listed_nounknowns=listed;reps_newstatusone_nounknowns=reps_newstatusone
load("reps_withunknowns.Rdat") #re-run lines 403-423
spending_nounknowns$type=rep("Without Unknowns",dim(spending_nounknowns)[1])
spending$type=rep("With Unknowns",dim(spending)[1])
spending=bind_rows(list(spending,spending_nounknowns))

colnames(reps_newstatusone_nounknowns)=temp;rownames(reps_newstatusone_nounknowns)=1:nboots
reps_newstatusone_nounknowns=melt(reps_newstatusone_nounknowns)
colnames(reps_newstatusone_nounknowns)=c("rep","temp","nspecies")
imperilled=reps_newstatusone_nounknowns%>%
  group_by(temp)%>%
  dplyr::summarise(nspecies=mean(nspecies))
zeroes=data.frame(temp=0,nspecies=0);imperilled=bind_rows(imperilled,zeroes)
listedtot=listed_nounknowns%>%
  group_by(temp)%>%
  dplyr::summarise(nspecies_listed=sum(mean))
imperilled=merge(imperilled,listedtot)
imperilled$mean=imperilled$nspecies-imperilled$nspecies_listed
imptemp=data.frame(taxon=rep("Imperilled, Not Listed",length(temp)+1),temp=c(0,temp),mean=imperilled$mean,lower=rep(NA,6),upper=rep(NA,6))
listed_nounknowns=bind_rows(listed_nounknowns,imptemp)
listed_nounknowns=listed_nounknowns[-which(listed_nounknowns$taxon=="Protists"),]
listed_nounknowns$taxon=factor(listed_nounknowns$taxon)
listed_nounknowns$taxon=fct_relevel(listed_nounknowns$taxon,"Imperilled, Not Listed","Amphibians","Birds","Fishes","Fungi","Invertebrates","Mammals","Plants","Reptiles")

a=ggplot(listed_nounknowns,aes(x=temp,y=mean,fill=taxon))+geom_area()+theme_bw(base_size=18)+labs(x="Temperature Increase Above Pre-Industrial (Degrees C)",y="Expected Number of Species",fill=NULL)
a=a+scale_fill_manual(values=c("lightgrey",rev(bpy.colors(8,cutoff.tails = 0.2))))
a

a=ggplot(spending,aes(x=temp,y=mean/1000,ymin=lower/1000,ymax=upper/1000,group=type,lty=type))+geom_line(lwd=2)+geom_errorbar(width=0.2,lty=1)+theme_bw(base_size=18)
a=a+labs(x="Temperature Increase Above Pre-Industrial (Degrees C)",y="NPC of 100 Years of Spending on Listed Species\n(Billions of Dollars)",lty="")
a=a+scale_linetype_manual(values=c(3,1))
a
#find committed spending for currently listed species
npclisting_cre$taxon=taxonmeans$taxon
colnames(npclisting_cre)[1:3]=c("mean","lower","upper")

#get unique set of listed species
uniquelisted=dat_listed%>%
  dplyr::select(code,taxon)%>%
  distinct()


#time series calcs
timeseries=datfull%>%
  filter(is.finite(USFWS_Earliest.Date.Listed))%>%
  group_by(USFWS_Earliest.Date.Listed,Tax.Group,.drop=FALSE)%>%
  dplyr::summarise(total=length(unique(EGT_Species.Code)))

timeseries=merge(timeseries,npclisting[,1:2],by.x="Tax.Group",by.y="taxon")

spendingtimeseries=timeseries%>%
  group_by(USFWS_Earliest.Date.Listed)%>%
  dplyr::summarise(npvspending=sum(total*npc_milliondollars))
spendingtimeseries$cumulative=cumsum(spendingtimeseries$npvspending)




#supplementary figure 1
a=ggplot(dat,aes(x=log(ngenus),y=log(evdist)))+geom_point()+geom_smooth(method='lm', formula= y~x)+theme_bw(base_size = 18)+labs(x="Log Number of Species in Genus",y="Log Evolutionary Distinctiveness")
a=a+labs(x="Temperature Increase Above Pre-Industrial (Degrees C)",y="NPV of 100 Years Spending on Listed Species\n(Billions of Dollars)")



#get data for endangered species with WTP estimates
wtp=read.csv("dataformarc.csv")
names=as.character(unique(wtp$GNAME))
names=names[-which(is.na(names))]
#add a couple extra
names=append(names,c("Dryobates borealis","Halophila johnsonii","Delphinapterus leucas"))
wtpspending=datfull[which(datfull$GNAME%in%names),]

write.csv(wtpspending,file="Writing Up and Presentations\\JAERE Submission\\JAERE Resubmission\\dataformarc_resubmission.csv")

means=datused%>%
  group_by(year)%>%
  dplyr::summarise(spending=mean(spending,na.rm=T))
