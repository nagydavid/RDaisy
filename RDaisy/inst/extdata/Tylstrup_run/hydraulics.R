######hydraulics

library(SoilHyP)
library(foreach)
library(doParallel)

TYLS_ret<-fread("F:/RDaisy_calib/obs/retention.csv")
TYLS_kh<-fread("F:/RDaisy_calib/obs/Kunsat.csv")
TYLS_ks<-fread("F:/RDaisy_calib/obs/Ksat.csv")

TYLS_ret


TYLS_ret.m<-melt.data.table(TYLS_ret,id.vars = c("PROFILNR","HORISONTNR","retID"), measure.vars = c("PF 1,0","PF 1,2","PF 1,7","PF 2,0",
                                                                                            "PF 2,2","PF 3,0","PF 4,2"))
##########
#retention  
##########

TYLS_ret.m[,pF:=ifelse(variable=="PF 1,0",1,
                       ifelse(variable=="PF 1,2",1.2,
                              ifelse(variable=="PF 1,7",1.7,
                                     ifelse(variable=="PF 2,0",2,
                                            ifelse(variable=="PF 2,2",2.2,
                                                   ifelse(variable=="PF 3,0",3,
                                                          ifelse(variable=="PF 4,2",4.2,0)))))))]

TYLS_ret.m[,suc:=10^pF]
TYLS_ret.m[,th:=value][,variable:=NULL][,value:=NULL]

TYLS_ret.m[,horID:=paste(PROFILNR,HORISONTNR,sep="-")]

###################################
#Unsaturated hydraulic conductivity
###################################
TYLS_kh.m<-TYLS_kh[,.SD,.SDcols=c("PROFILNR","HORISONTNR","KOLONNENR","h","kh")]

TYLS_kh.m[,kh:=kh/24]

setkey(TYLS_kh.m,"PROFILNR","HORISONTNR","KOLONNENR")

TYLS_ks[,h:=0]
TYLS_ks[,Ksat:=Ksat/24]

TYLS_ks<-TYLS_ks[,.SD,.SDcols=c("PROFILNR","HORISONTNR","KOLONNENR","h","Ksat")]

setkey(TYLS_ks.m,"PROFILNR","HORISONTNR","KOLONNENR")

TYLS_k<-rbind(TYLS_ks.m[KOLONNENR %in%TYLS_kh.m$KOLONNENR][,kh:=Ksat][,.SD,.SDcols=c("PROFILNR","HORISONTNR","KOLONNENR","h","kh")],
      TYLS_kh.m)

TYLS_k<-TYLS_k[ with(TYLS_k, order(PROFILNR,HORISONTNR,KOLONNENR,h)),][,suc:=h][,h:=NULL]

TYLS_k[,horID:=paste(PROFILNR,HORISONTNR,sep="-")]

TYLS_k[,kolID:=paste(PROFILNR,HORISONTNR,KOLONNENR,sep="-")]

TYLS_HOR<-TYLS_k[,.N,by=horID]

#############################
#Combined list of ret and con
#############################

retlist<-NULL

for(i in 1:nrow(TYLS_HOR)){
  
  for(j in 1:length(levels(TYLS_ret.m[horID == TYLS_HOR[i]$horID][,as.factor(retID)]))){
    
    x<-TYLS_ret.m[horID == TYLS_HOR[i]$horID][retID==j][,kh:=NA][,.SD,.SDcol=c("PROFILNR","HORISONTNR","th","suc","kh","horID")]
    
    for(k in 1:length(levels(TYLS_k[horID == TYLS_HOR[i]$horID][,as.factor(KOLONNENR)]))){
      
      y<-TYLS_k[horID == TYLS_HOR[i]$horID][KOLONNENR==TYLS_k[horID == TYLS_HOR[i]$horID][,unique(KOLONNENR)][k]][,th:=NA][,.SD,.SDcol=c("PROFILNR", "HORISONTNR","th","suc","kh","horID")]
      z<-rbind(x,y)[,kolID:=paste(TYLS_k[horID == TYLS_HOR[i]$horID][KOLONNENR==TYLS_k[horID == TYLS_HOR[i]$horID][,unique(KOLONNENR)][k]][1]$kolID,j,sep="-")]     
      retlist<-rbind(retlist,z)
    }
    
  }
  
}

retlist[,.N,by=c("kolID")]

retlist<-retlist[!suc==0][,suc:=-suc]


swcobs<-NULL
swcsim<-NULL
Kobs<-NULL
Ksim<-NULL
OutvGM<-NULL

for(i in 1:length(unique(retlist$kolID))){

      t1<-unique(retlist[,.(horID,kolID)])
       t1[i]$kolID
       
      t2<-unique(retlist[,.(horID,kolID)])
       t2[i]$horID
  
ans <- fitSHP(obs         = list(th = retlist[kolID==t1[i]$kolID & !is.na(th)]$th, K = retlist[kolID==t1[i]$kolID & !is.na(kh)]$kh),
              suc          = list(th = retlist[kolID==t1[i]$kolID & !is.na(th)]$suc, K =  retlist[kolID==t1[i]$kolID & !is.na(kh)]$suc),
              FUN.shp      = 'vg',
              modality     = 'uni',
              par.shp      =  NULL,
              par          = c(ths = 0.4, thr = 0.1, alfa = log(0.01), n = log(1.1), ks=log(1) ,tau=0.5 ),
              lower        = c(ths = 0.36, thr = 0.012, alfa = log(0.001), n = log(1.01),ks=log(0.2375) ,tau=-3 ),
              upper        = c(ths = 0.493699989, thr = 0.054099998, alfa = log(1), n = log(5),ks=log(14.80875) ,tau=3 ),
              log          = c('alfa', 'n', 'ks'),
              fit          = 'both',
              weighting    = '2step',
              control      = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7),
              suc.negativ  = TRUE,
              integral     = FALSE,
              L            = 0,
              print.info   = TRUE)
  
  v<-data.table(as.data.table(ans$par),RMSE.th = ans$RMSE.th,RMSE.K = ans$RMSE.K,kolID = t1[i]$kolID,horID = t2[i]$horID)
  
  print(v)

  assign(t1[i]$kolID,plot.fitSHP.D(ans,ttle = t1[i]$kolID))
  
  Data<-get(t1[i]$kolID)
  
  interim<-as.data.table(Data$Data.SWC$swc.obs)[,kolID:=t1[i]$kolID][,horID:=t2[i]$horID]
  swcobs<-rbind(swcobs,interim)
  
  interim<-as.data.table(Data$Data.SWC$swc.sim)[,kolID:=t1[i]$kolID][,horID:=t2[i]$horID]
  swcsim<-rbind(swcsim,interim)
  
  interim<-as.data.table(Data$Data.K$K.obs)[,kolID:=t1[i]$kolID][,horID:=t2[i]$horID]
  Kobs<-rbind(Kobs,interim)
  
  interim<-as.data.table(Data$Data.K$K.sim)[,kolID:=t1[i]$kolID][,horID:=t2[i]$horID]
  Ksim<-rbind(Ksim,interim)
  
  rm(list = t1[i]$kolID)
  
  OutvGM<-rbind(OutvGM, v)

}
############## 
#RIBBON SWC
##############
swcobs[,pF:=log10(obs.swc.h)]
swcsim[,pF:=log10(sim.swc.h)]

SWC_ribbon <- as.data.table(dcast.data.table(
  swcsim[,c("pF","kolID","horID","sim.swc.th"),with=F],
  pF~kolID,
  value.var = "sim.swc.th",
  fill = 0
))

min.max.SWC<-NULL

for(i in 1:length(unique(retlist$horID))){
  
  sub_swc<-as.data.table(t(SWC_ribbon[,-1]))
  colnames(sub_swc)<-as.character(SWC_ribbon$pF)
  sub_swc[,kolID:=colnames(SWC_ribbon[,-1])]
  
  
  min.swc<-sub_swc[kolID %like% unique(retlist$horID)[i],lapply(.SD,min),.SDcol=as.character(SWC_ribbon$pF)]
  max.swc<-sub_swc[kolID %like% unique(retlist$horID)[i],lapply(.SD,max),.SDcol=as.character(SWC_ribbon$pF)]
  
  hor.1<-c("min.A1","min.B1","min.C1","min.A2","min.B2","min.C2")
  hor.2<-c("max.A1","max.B1","max.C1","max.A2","max.B2","max.C2")  
  
  min.swc[,hor:=hor.1[i]]
  max.swc[,hor:=hor.2[i]]
  
  min.max<-rbind(min.swc,max.swc)
  
  min.max.SWC<-rbind(min.max.SWC,min.max)
}

SWC_rib<-as.data.table(t(min.max.SWC[,-101]))

colnames(SWC_rib)<-min.max.SWC$hor
SWC_rib[,pF:=as.numeric(colnames(min.max.SWC[,-101]))]

############## 
#RIBBON K
##############
Kobs[,pF:=log10(obs.K.h)]
Ksim[,pF:=log10(sim.K.h)]
Ksim[,K:=log10(sim.K.ku)]
Kobs[,K:=log10(obs.K.ku)]


K_ribbon <- as.data.table(dcast.data.table(
  Ksim[,c("pF","kolID","horID","K"),with=F],
  pF~kolID,
  value.var = "K",
  fill = 0
))

min.max.K<-NULL

for(i in 1:length(unique(retlist$horID))){
  
  sub_K<-as.data.table(t(K_ribbon[,-1]))
  colnames(sub_K)<-as.character(K_ribbon$pF)
  sub_K[,kolID:=colnames(K_ribbon[,-1])]
  
  
  min.K<-sub_K[kolID %like% unique(retlist$horID)[i],lapply(.SD,min),.SDcol=as.character(K_ribbon$pF)]
  max.K<-sub_K[kolID %like% unique(retlist$horID)[i],lapply(.SD,max),.SDcol=as.character(K_ribbon$pF)]
  
  hor.1<-c("min.A1","min.B1","min.C1","min.A2","min.B2","min.C2")
  hor.2<-c("max.A1","max.B1","max.C1","max.A2","max.B2","max.C2")  
  
  min.K[,hor:=hor.1[i]]
  max.K[,hor:=hor.2[i]]
  
  min.max<-rbind(min.K,max.K)
  
  min.max.K<-rbind(min.max.K,min.max)
}

K_rib<-as.data.table(t(min.max.K[,-101]))

colnames(K_rib)<-min.max.K$hor
K_rib[,pF:=as.numeric(colnames(min.max.K[,-101]))]

#################
#plots
#################


grid.arrange(ggplot()+
               geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.A1, ymax=max.A1,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3087-1"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3087-1"],aes(x=pF ,y=sim.swc.th,color=kolID)) + 
               theme(legend.position="none"),
             
             ggplot()+ geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.B1, ymax=max.B1,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3087-3"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3087-3"],aes(x=pF ,y=sim.swc.th,color=kolID)) + 
               theme(legend.position="none"),
             
             ggplot()+ geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.C1, ymax=max.C1,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3087-4"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3087-4"],aes(x=pF ,y=sim.swc.th,color=kolID)) +
               theme(legend.position="none"),
             
             ggplot()+ geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.A2, ymax=max.A2,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3088-1"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3088-1"],aes(x=pF ,y=sim.swc.th,color=kolID)) + 
               theme(legend.position="none"),
             
             ggplot()+ geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.B2, ymax=max.B2,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3088-2"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3088-2"],aes(x=pF ,y=sim.swc.th,color=kolID)) +
               theme(legend.position="none"),
             
             ggplot()+ geom_ribbon(data=SWC_rib,aes(x=pF,ymin=min.C2, ymax=max.C2,  alpha=10^-5),fill="grey")+coord_flip()+
               geom_point(data=swcobs[horID=="3088-4"],aes(x=pF ,y=obs.swc.th,color=kolID))+
               geom_line(data=swcsim[horID=="3088-4"],aes(x=pF ,y=sim.swc.th,color=kolID)) +
               theme(legend.position="none"),nrow=2,ncol=3)

grid.arrange(
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.A1, ymax=max.A1,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3087-1"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3087-1"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.B1, ymax=max.B1,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3087-3"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3087-3"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.C1, ymax=max.C1,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3087-4"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3087-4"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.A2, ymax=max.A2,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3088-1"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3088-1"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.B2, ymax=max.B2,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3088-2"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3088-2"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),
  
  ggplot()+geom_ribbon(data=K_rib,aes(x=pF,ymin=min.C2, ymax=max.C2,  alpha=10^-5),fill="grey")+
    geom_point(data=Kobs[horID=="3088-4"],aes(x=pF ,y=K,color=kolID))+
    geom_line(data=Ksim[horID=="3088-4"],aes(x=pF ,y=K,color=kolID)) + theme(legend.position="none"),nrow=2,ncol=3)

##############################
#Calculating parameter ranges
##############################

OutvGM[,COLUMN:=substr(gsub(pattern=paste0(horID,"-"),replacement = "",kolID),1,2),by=horID]
OutvGM[,COLUMN:=as.factor(COLUMN)]
OutvGM[,HOR:=as.factor(ifelse(grepl("-1",horID) ,"A",ifelse(grepl("-4",horID),"C","B")))]

range<-OutvGM[,lapply(.SD,min),.SDcols=c("ths","thr","alfa","n","ks","tau"),by="HOR"][,range:="min"]

range<-rbind(range,OutvGM[,lapply(.SD,max),.SDcols=c( "ths","thr","alfa","n","ks","tau"),by="HOR"][,range:="max"])

range<-data.table(rbind(range[range=="min",.SD,.SDcols=c( "ths","thr","alfa","n","ks","tau")],
                        range[range=="max",.SD,.SDcols=c( "ths","thr","alfa","n","ks","tau")]),HOR=c("A","B","C","A","B","C"),range=c(rep("min",3),rep("max",3)))

