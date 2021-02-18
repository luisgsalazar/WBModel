
Scenario = 41:42
par(mfrow = c(2,1)) 
load("habitats_Dep64.RData")
library(rgeos)

gArea(habitats)/1e6->surface

for (Scen in Scenario){
load(paste0("./Outputs/Scenario_",Scen,"/",Scen,".RData"))
do.call(rbind,res)->a
a1=a[,c("DayOutInfMat","DayOutPopMat","DayOutAniMat","DayOutCarcMat","DayOutInfAniMat")]
a2<-a[,c("NewInfAnimals","NewInfCarcass","NewInfGroups","InfectedPerCells")]
apply(a1,2,identity)->z
pop<-do.call(cbind,z$DayOutAniMat)
Infect<-do.call(cbind,z$DayOutInfMat)
carcasses<-do.call(cbind,z$DayOutCarcMat)

apply(a2,2,function(x) do.call(rbind,x))->z
InfPerCells<-z$InfectedPerCells



plot(pop[,1]/surface,typ="l",xlab="temps (jours)",ylab="Densité (sangliers/km²)",ylim = c(0,5))
apply(pop[,2:9]/surface,2,lines,col=2:9)

}

for (Scen in Scenario){
  load(paste0("./Outputs/Scenario_",Scen,"/",Scen,".RData"))
  do.call(rbind,res)->a
  a1=a[,c("DayOutInfMat","DayOutPopMat","DayOutAniMat","DayOutCarcMat","DayOutInfAniMat")]
  a2<-a[,c("NewInfAnimals","NewInfCarcass","NewInfGroups","InfectedPerCells")]
  apply(a1,2,identity)->z
  # pop<-do.call(cbind,z$DayOutAniMat)
  Infect<-do.call(cbind,z$DayOutInfAniMat)
  # carcasses<-do.call(cbind,z$DayOutCarcMat)
  
  apply(a2,2,function(x) do.call(rbind,x))->z
  InfPerCells<-z$InfectedPerCells
  
  
  
  plot(Infect[,1],typ="l",xlab="temps (jours)",ylab="nombre d'individus",
       ylim = c(0,max(Infect)+100))
  apply(Infect[,2:9],2,lines,col=2:9)
  
}

for (Scen in Scenario){
  load(paste0("./Outputs/Scenario_",Scen,"/",Scen,".RData"))
  do.call(rbind,res)->a
  a1=a[,c("DayOutInfMat","DayOutPopMat","DayOutAniMat","DayOutCarcMat","DayOutInfAniMat")]
  a2<-a[,c("NewInfAnimals","NewInfCarcass","NewInfGroups","InfectedPerCells")]
  plot(c(0,3000),c(0,200),typ='n',xlab="temps (jours)",ylab="Incidence")
  NewInfAnimals<-a2[,'NewInfAnimals']
  lapply(NewInfAnimals, function(x) lines(unique(x[,2]),table(x[,2])))  
}
# 
# a<-read.delim(paste(file,"-FNewInfAnimals.txt",sep=''),sep=' ')
# plot(unique(a[,2]),table(a[,2]),typ='l',xlab="temps (jours)",ylab="Incidence")  
# plot(unique(a[,2]),cumsum(table(a[,2])),typ='l',xlab="temps (jours)",ylab="Prévalence")  
# 
# 
# 
# plot(carcasses$V1,typ="l",xlab="temps (jours)",ylab="nombre de carcasses infectieuses",ylim = c(0,3000))


# IGroups<-read.delim(paste(file,"-FNewInfGroups.txt",sep=''),sep=' ')
# 
# InfPerCells<-read.table(paste(file,"-FDayOutInfPerCell.txt",sep=''),sep=' ')
# splitIPC<-split(InfPerCells,InfPerCells[,3])
# lapply(splitIPC, function(x) lines(x[,2]-730,x[,4],col=x[,3])
# )
# 
# day=730
# habitats$InfStatus=0
# habitats$alpha=0
# tmp=subset.data.frame(InfPerCells,InfPerCells$V2==day)
# habitats$InfStatus[match(tmp[,3],habitats$ID)]=1
# habitats$alpha[match(tmp[,3],habitats$ID)]=tmp[,4]/tmp[,5]
# library(tmap)
# tm_shape(habitats)+tm_fill("InfStatus")
# library(leaflet)
# library(sf)
# col=rev(heat.colors(10))[c(1,10)]
# habitats$col=col[habitats$InfStatus+1]
# habitats$col[habitats$alpha>0]=adjustcolor(habitats$col[habitats$alpha>0],
#                                            habitats$alpha[habitats$alpha>0])
# 
# tm_shape(habitats)+tm_fill("col")
# 

