
rm(list=ls())

#!!!!!!!!!!!!!!!#
#!!!! INPUT !!!!#
#!!!!!!!!!!!!!!!#

files = 3   #Vul hier het aantal gemergede bestanden in dat je wil gebruiken om plots te visualiseren

# Geef elk bestand dat gemerged moet worden de naam van een getal, beginnend met 1 en daarna oplopend.
# !!! Plaats de excel bestanden op de Q:/schijf in onderstaande map en 
# Psychologie/Onderwijsinstituut/Onderwijsbalie Psychologie/ICT en Onderwijs/Projecten Paul/Analyses/Turnitin Jos/Output/
# De output zal verschijnen in de map output

#!!!!!!!!!!!!!!!#
#!! END INPUT !!#
#!!!!!!!!!!!!!!!#


##################
# PREPAIR R-FILE #
##################

if("gdata" %in% rownames(installed.packages()) == FALSE) {install.packages("gdata")}
if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
library(gdata)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)
if(Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/")}
if(Sys.info()["sysname"]=="Darwin"){
  setwd("/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output")}



##################
## Start script ##
##################

rdata=list()
students=c()

# Create dataframe for each group
for(i in 1:files){
  nam <- paste("file", i, sep = "")
  assign(nam, read.xls(gsub(" ","",paste(toString(i),".xlsx")),pattern="Schrijver"))
}

# Combine dataframes in a list
for(j in 1:files){
  rdata[[j]]<-get(ls(pattern = "file")[j])
}



#################
# Read Datafile #
#################

# Remove duplicates
for(j in 1:files){
 rdata[[j]]<-rdata[[j]][!duplicated(rdata[[j]][,-1]),]
}

n=ng=rubrics=quickmarks=rep(0,files)
npg=list()
rubrics.names=quickmarks.names=list()

for(i in 1:files){
  data<-rdata[[i]]
  n[i]<-length(unique(data$author))
  ng[i]<-length(unique(data$groep))
  npg[[i]]<-table(data$groep)
  rubrics[i]<-which(names(data)=="titel.y")-which(names(data)=="titel.x")-1
  quickmarks[i]<-length(names(data))-which(names(data)=="titel.y")
  rubrics.names[[i]]<-names(data[1:rubrics[i]+which(names(data)=="titel.x")])
  quickmarks.names[[i]]<-names(data[1:quickmarks[i]+which(names(data)=="titel.y")]) 
  }
info=data.frame(n,ng,npg,rubrics,quickmarks)



##########################
# Aggregate Student Data #
##########################

# Rubrics
'
for(i in 1:files){
  data<-rdata[[1]]
  rubstart<-which(names(data)=="Rubrics.onderdeel")+1
  rubend<-rubstart+rubrics[1]-1
  attach(data[,rubstart:rubend],name=paste(names(data)[rubstart:rubend]),i)
}       




  data<-rdata[[i]]
  for(j in 1:info$rubrics[i]){
    assign(rubrics.names[[j]],data$rubrics.names[[i]][j])
    
    
  } 
}
'

# Quickmarks




####################
#       PLOTS      #
####################


#rem<-c("*","--",NA)
#score<-as.numeric(rubscore[! rubscore %in% rem])


### # Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### NOG IN LOOP ZETTEN voor aantal rubrics en quickmarks (lukte niet): ###

###################
## RUBRIC SCORES ##
###################

for(i in 1:files){
    score<-rdata[[i]][,rubrics.names[[i]]]
    
    for(c in 1:dim(score)[2]){
      score[,c] <- gsub("--", NA, score[,c])
      score[,c] <- gsub("<NA>", NA, score[,c])
      score[,c] <- gsub("\\*", NA, score[,c])
    }
    
    rubplot1<-qplot(score[,1], xlab="Score",ylab="Frequency", main=rubrics.names[[i]][1])
    rubplot2<-qplot(score[,2], xlab="Score",ylab="Frequency", main=rubrics.names[[i]][2])
    rubplot3<-qplot(score[,3], xlab="Score",ylab="Frequency", main=rubrics.names[[i]][3])
    rubplot4<-qplot(score[,4], xlab="Score",ylab="Frequency", main=rubrics.names[[i]][4])
    rubplot5<-qplot(score[,5], xlab="Score",ylab="Frequency", main=rubrics.names[[i]][5])
    
    pdf(file=paste("Rubrics_assignment",i,".pdf",sep = ""),width=15,height=3)
        multiplot(rubplot1,rubplot2,rubplot3,rubplot4,rubplot5,cols=5)
    dev.off()


for(k in 1:dim(score)[1]){
  for(j in 1:dim(score)[2]){
    score[k,j]<-as.numeric(unlist(score[k,j]))
   }
}

score<-score[complete.cases(score),]
score<-matrix(as.numeric(unlist(score)),dim(score)[1],dim(score)[2],byrow=F)
rubcor<-cor(score)
rownames(rubcor)<-rubrics.names[[i]]
colnames(rubcor)<-rubrics.names[[i]]
nam=paste("rubcor",i,sep="")
assign(nam,rubcor)



}


######################
## QUICKMARK SCORES ##
######################

for(i in 1:files){
  score<-rdata[[i]][,quickmarks.names[[i]]]
  
score<-as.data.frame(sapply(score,gsub,pattern="--",replacement="0",fixed=T))
# Onderstaand script zorgt ervoor dat x-as van plots mooi sorteren, maar de x as labels overlappen elkaar dus is niet mooi!
'  score<-as.data.frame(sapply(score,gsub,pattern="1",replacement="01",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="2",replacement="02",fixed=T)) 
  score<-as.data.frame(sapply(score,gsub,pattern="3",replacement="03",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="4",replacement="04",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="5",replacement="05",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="6",replacement="06",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="7",replacement="07",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="8",replacement="08",fixed=T))
  score<-as.data.frame(sapply(score,gsub,pattern="9",replacement="09",fixed=T))
' 
  quickplot1<-qplot(as.factor(na.omit(score[,1])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][1])
  quickplot2<-qplot(as.factor(na.omit(score[,2])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][2])
  quickplot3<-qplot(as.factor(na.omit(score[,3])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][3])
  quickplot4<-qplot(as.factor(na.omit(score[,4])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][4])
  quickplot5<-qplot(as.factor(na.omit(score[,5])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][5])
  quickplot6<-qplot(as.factor(na.omit(score[,6])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][6])
  quickplot7<-qplot(as.factor(na.omit(score[,7])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][7])
  quickplot8<-qplot(as.factor(na.omit(score[,8])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][8])
  quickplot9<-qplot(as.factor(na.omit(score[,9])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][9])
  quickplot10<-qplot(as.factor(na.omit(score[,10])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][10])
  quickplot11<-qplot(as.factor(na.omit(score[,11])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][11])
  quickplot12<-qplot(as.factor(na.omit(score[,12])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][12])
  quickplot13<-qplot(as.factor(na.omit(score[,13])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][13])
  quickplot14<-qplot(as.factor(na.omit(score[,14])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][14])
  quickplot15<-qplot(as.factor(na.omit(score[,15])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][15])
  quickplot16<-qplot(as.factor(na.omit(score[,16])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][16])
  quickplot17<-qplot(as.factor(na.omit(score[,17])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][17])
  quickplot18<-qplot(as.factor(na.omit(score[,18])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][18])
  quickplot19<-qplot(as.factor(na.omit(score[,19])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][19])
  quickplot20<-qplot(as.factor(na.omit(score[,20])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][20])
  quickplot21<-qplot(as.factor(na.omit(score[,21])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][21])
  quickplot22<-qplot(as.factor(na.omit(score[,22])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][22])
  quickplot23<-qplot(as.factor(na.omit(score[,23])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][23])
  quickplot24<-qplot(as.factor(na.omit(score[,24])),ylim=c(0,200), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][24])
  
  
'
  for(q in 1:quickmarks[1]){ 
    quickplot<-qplot(as.factor(score[,q]), xlab="Score",ylab="Frequency", main=quickmarks.names[[i]][q])
    nam=paste("quickplot",q,sep="")
    print(nam)
    assign(nam,quickplot)
  }
 ' 
  pdf(file=paste("Quickmarks_assignment",i,".pdf",sep = ""),width=15,height=15)
  multiplot(quickplot1,quickplot2,quickplot3,quickplot4,quickplot5,quickplot6,quickplot7,quickplot8,quickplot9,quickplot10,
            quickplot11,quickplot12,quickplot13,quickplot14,quickplot15,quickplot16,quickplot17,quickplot18,quickplot19,quickplot20,
            quickplot21,quickplot22,quickplot23,quickplot24,layout=matrix(c(1:9,9,10:24),5,5,byrow=T))
  dev.off()
}



##################################
## MEAN RUBRIC SCORES PER GROUP ##
##################################

for(i in 1:files){
  rdat<-rdata[[i]]
  
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  rdat[,rubrics.names[[i]]]<-sapply(rdat[,rubrics.names[[i]]],as.numeric)
  
  rubscore<-cbind(groep=as.numeric(rdat$groep),rdat[,rubrics.names[[i]]])

  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(mean(., na.rm=TRUE))) ->rub.mean
    names(rub.mean)<-paste(names(rub.mean),i,sep = "")
  
    rubscore %>%
      summarise_each(funs(mean(., na.rm=TRUE))) ->rub.gmean
    
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(sd(., na.rm=TRUE)))->rub.sd
    names(rub.sd)<-paste(names(rub.sd),i,sep = "")
    
    rubscore %>%
      summarise_each(funs(sd(., na.rm=TRUE))) ->rub.gsd
    
  if(i==1){
    rubmeans<-rub.mean
    rubsds<-rub.sd
    rubgmeans<-rub.gmean[,-1]
    rubgsds<-rub.gsd[,-1]
  }
  if(i>1){
    rubmeans<-cbind(rubmeans,rub.mean)
    rubsds<-cbind(rubsds,rub.sd)
    rubgmeans<-rbind(rubgmeans,rub.gmean[,-1])
    rubgsds<-rbind(rubgsds,rub.gsd[,-1])
  }

}

### GENERATE PLOTS ###

for(j in 1:5){
  
  dpoly=data.frame(x=c(1,2,3, 3,2,1), 
                   y=c(rubgmeans[1,j]+rubgsds[1,j],rubgmeans[2,j]+rubgsds[2,j],rubgmeans[3,j]+rubgsds[3,j],
                       rubgmeans[1,j]-rubgsds[1,j],rubgmeans[2,j]-rubgsds[2,j],rubgmeans[3,j]-rubgsds[3,j]), 
                   t=c('a', 'b', 'c',  'c', 'b', 'a'))
  dline=data.frame(x=c(1,2,3),
                   y=c(rubgmeans[1,j],rubgmeans[2,j],rubgmeans[3,j]))
  
  dpoly[dpoly[,2]<0,2]<-0
  
    for(g in 1:ng[1]){
    
      datpoints=data.frame(x=c(1,2,3),
                           y=as.numeric(rubmeans[g,c(1+j,7+j,13+j)]))
      
      q<-qplot(x=seq(1,files,1),y=rep(-100,files),xlim=c(1,files),ylim=c(min(rubmeans[,c(1+j,7+j,13+j)],na.rm=T)-0.5,0.5+max(rubmeans[,c(1+j,7+j,13+j)])),xlab="Assignment", ylab="Score",main=paste("Groep",g, rubrics.names[[1]][j]))
      q<-q + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
             geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
             geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
             theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
             geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue") 
      
        nam=paste("q",g,sep="")
        print(nam)
        assign(nam,q)
    }
  
 pdf(file=paste("Rubric",rubrics.names[[1]][j],"pergroep.pdf"),width=13,height=12)
 multiplot(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
          q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,
          q21,q22,q23,q24,q25,q26,q27,q28,q29,q30,
          layout=matrix(c(1:30),5,6,byrow=T))
dev.off() 

}








### Mean quickmark scores per group ###


for(i in 1:files){
  rdat<-rdata[[i]]
  
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  rdat[,quickmarks.names[[i]]]<-sapply(rdat[,quickmarks.names[[i]]],as.numeric)
  
  rubscore<-cbind(groep=as.numeric(rdat$groep),rdat[,quickmarks.names[[i]]])
 
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(sum(., na.rm=TRUE))) ->quickmark.sum
  names(quickmark.sum)<-paste(names(quickmark.sum),i,sep = "")
  
  rubscore %>%
    summarise_each(funs(sum(., na.rm=TRUE))) ->quickmark.gsum
  
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(sd(., na.rm=TRUE)))->quickmark.sd
  names(quickmark.sd)<-paste(names(quickmark.sd),i,sep = "")
  
  rubscore %>%
    summarise_each(funs(sd(., na.rm=TRUE))) ->quickmark.gsd
  
  if(i==1){
    quickmark.sums<-quickmark.sum
    quickmark.sds<-quickmark.sd
    quickmark.gsums<-quickmark.gsum[,-1]
    quickmark.gsds<-quickmark.gsd[,-1]
  }
  if(i>1){
    quickmark.sums<-cbind(quickmark.sums,quickmark.sum)
    quickmark.sds<-cbind(quickmark.sds,quickmark.sd)
    quickmark.gsums<-rbind(quickmark.gsums,quickmark.gsum[,-1])
    quickmark.gsds<-rbind(quickmark.gsds,quickmark.gsd[,-1])
  }
  
}


quickmark.gsums<-apply(quickmark.sums,2,mean)
quickmark.gsds<-apply(quickmark.sums,2,sd)#/sqrt(15)


### GENERATE PLOTS ###

for(j in 1:24){
  
  dpoly=data.frame(x=c(1,2,3, 3,2,1), 
                   y=c(quickmark.gsums[1+j]+quickmark.gsds[1+j],quickmark.gsums[26+j]+quickmark.gsds[26+j],quickmark.gsums[51+j]+quickmark.gsds[51+j],
                       quickmark.gsums[1+j]-quickmark.gsds[1+j],quickmark.gsums[26+j]-quickmark.gsds[26+j],quickmark.gsums[51+j]-quickmark.gsds[51+j]), 
                   t=c('a', 'b', 'c',  'c', 'b', 'a'))
  dline=data.frame(x=c(1,2,3),
                   y=c(quickmark.gsums[1+j],quickmark.gsums[26+j],quickmark.gsums[51+j]))
  
  dpoly[dpoly[,2]<0,2]<-0
  
  for(g in 1:ng[1]){
    
    datpoints=data.frame(x=c(1,2,3),
                         y=as.numeric(quickmark.sums[g,c(1+j,26+j,51+j)]))
    
    q<-qplot(x=seq(1,files,1),y=rep(-100,files),xlim=c(1,files),ylim=c(min(quickmark.sums[,c(1+j,26+j,51+j)],na.rm=T)-0.5,0.5+max(quickmark.sums[,c(1+j,26+j,51+j)])),xlab="Assignment", ylab="Aantal keer gebruikt", main=paste("Groep",g))
    q<-q + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) +
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue") 
    
    nam=paste("q",g,sep="")
    print(nam)
    assign(nam,q)
  }
  
  pdf(file=paste("Quickmark",quickmarks.names[[1]][j],"pergroep.pdf"),width=13,height=12)
  multiplot(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
            q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,
            q21,q22,q23,q24,q25,q26,q27,q28,q29,q30,
            layout=matrix(c(1:30),5,6,byrow=T))
  dev.off() 
  
}







####################################
## MEAN RUBRIC SCORES PER STUDENT ##
####################################

for(i in 1:files){
  rdat<-rdata[[i]]
  
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  rdat[,rubrics.names[[i]]]<-sapply(rdat[,rubrics.names[[i]]],as.numeric)
  
  rubscore<-cbind(Schrijver=rdat$Schrijver,rdat[,rubrics.names[[i]]])

  rubscore %>%
    summarise_each(funs(mean(., na.rm=TRUE))) ->rub.gmean

  rubscore %>%
    summarise_each(funs(sd(., na.rm=TRUE))) ->rub.gsd
  
  if(i==1){
    rubgmeans<-rub.gmean[,-1]
    rubgsds<-rub.gsd[,-1]
  }
  if(i>1){
    rubgmeans<-rbind(rubgmeans,rub.gmean[,-1])
    rubgsds<-rbind(rubgsds,rub.gsd[,-1])
  }

}
### TOT HIER GEKOMEN VORIGE KEER ###
### GENERATE PLOTS ###

for(i in 1:dim(rubscore)[1])

  dpoly=data.frame(x=c(1:files, files:1), 
                   y=c(rubgmeans[1,j]+rubgsds[1,j],rubgmeans[2,j]+rubgsds[2,j],rubgmeans[3,j]+rubgsds[3,j],
                       rubgmeans[1,j]-rubgsds[1,j],rubgmeans[2,j]-rubgsds[2,j],rubgmeans[3,j]-rubgsds[3,j]), 
                   t=c('a', 'b', 'c',  'c', 'b', 'a'))
  dline=data.frame(x=c(1,2,3),
                   y=c(rubgmeans[1,j],rubgmeans[2,j],rubgmeans[3,j]))
  
  dpoly[dpoly[,2]<0,2]<-0
  
  for(g in 1:ng[1]){
    
    datpoints=data.frame(x=c(1,2,3),
                         y=as.numeric(rubmeans[g,c(1+j,7+j,13+j)]))
    
    q<-qplot(x=seq(1,files,1),y=rep(-100,files),xlim=c(1,files),ylim=c(min(rubmeans[,c(1+j,7+j,13+j)],na.rm=T)-0.5,0.5+max(rubmeans[,c(1+j,7+j,13+j)])),xlab="Assignment", ylab="Score",main=paste("Groep",g, rubrics.names[[1]][j]))
    q<-q + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue") 
    
    nam=paste("q",g,sep="")
    print(nam)
    assign(nam,q)
  }
  
  pdf(file=paste("Rubric",rubrics.names[[1]][j],"pergroep.pdf"),width=13,height=12)
  multiplot(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,
            q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,
            q21,q22,q23,q24,q25,q26,q27,q28,q29,q30,
            layout=matrix(c(1:30),5,6,byrow=T))
  dev.off() 
  
}






