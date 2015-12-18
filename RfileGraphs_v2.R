# Remove all objects in memory
rm(list=ls())

'HEEFT U EEN EXTRA KOLOM TOEGEVOEGD MET CIJFERS DIE GECORRELEERD MOETEN WORDEN MET DE RUBRIC SCORES?'
'ZO JA, GEEF HIERONDER AAN HOEVEEL KOLOMMEN DAT ZIJN. LET OP: DEZE KOLOM MOET AAN ALLE INPUT BESTANDEN ZIJN TOEGEVOEGD' 

aantal=0


##################
## PREPARATIONS ##
##################

# Set working directory
if(Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Input_deel2")}
if(Sys.info()["sysname"]=="Darwin"){
  setwd("/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Input_deel2")}

# Install and load R packages
if("gdata" %in% rownames(installed.packages()) == FALSE) {install.packages("gdata")}
if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
library(gdata)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(xlsx)

# Zoek de excel bestanden in working directory (deze bestanden worden als input gebruikt voor de visualisaties)
xlsxfiles <- system("ls -U *.xlsx", intern=T)
files<-length(xlsxfiles)


##################
## Start script ##
##################

# Create dataframe for each merged excel file
for(i in 1:files){
  nam <- paste("file", i, sep = "")
  assign(nam, read.xls(xlsxfiles[i],pattern="Schrijver"))
}

# Combine these dataframes in a list
rdata=list()
for(j in 1:files){
  rdata[[j]]<-get(ls(pattern = "file")[j])
}

# Remove duplicates
for(j in 1:files){
 rdata[[j]]<-rdata[[j]][!duplicated(rdata[[j]][,-1]),]
}

# Create descriptive statistics
n=ng=rubrics=quickmarks=rep(0,files)
npg=rubrics.names=quickmarks.names=list()
for(i in 1:files){
  data<-rdata[[i]]
  n[i]<-length(unique(data$Schrijver))
  ng[i]<-length(unique(data$groep))
  npg[[i]]<-table(data$groep)
  rubrics[i]<-which(names(data)=="titel.y")-which(names(data)=="titel.x")-1
  quickmarks[i]<-length(names(data))-which(names(data)=="titel.y")
  rubrics.names[[i]]<-names(data[1:rubrics[i]+which(names(data)=="titel.x")])
  quickmarks.names[[i]]<-names(data[1:quickmarks[i]+which(names(data)=="titel.y")]) 
}


###########################
# MULTIPLE PLOTS FUNCTION #
###########################

### Multiple plot function ###


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
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


###################
## RUBRIC SCORES ##
###################

# Set working directory
if(Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Rubrics")}
if(Sys.info()["sysname"]=="Darwin"){
  setwd("/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Rubrics")}


# Herhaal deze plotfunctie voor het aantal input bestanden
for(i in 1:files){
    score<-rdata[[i]][,rubrics.names[[i]]]
    
    # Verwijder ongewenste datapunten
    for(c in 1:dim(score)[2]){
      score[,c] <- gsub("--", NA, score[,c])
      score[,c] <- gsub("<NA>", NA, score[,c])
      score[,c] <- gsub("\\*", NA, score[,c])
    }
    
    # Maak de frequentieplot voor iedere rubric
    rubplot1<-qplot(na.omit(score[,1]), xlab="Score",ylab="Frequency", main=rubrics.names[[i]][1])
    rubplot2<-qplot(na.omit(score[,2]), xlab="Score",ylab="Frequency", main=rubrics.names[[i]][2])
    rubplot3<-qplot(na.omit(score[,3]), xlab="Score",ylab="Frequency", main=rubrics.names[[i]][3])
    rubplot4<-qplot(na.omit(score[,4]), xlab="Score",ylab="Frequency", main=rubrics.names[[i]][4])
    rubplot5<-qplot(na.omit(score[,5]), xlab="Score",ylab="Frequency", main=rubrics.names[[i]][5])
    
    pdf(file=paste("Rubrics_assignment",i,".pdf",sep = ""),width=15,height=3)
        multiplot(rubplot1,rubplot2,rubplot3,rubplot4,rubplot5,cols=5)
    dev.off()
}


####################################################################################################
## CALCULATE MEAN RUBRIC SCORES PER GROUP ##
############################################

# Create datafile for each input file
for(i in 1:files){
  rdat<-rdata[[i]]
  
  # Remove unwanted datapoints
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  
  # Format data to numeric values
  rdat[,rubrics.names[[i]]]<-sapply(rdat[,rubrics.names[[i]]],as.numeric)
  
  # Create dataframe with groep ID and rubric scores
  rubscore<-cbind(groep=as.numeric(rdat$groep),rdat[,rubrics.names[[i]]])
  
  # Calculate mean rubric scores for each group
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(mean(., na.rm=TRUE))) ->rub.mean
    names(rub.mean)<-paste(names(rub.mean),i,sep = "")
  
  # For each rubric calculate the grand mean over groups
  rubscore %>%
    summarise_each(funs(mean(., na.rm=TRUE))) ->rub.gmean
  
  # Calculate rubric score variance for each group
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(sd(., na.rm=TRUE)))->rub.sd
  names(rub.sd)<-paste(names(rub.sd),i,sep = "")
  
  # Calculate grand rubric score variance 
  rubscore %>%
    summarise_each(funs(sd(., na.rm=TRUE))) ->rub.gsd
  
  rub.gsd<-(rub.gsd*2)/sqrt(ng[[i]])
  
  # Concatenate data if nr. of input datasets is larger than 1
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

#################################
### GENERATE PLOTS PER RUBRIC ###
#################################




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


######################################################################################################
### RUBRICSCORES PER GROUP ###
##############################

# Create separate file for each group
for(g in 1:ng[1]){

  rubricgrand=rubricerrorup=rubricerrorlow=rubricscores=minim=maxim=matrix(0,files,rubrics[1])
  
  # Loop the following code through each of the rubrics
  for(j in 1:5){
    
     # Calculate Error bars and grand mean rubric scores
    for(f in 1:files){
      rubricscores[f,j]<-as.numeric(rubmeans[g,6*f-5+j])
      rubricgrand[f,j]<-as.numeric(rubgmeans[f,j])
      rubricerrorup[f,j]=rubgmeans[f,j]+rubgsds[f,j]
      rubricerrorlow[f,j]=rubgmeans[f,j]-rubgsds[f,j]
      minim[f,j]=min(rubmeans[,6*f-5+j],na.rm=T)
      maxim[f,j]=max(rubmeans[,6*f-5+j],na.rm=T)
      }
  }  

  
  
## 1 ##  
  
    # Create dataframes 
    datpoints=data.frame(    # For particular group
          x=seq(1,files,1),
          y=rubricscores[,1])       
    dpoly=data.frame(        # For error bars
          x=c(seq(1,files,1), rev(seq(1,files,1))), 
          y=c(rubricerrorup[,1],rubricerrorlow[,1]),
          t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
    dline=data.frame(        # For grand means
          x=c(1,2,3),
          y=rubricgrand[,1])
    dpoly[dpoly[,2]<0,2]<-0

    # Create each rubric plot

     q1<-qplot(x=seq(1,files,1),
             y=rep(-100,files),
             xlim=c(1,files),
             ylim=c(min(minim[,1])-0.5,max(maxim[,1])+0.5),
             xlab="Assignment", 
             ylab="Score",
             main=paste("Groep",g, rubrics.names[[1]][1]))
    
    # Change plot scales
    q1<-q1 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
   

    ## 2 ##  
    
    # Create dataframes 
    datpoints=data.frame(    # For particular group
      x=seq(1,files,1),
      y=rubricscores[,2])       
    dpoly=data.frame(        # For error bars
      x=c(seq(1,files,1), rev(seq(1,files,1))), 
      y=c(rubricerrorup[,2],rubricerrorlow[,2]),
      t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
    dline=data.frame(        # For grand means
      x=c(1,2,3),
      y=rubricgrand[,2])
    dpoly[dpoly[,2]<0,2]<-0
    
    # Create each rubric plot
    
    q2<-qplot(x=seq(1,files,1),
              y=rep(-100,files),
              xlim=c(1,files),
              ylim=c(min(minim[,2])-0.5,max(maxim[,2])+0.5),
              xlab="Assignment", 
              ylab="Score",
              main=paste("Groep",g, rubrics.names[[1]][2]))
    
    # Change plot scales
    q2<-q2 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      # Add mean error bars
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      # Add grand mean points
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
      # Add background grid
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      # Add score of particular group as blue dot
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
    
    
    ## 3 ##  
    
    # Create dataframes 
    datpoints=data.frame(    # For particular group
      x=seq(1,files,1),
      y=rubricscores[,3])       
    dpoly=data.frame(        # For error bars
      x=c(seq(1,files,1), rev(seq(1,files,1))), 
      y=c(rubricerrorup[,3],rubricerrorlow[,3]),
      t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
    dline=data.frame(        # For grand means
      x=c(1,2,3),
      y=rubricgrand[,3])
    dpoly[dpoly[,2]<0,2]<-0
    
    # Create each rubric plot
    
    q3<-qplot(x=seq(1,files,1),
              y=rep(-100,files),
              xlim=c(1,files),
              ylim=c(min(minim[,3])-0.5,max(maxim[,3])+0.5),
              xlab="Assignment", 
              ylab="Score",
              main=paste("Groep",g, rubrics.names[[1]][3]))
    
    # Change plot scales
    q3<-q3 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      # Add mean error bars
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      # Add grand mean points
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
      # Add background grid
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      # Add score of particular group as blue dot
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
    
    
    ## 4 ##  
    
    # Create dataframes 
    datpoints=data.frame(    # For particular group
      x=seq(1,files,1),
      y=rubricscores[,4])       
    dpoly=data.frame(        # For error bars
      x=c(seq(1,files,1), rev(seq(1,files,1))), 
      y=c(rubricerrorup[,4],rubricerrorlow[,4]),
      t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
    dline=data.frame(        # For grand means
      x=c(1,2,3),
      y=rubricgrand[,4])
    dpoly[dpoly[,2]<0,2]<-0
    
    # Create each rubric plot
    
    q4<-qplot(x=seq(1,files,1),
              y=rep(-100,files),
              xlim=c(1,files),
              ylim=c(min(minim[,4])-0.5,max(maxim[,4])+0.5),
              xlab="Assignment", 
              ylab="Score",
              main=paste("Groep",g, rubrics.names[[1]][4]))
    
    # Change plot scales
    q4<-q4 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      # Add mean error bars
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      # Add grand mean points
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
      # Add background grid
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      # Add score of particular group as blue dot
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
    
    
    ## 5 ##  
    
    # Create dataframes 
    datpoints=data.frame(    # For particular group
      x=seq(1,files,1),
      y=rubricscores[,5])       
    dpoly=data.frame(        # For error bars
      x=c(seq(1,files,1), rev(seq(1,files,1))), 
      y=c(rubricerrorup[,5],rubricerrorlow[,5]),
      t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
    dline=data.frame(        # For grand means
      x=c(1,2,3),
      y=rubricgrand[,5])
    dpoly[dpoly[,2]<0,2]<-0
    
    # Create each rubric plot
    
    q5<-qplot(x=seq(1,files,1),
              y=rep(-100,files),
              xlim=c(1,files),
              ylim=c(min(minim[,5])-0.5,max(maxim[,5])+0.5),
              xlab="Assignment", 
              ylab="Score",
              main=paste("Groep",g, rubrics.names[[1]][5]))
    
    # Change plot scales
    q5<-q5 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
      # Add mean error bars
      geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
      # Add grand mean points
      geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
      # Add background grid
      theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
      # Add score of particular group as blue dot
      geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
    
    
    
  # Create PDF file for each group with the rubric plots
  pdf(file=paste("Rubric scores groep",g,".pdf"),width=12,height=3)
  multiplot(q1,q2,q3,q4,q5,
  layout=matrix(c(1:5),1,5,byrow=T))
  dev.off() 

}



#######################################
# Calculate Rubric score correlations #  
#######################################

for(i in 1:files){
  for(k in 1:dim(score)[1]){
    for(j in 1:dim(score)[2]){
      score[k,j]<-as.numeric(unlist(score[k,j]))
    }}
  score<-rdata[[i]][,rubrics.names[[i]]]
  extra<-right(rev(rdata[[i]]),aantal)
  score<-cbind(score,extra)
  score<-score[complete.cases(score),]
  score<-matrix(as.numeric(unlist(score)),dim(score)[1],dim(score)[2],byrow=F)
  rubcor<-cor(score)
  rownames(rubcor)<-colnames(rubcor)<-c(rubrics.names[[i]],names(right(rev(rdata[[i]]),aantal)))
  nam=paste("rubcor",i,sep="")
  assign(nam,rubcor)
  file <- paste(getwd(),"/Rubric_correlations",i, ".xlsx", sep="")
  write.xlsx(round(rubcor,2),file)
}




##########################################################################################################
## MEAN RUBRIC SCORES PER STUDENT ##
####################################

# Set working directory
if(Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Students")}
if(Sys.info()["sysname"]=="Darwin"){
  setwd("/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Students")}


rubscore=list()

for(i in 1:files){
  rdat<-rdata[[i]]
  
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  rdat[,rubrics.names[[i]]]<-sapply(rdat[,rubrics.names[[i]]],as.numeric)
  
  rubscore[[i]]<-cbind(Schrijver=rdat$Schrijver,rdat[,rubrics.names[[i]]])
  rubscore[[i]]<-rubscore[[i]][!duplicated(rubscore),]
  
  rubscore[[i]] %>%
    summarise_each(funs(mean(., na.rm=TRUE))) ->rub.gmean
  
  rubscore[[i]] %>%
    summarise_each(funs(sd(., na.rm=TRUE))) ->rub.gsd
  
 
  rubscore[[i]]<-rubscore[[i]][!duplicated(rubscore[[i]][,-1]),]
 
  if(i==1){
    rubgmeans<-rub.gmean[,-1]
    rubgsds<-rub.gsd[,-1]
    rubmerge<-rubscore[[1]]
  }
  if(i>1){
    rubgmeans<-rbind(rubgmeans,rub.gmean[,-1])
    rubgsds<-rbind(rubgsds,rub.gsd[,-1])
    rubmerge<-merge(rubmerge,rubscore[[i]], by="Schrijver", all.x=T)
  }
}


### GENERATE PLOTS ###

# Create separate file for each group
for(g in 1:dim(rubmerge)[1]){
  
  rubricgrand=rubricerrorup=rubricerrorlow=rubricscores=minim=maxim=matrix(0,files,rubrics[1])
  
  # Loop the following code through each of the rubrics
  for(j in 1:5){
    
    # Calculate Error bars and grand mean rubric scores
    for(f in 1:files){
      rubricscores[f,j]<-as.numeric(rubmerge[g,5*f-4+j])
      rubricgrand[f,j]<-as.numeric(rubgmeans[f,j])
      rubricerrorup[f,j]=rubgmeans[f,j]+((2*rubgsds[f,j])/sqrt(dim(rubmerge)[1]))
      rubricerrorlow[f,j]=rubgmeans[f,j]-((2*rubgsds[f,j])/sqrt(dim(rubmerge)[1]))
      minim[f,j]=min(rubmerge[,5*f-4+j],na.rm=T)
      maxim[f,j]=max(rubmerge[,5*f-4+j],na.rm=T)
    }
  }  
  
  
  
  ## 1 ##  
  
  # Create dataframes 
  datpoints=data.frame(    # For particular group
    x=seq(1,files,1),
    y=rubricscores[,1])       
  dpoly=data.frame(        # For error bars
    x=c(seq(1,files,1), rev(seq(1,files,1))), 
    y=c(rubricerrorup[,1],rubricerrorlow[,1]),
    t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
  dline=data.frame(        # For grand means
    x=c(1,2,3),
    y=rubricgrand[,1])
  dpoly[dpoly[,2]<0,2]<-0
  
  # Create each rubric plot
  
  q1<-qplot(x=seq(1,files,1),
            y=rep(-100,files),
            xlim=c(1,files),
            ylim=c(min(minim[,1])-0.5,max(maxim[,1])+0.5),
            xlab="Assignment", 
            ylab=paste("Score ",rubmerge[g,1]),
            main=paste("Rubric", rubrics.names[[1]][1]))
  
  # Change plot scales
  q1<-q1 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
  
  
  ## 2 ##  
  
  # Create dataframes 
  datpoints=data.frame(    # For particular group
    x=seq(1,files,1),
    y=rubricscores[,2])       
  dpoly=data.frame(        # For error bars
    x=c(seq(1,files,1), rev(seq(1,files,1))), 
    y=c(rubricerrorup[,2],rubricerrorlow[,2]),
    t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
  dline=data.frame(        # For grand means
    x=c(1,2,3),
    y=rubricgrand[,2])
  dpoly[dpoly[,2]<0,2]<-0
  
  # Create each rubric plot
  
  q2<-qplot(x=seq(1,files,1),
            y=rep(-100,files),
            xlim=c(1,files),
            ylim=c(min(minim[,2])-0.5,max(maxim[,2])+0.5),
            xlab="Assignment", 
            ylab=paste("Score ",rubmerge[g,1]),
            main=paste("Rubric", rubrics.names[[1]][2]))
  
  # Change plot scales
  q2<-q2 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
  
  
  ## 3 ##  
  
  # Create dataframes 
  datpoints=data.frame(    # For particular group
    x=seq(1,files,1),
    y=rubricscores[,3])       
  dpoly=data.frame(        # For error bars
    x=c(seq(1,files,1), rev(seq(1,files,1))), 
    y=c(rubricerrorup[,3],rubricerrorlow[,3]),
    t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
  dline=data.frame(        # For grand means
    x=c(1,2,3),
    y=rubricgrand[,3])
  dpoly[dpoly[,2]<0,2]<-0
  
  # Create each rubric plot
  
  q3<-qplot(x=seq(1,files,1),
            y=rep(-100,files),
            xlim=c(1,files),
            ylim=c(min(minim[,3])-0.5,max(maxim[,3])+0.5),
            xlab="Assignment", 
            ylab=paste("Score ",rubmerge[g,1]),
            main=paste("Rubric", rubrics.names[[1]][3]))
  
  # Change plot scales
  q3<-q3 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
  
  
  ## 4 ##  
  
  # Create dataframes 
  datpoints=data.frame(    # For particular group
    x=seq(1,files,1),
    y=rubricscores[,4])       
  dpoly=data.frame(        # For error bars
    x=c(seq(1,files,1), rev(seq(1,files,1))), 
    y=c(rubricerrorup[,4],rubricerrorlow[,4]),
    t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
  dline=data.frame(        # For grand means
    x=c(1,2,3),
    y=rubricgrand[,4])
  dpoly[dpoly[,2]<0,2]<-0
  
  # Create each rubric plot
  
  q4<-qplot(x=seq(1,files,1),
            y=rep(-100,files),
            xlim=c(1,files),
            ylim=c(min(minim[,4])-0.5,max(maxim[,4])+0.5),
            xlab="Assignment", 
            ylab=paste("Score ",rubmerge[g,1]),
            main=paste("Rubric", rubrics.names[[1]][4]))
  
  # Change plot scales
  q4<-q4 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
  
  
  ## 5 ##  
  
  # Create dataframes 
  datpoints=data.frame(    # For particular group
    x=seq(1,files,1),
    y=rubricscores[,5])       
  dpoly=data.frame(        # For error bars
    x=c(seq(1,files,1), rev(seq(1,files,1))), 
    y=c(rubricerrorup[,5],rubricerrorlow[,5]),
    t=c(letters[seq(1,files,1)], letters[rev(seq(1,files,1))]))
  dline=data.frame(        # For grand means
    x=c(1,2,3),
    y=rubricgrand[,5])
  dpoly[dpoly[,2]<0,2]<-0
  
  # Create each rubric plot
  
  q5<-qplot(x=seq(1,files,1),
            y=rep(-100,files),
            xlim=c(1,files),
            ylim=c(min(minim[,5])-0.5,max(maxim[,5])+0.5),
            xlab="Assignment", 
            ylab=paste("Score ",rubmerge[g,1]),
            main=paste("Rubric",rubrics.names[[1]][5]))
  
  # Change plot scales
  q5<-q5 + scale_x_continuous(breaks=seq(1,files,1), labels=seq(1,files,1)) +
    # Add mean error bars
    geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="black",size=.5) +
    # Add grand mean points
    geom_point(data=dline,mapping=aes(x=x, y=y),colour="black", shape=45,size=7) + 
    # Add background grid
    theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank()) + 
    # Add score of particular group as blue dot
    geom_point(data=datpoints,mapping=aes(x=x, y=y),size=4,colour="blue")
  
  
  
  # Create PDF file for each group with the rubric plots
  pdf(file=paste("Rubric scores ",rubmerge[g,1],g,".pdf"),width=12,height=3)
  multiplot(q1,q2,q3,q4,q5,
            layout=matrix(c(1:5),1,5,byrow=T))
  dev.off() 
  
}




################################################################################################
## QUICKMARK SCORES ##
######################

### Mean quickmark scores per group ###

# Set working directory
if(Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Quickmarks")}
if(Sys.info()["sysname"]=="Darwin"){
  setwd("/Users/paullodder/SURFdrive/ICT en Onderwijs/Turnitin/Turnitin R project/Output/Quickmarks")}


quickplot=function(i,g){
  
#for(i in 1:files){
  rdat<-rdata[[i]]
  
  for(c in 1:dim(rdat)[2]){
    rdat[,c] <- gsub("--", NA, rdat[,c])
    rdat[,c] <- gsub("<NA>", NA, rdat[,c])
    rdat[,c] <- gsub("\\*", NA, rdat[,c])
  }
  rdat[,quickmarks.names[[i]]]<-sapply(rdat[,quickmarks.names[[i]]],as.numeric)
  
  rubscore<-cbind(groep=as.numeric(rdat$groep),rdat[,quickmarks.names[[i]]])
 
  # Calculate sum of used quickmarks in each group
  rubscore %>%
    group_by(groep) %>%
    summarise_each(funs(sum(., na.rm=TRUE))) ->quickmark.sum
  names(quickmark.sum)<-paste(names(quickmark.sum),i,sep = "")
  
  # Calculate mean quickmark use over groups
  rubscore %>%
    summarise_each(funs(sum(., na.rm=TRUE))) ->quickmark.gsum
  
  # Calculate SD of quickmark use over groups
  rubscore %>%
    summarise_each(funs(sd(., na.rm=TRUE))) ->quickmark.gsd
  
  #for(g in 1:ng[i]){
    
    # Prepare plot data
    quickdat<-data.frame(quickmarks.names=quickmarks.names[[i]],
                         quickmarks.sums=as.numeric(as.data.frame(quickmark.sum[g,-1])),
                         quickmarks.gmean=as.numeric(quickmark.gsum)[-1]/ng[[i]],
                         quickmarks.errorup=(as.numeric(quickmark.gsum)[-1]/ng[[i]])+1.96*(as.numeric(quickmark.gsd)[-1]/sqrt(ng[[i]])),
                         quickmarks.errorlow=(as.numeric(quickmark.gsum)[-1]/ng[[i]])-1.96*(as.numeric(quickmark.gsd)[-1]/sqrt(ng[[i]])))
    dline=data.frame(x=seq(1,quickmarks[[i]],1),y=quickdat$quickmarks.gmean)
    derup=data.frame(x=seq(1,quickmarks[[i]],1),y=quickdat$quickmarks.errorup)
    derlow=data.frame(x=seq(1,quickmarks[[i]],1),y=quickdat$quickmarks.errorlow)
    dpoly=data.frame(x=c(seq(1,quickmarks[[i]],1),rev(seq(1,quickmarks[[i]],1))),
                     y=c(quickdat$quickmarks.errorup,rev(quickdat$quickmarks.errorlow)),
                     t=c(letters[seq(1,26,1)],LETTERS[seq(1,quickmarks[[i]]-26,1)],
                         LETTERS[rev(seq(1,quickmarks[[i]]-26,1))],letters[rev(seq(1,26,1))]))
  
    
    # Create plot
    q=qplot(x=quickdat$quickmarks.names,y=quickdat$quickmarks.sums,stat="identity",xlab="Quickmarks",ylab="Aantal keer gebruikt",main=paste("Aantal gebruikte quickmarks groep",g,"Assignment",i),geom = 'bar')
    q<-q + theme(axis.text.x=element_text(angle = -90, hjust = 0,size=15,color="black")) 
    q<-q + geom_point(data=dline,mapping=aes(x=x, y=y),colour="purple",size=3) 
    q<-q + geom_point(data=derup,mapping=aes(x=x, y=y),colour="purple",shape=45,size=10) 
    q<-q + geom_point(data=derlow,mapping=aes(x=x, y=y),colour="purple",shape=45,size=10) 
    q<-q + geom_line(data=dpoly,mapping=aes(x=x, y=y,g=t),colour="purple",size=.5) 
    q<-q + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) 
    q<-q + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
    print(q)

#}
#}
}

# Create plots for all files and all groups
for(i in 1:files){
  for(g in 1:ng[[i]]){
    q<-quickplot(i,g)
    nam=paste("q",g,"f",i,sep="")
    assign(nam,q)
  }
}

# Create pdf files of those plots
for(i in 1:files){
  for(g in 1:ng[[i]]){
    pdf(file=paste("Quickmarks_groep",g,"_file",i,".pdf"),width=13,height=12)
    print(get(paste("q",g,"f",i,sep="")))
    dev.off()
  }
}





