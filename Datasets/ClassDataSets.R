Data.Set = function(i.data,wd=getwd())
{
  #wd=paste0(wd,"/DataSets/Classification1/")

  if(i.data==1){
    yx=read.table(paste0(wd,"data16.txt"),sep=",")
    p=ncol(yx)-1
    n=nrow(yx) #n=19020,p=10
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==2){
    yx = as.matrix(read.table(paste0(wd,"data1.txt"),sep=","))
    p = ncol(yx)-1
    n = nrow(yx) #n=14980,p=14
    X0 = yx[,1:p]
    y0 = yx[,p+1]
  }
  if(i.data==3){
    yx=read.table(paste0(wd,"data14.txt"),sep=",")
    p=ncol(yx)-1
    n=nrow(yx) #n=1151,p=19
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  #26
  if(i.data ==4){
    yx=read.csv(paste0(wd,"data26.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==5){
    yx=read.csv(paste0(wd,"data9/Pistachio_28_Features_Dataset.csv"))
    p=ncol(yx)-1
    n=nrow(yx) #n=2148,p=28
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==6){
    yx=read.csv(paste0(wd,"data7.csv"))
    y0=as.integer(as.factor(yx[,2]))
    y0[y0==2]=0
    X0=as.matrix(yx[,3:32])
    n=length(y0)
    p=ncol(X0) #n=569,p=30
  }
  if(i.data==7){#6
    XY=read.csv(paste0(wd,"23-waveform.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==8){
    yx=read.csv(paste0(wd,"data13.csv"),header=F)
    p=ncol(yx)-1
    n=nrow(yx) #n=1055,p=41
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  if(i.data ==9){
    yx=read.csv(paste0(wd,"data27.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==10){
    yx=read.table(paste0(wd,"data2.csv"),header=TRUE,sep=",")[,-c(1,80:82)]
    del=order(as.vector(apply(is.na(yx[,1:77]),2,sum)),decreasing=TRUE)[1:6]
    yx=yx[,-del]
    yx=yx[apply(is.na(yx),1,sum)==0,]
    yx=yx[,-69] #69th column of data set 2 is the same as 54th column
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=1047,p=70
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  if(i.data ==11){
    yx=read.csv(paste0(wd,"data22.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==12){
    yx=read.csv(paste0(wd,"data8.csv"))
    y0=yx[,1]
    X0=as.matrix(yx[,2:96])
    n=length(y0)
    p=ncol(X0) #n=6819,p=95
  }
  if(i.data==13){
    yx1=read.table(paste0(wd,"data3/Hill_Valley_without_noise_Testing.txt"),header=TRUE,sep=",")
    yx2=read.table(paste0(wd,"data3/Hill_Valley_without_noise_Training.txt"),header=TRUE,sep=",")
    yx=rbind(yx1,yx2)
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=1212,p=100
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  if(i.data==14){#5
    XY=read.csv(paste0(wd,"6-hillValleyNoisy.csv"))#5-hillValley.csv
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==15){
    yx=read.table(paste0(wd,"data4/clean2.txt"),header=FALSE,sep=",")[,-(1:2)]
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=6598,p=166
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  if(i.data==16){
    yx1=as.matrix(read.csv(paste0(wd,"data11/ptbdb_normal.csv")))
    yx2=as.matrix(read.csv(paste0(wd,"data11/ptbdb_abnormal.csv")))
    yx=rbind(yx1,yx2)
    yx=yx[,-187]
    p=ncol(yx)-1
    n=nrow(yx) #n=14550,p=186
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }

  if(i.data==17){
    yx=read.csv(paste0(wd,"data20.csv"))[,-1]
    p=ncol(yx)-1
    n=nrow(yx) #n=420,p=192
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==18){
    om<-function(a) return(sum(is.na(a)))
    yx=read.csv(paste0(wd,"data10/2018_Financial_Data.csv"))
    yx=yx[apply(yx,1,om)<=3,]
    yx=yx[,apply(yx,2,om)==0]
    yx=yx[,-c(1,218)]
    p=ncol(yx)-1
    n=nrow(yx) #n=986,p=216
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  if(i.data==19){
    y=read.table(paste0(wd,"data15/madelon_train.labels"))
    x=read.table(paste0(wd,"data15/madelon_train.data"))
    yx=cbind(x,y)
    n=nrow(yx)
    p=ncol(yx)-1 #n=2000,p=500
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  if(i.data==20){
    y=read.table(paste0(wd,"data5/train/y_train.txt"),header=FALSE,sep="")
    x=read.table(paste0(wd,"data5/train/X_train.txt"),header=FALSE,sep=" ")[,-1]
    yx=cbind(x,y)
    yx=yx[(yx[,562]==1)|(yx[,562]==6),] #walking 1 laying 6
    yx[yx[,562]==1,562]=0
    yx[yx[,562]==6,562]=1
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=2633,p=561
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }

  n = dim(X0)[1]
  p = dim(X0)[2]

  I = which((rowSums(is.na(X0))>0)|is.na(y0))
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }

  return(list(X0 = X0, y0=y0))
}

