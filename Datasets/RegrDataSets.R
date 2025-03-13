Data.Set = function(i.data,wd=getwd())
{
  #wd0=getwd()
  #setwd(wd)

  if(i.data==1){#6
    XY=read.csv(paste0(wd,"/35-servo.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==2){#6
    XY=read.csv(paste0(wd,"/38-strike.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==3){#6
    XY=read.csv(paste0(wd,"/3-autoMpg.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==4){#6
    XY=read.csv(paste0(wd,"/23-lowbwt.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==5){#6
    XY=read.csv(paste0(wd,"/28-pharynx.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==6){#6
    XY=read.csv(paste0(wd,"/6-bodyfat.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==7){
    data=as.matrix(read.csv(paste0(wd,"/ParisHousing.csv")))
    X0 = data[,1:16]
    y0 = data[,17] #N=10000,p=16
  }
  if(i.data==8){
    data = read.csv(paste0(wd,"/parkinsons.csv"))
    p = ncol(data)
    data = as.matrix(data[,2:p])
    y0 = data[,5]
    X0 = data[,-c(4, 5)]
    N = nrow(data)
    p = ncol(data)
    #  X0 = as.matrix(data[,1:p-1]);
    #  y0 = as.matrix(data[,p]);
    I = (apply(X0, 2, sd)>0)
    X0 = X0[,I]
    p = ncol(X0) #N=5875,p=19
  }
  if(i.data==9){#6
    XY=read.csv(paste0(wd,"/1-auto93.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==10){#6
    XY=read.csv(paste0(wd,"/2-autoHorse.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  if(i.data==11){
    XY = as.matrix(read.csv(paste0(wd,'/Adelaide_Data.csv')))
    X0 = XY[,1:32]
    y0 = XY[, 49]
    N = length(y0)
    p = ncol(X0) #N=71998,p=32
  }
  if(i.data==12){
    XY = read.csv(paste0(wd,"/Baseball_player_statistics/Baseball_player_statistics.csv"))
    y0 = XY[,12]/XY[,10]
    X0 = as.matrix(XY[,-c(1,3:7,10,12)])
    p = ncol(X0)
    N = length(y0) #p=74,N=4535
  }
  if(i.data==13){
    XY = as.matrix(read.table(paste0(wd,"/YearPredictionMSD.txt")))
    p = ncol(XY)-1
    N = nrow(XY)
    X0 = XY[,1:p]
    y0 = XY[,p+1] #N=50000,p=90
  }
  if(i.data==14){
    XY = as.matrix(read.csv(paste0(wd,'/Residential_Building_Data_Set.csv'),header=F))
    X0 = XY[,2:103]
    y = XY[, 104] # sales price
    N = length(y)
    p = ncol(X0)
    y0 = log(y) #N=372,p=102
  }
  if(i.data==15){
    XY = as.matrix(read.csv(paste0(wd,"/Residential_Building_Data_Set.csv"),header=F))
    X0 = XY[,2:103]
    y = XY[, 105] # construction cost
    N = length(y)
    p = ncol(X0)
    y0 = log(y) #N=372,p=102
  }
  if(i.data==16){
    XY = as.matrix(read.table(paste0(wd,"/GeographicalOriginalofMusic.txt")))
    X0= XY[,1:116]
    y0= XY[,117] # latitude
    p=ncol(X0)
    N = length(y0) #N=1059,p=116
  }
  if(i.data==17){
    data = read.csv(paste0(wd,"/blogData_train.csv"),header=F)
    X0 = as.matrix(data[,1:280])
    y0 = as.numeric(data[,281])
    I = which(y0>1)
    X0 = X0[I,]
    y0 = sqrt(log(y0[I]))
    N = nrow(X0)
    p = ncol(X0) #N=13063,p=280
  }
  if(i.data==18){
    data = read.csv(paste0(wd,"/CreditScore_train.csv"))
    N = nrow(data)
    p = ncol(data)
    I = c()
    for (i in 1:p)
    {
      if (mean(is.na(data[,i]))>0)
        I = c(I, i)
    }
    data = data[,-I]
    N = nrow(data)
    p = ncol(data)-1
    X0 = as.matrix(data[,1:p])
    y0 = log(as.matrix(data[,p+1])) #N=80000,p=263
  }
  if(i.data==19){
    XY = read.csv(paste0(wd,'/slice_localization_data.csv'))
    X0 = as.matrix(XY[,1:385])
    y0 = XY[,386]
    p = ncol(X0)
    N = length(y0)
    I = c()
    for (i in 1:p)  I = c(I, length(unique(X0[,i])))
    v = which(I >= 2)
    X0 = X0[,v] #N=53500,p=380
  }
  if(i.data==20){
    XY = as.matrix(read.csv(paste0(wd,"/UJIndoor.csv")))
    aa = c()
    for(i in 1:ncol(XY)){
      aa = c(aa,length(unique(XY[,i])))
    }
    XY = XY[,aa>1]
    X0 = XY[,1:465]
    y0 = XY[,466] # Longitude
    p = ncol(X0)
    N = length(y0) #N=19937,p=465
  }
  ##########################################
  #setwd(wd0)

  n = dim(X0)[1]
  p = dim(X0)[2]

  #I = union(c(which(rowSums(is.na(X0))>0),which(rowSums(X0==0)==p)),which(is.na(y0)))
  I = which((rowSums(is.na(X0))>0)|is.na(y0))#,which(rowSums(X==0)==p))
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }

  return(list(X0 = X0, y0=y0))
}

