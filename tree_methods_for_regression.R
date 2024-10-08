rm(list = ls())

library(ODRF)
library(rpart)
library(RLT);
library(evtree);
library(partykit);
source('./Datasets/RegrDataSets.R')
#####################################start######################################
print("start")

cnames = c()
#i.data = 10
dataR=seq(20)#floor(seq(1,20,length.out = 5))
for (i.data in dataR)
{
  data = Data.Set(i.data,wd = "./Datasets/Regression/")

  X0 =as.matrix(data$X0)
  y0 =c(data$y0)

  I = which((rowSums(is.na(X0))>0)|is.na(y0))#,which(rowSums(X==0)==p))
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }
  v = which(apply(X0, 2, sd)>1.0e-10)
  X0 = X0[,v]
  p = ncol(X0)
  N = length(y0)

  rm(data)
  #print(c(N,p))
  #sn = min(floor(N*1/3), 500)
  sn = min(floor(N*2/3), 2000)
  ico = 1

  E=matrix(0,100,10)
  for (ico in seq(100)){

    set.seed(ico)
    I = sample(1:N, N)
    XI = X0[I,]
    yI = y0[I]

    X = XI[1:sn,]
    y = yI[1:sn]

    SN = min(N, sn+5000)
    X1 = XI[(sn+1):SN,]
    y1 = yI[(sn+1):SN]

    rm(XI)
    rm(yI)

    v = which(apply(X, 2, sd)>1.0e-10)
    X = X[,v]
    X1 = X1[,v]

    p1 = ncol(X)
    name1 = paste("v", 1:p1, sep="")
    colnames(X) = name1
    colnames(X1) = name1

    #level=levels(y)
    t1=proc.time()
    A = ODT(X,y, split= "mse")
    pred <- predict(A,X1)
    e.ODT <- mean((pred-y1)^2)
    t.ODT=(proc.time()-t1)[3]

    t1=proc.time()
    A=rpart(y~ ., data=data.frame(X,y=y))
    pred=predict(A,data.frame(X1))  # factor
    e.CART <- mean((pred-y1)^2)
    t.CART=(proc.time()-t1)[3]

    t0=proc.time()
    A = RLT(X, y, model = "regression", ntrees = 1)
    pred <-predict(A, X1)$Prediction
    e.extraTree = mean((pred-y1)^2)
    t1=proc.time()
    t.extraTree=c(t1-t0)[3]

    #library(evtree)
    t0=proc.time()
    A <- evtree(y~., data=data.frame(X,y=y),control = evtree.control(ntrees = 10, niterations = 1000L))
    pred <-predict(A, data.frame(X1))
    e.evTree=mean((pred-y1)^2)
    t1=proc.time()
    t.evTree=c(t1-t0)[3]

    #library(partykit)
    t0=proc.time()
    A <- ctree(y~., data=data.frame(X,y=y))
    pred <-predict(A, data.frame(X1))
    e.cTree=mean((pred-y1)^2)
    t1=proc.time()
    t.cTree=c(t1-t0)[3]

    e.0 = mean((y1-mean(y))^2)
    e=c(e.CART,e.extraTree,e.evTree,e.cTree,e.ODT)/e.0
    t=c(t.CART,t.extraTree,t.evTree,t.cTree,t.ODT)

    E[ico,] = c(e, t)
  }

  ###################################end##########################################
  OUT =  matrix(c(i.data, N, p,sn,colMeans(E)), nrow=1)
  colnames(OUT) = c("i.data", "n","p","sn","e.CART","e.ERT","e.EVT","e.CT","e.ODT",
                    "t.CART","t.ERT","t.EVT","t.CT","t.ODT")
  print(OUT, 4.4)

  write.table(OUT, file="ODT_Regr_Real_Time.csv",sep = ",",append=TRUE,
              row.names = FALSE,col.names = is.null(cnames))
  rm(E)
  rm(X0)
  rm(y0)
  cnames=colnames(OUT)
}
