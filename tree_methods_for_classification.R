rm(list = ls())

library(ODRF)
library(rpart)
library(RLT);
library(evtree);
library(partykit);
source('./Datasets/ClassDataSets.R')
#####################################start######################################
print("start")

cnames = c()
#i.data = 10
dataC=seq(20)#floor(seq(1,20,length.out = 5))
for (i.data in dataC)
{
  data = Data.Set(i.data,wd = "./Datasets/Classification/")
  X0 =as.matrix(data$X0)
  y0 =as.factor(data$y0)
  I = which((rowSums(is.na(X0))>0)|is.na(y0))#,which(rowSums(X==0)==p))
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }
  catMap=NULL
  v = which(apply(X0, 2, sd)>1.0e-10)
  X0 = X0[,v]
  p = ncol(X0)
  N = length(y0)

  rm(data)
  #print(c(N,p))
  #sn = min(floor(N*1/3), 500)
  sn = min(floor(N*2/3), 2000)

  num.class <- length(levels(y0))
  classCt <- table(y0)
  Cindex <- vector("list", num.class)
  names(Cindex)=levels(y0)
  for (m in 1:num.class) {
    Cindex[[m]] <- which(y0 ==levels(y0)[m])
  }

  ico = 1
  E=matrix(0,100,10)
  for (ico in seq(100)){

    set.seed(ico)
    I=c()
    for (m in 1:num.class) {
      I <-c(I,sample(Cindex[[m]],ceiling(sn*classCt[m]/N)))
    }

    X = X0[I,]
    y = y0[I]

    I1=setdiff(1:N,I)[1:min(N-length(I),5000)]
    X1 = X0[I1,]
    y1 = y0[I1]


    v = which(apply(X, 2, sd)>1.0e-10)
    X = X[,v]
    X1 = X1[,v]

    p1 = ncol(X)
    name1 = paste("v", 1:p1, sep="")
    colnames(X) = name1
    colnames(X1) = name1

    level=levels(y)
    t1=proc.time()
    A = ODT(X,y, split= "gini")
    pred <- predict(A,X1)
    e.ODT <- mean(pred!=y1)
    t.ODT=(proc.time()-t1)[3]

    t1=proc.time()
    A=rpart(y~ ., method="class", data=data.frame(X,y=y))
    pred=predict(A,data.frame(X1), type = "class")  # factor
    e.CART <- mean(pred!=y1)
    t.CART=(proc.time()-t1)[3]

    t0=proc.time()
    A = RLT(X, y, model="classification", ntrees = 1)
    pred <-predict(A, X1)$Prediction
    e.extraTree = mean(pred!=y1)
    t1=proc.time()
    t.extraTree=c(t1-t0)[3]

    #library(evtree)
    t0=proc.time()
    A <- evtree(y~., data=data.frame(X,y=y),control = evtree.control(ntrees = 10, niterations = 1000L))
    pred <-predict(A, data.frame(X1))
    e.evTree=mean(pred != y1)
    t1=proc.time()
    t.evTree=c(t1-t0)[3]

    #library(partykit)
    t0=proc.time()
    A <- ctree(y~., data=data.frame(X,y=y))
    pred <-predict(A, data.frame(X1))
    e.cTree=mean(pred != y1)
    t1=proc.time()
    t.cTree=c(t1-t0)[3]

    e=c(e.CART,e.extraTree,e.evTree,e.cTree,e.ODT)
    t=c(t.CART,t.extraTree,t.evTree,t.cTree,t.ODT)

    E[ico,] = c(e, t)
  }

  ###################################end##########################################
  OUT =  matrix(c(i.data, N, p,sn,colMeans(E)), nrow=1)
  colnames(OUT) = c("i.data", "n","p","sn","e.CART","e.ERT","e.EVT","e.CT","e.ODT",
                    "t.CART","t.ERT","t.EVT","t.CT","t.ODT")
  print(OUT, 4.4)

  write.table(OUT, file="ODT_Class_Real_Time.csv",sep = ",",append=TRUE,
              row.names = FALSE,col.names = is.null(cnames))
  rm(E)
  rm(X0)
  rm(y0)
  cnames=colnames(OUT)
}
