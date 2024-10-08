rm(list = ls())

library(foreach)
library(doParallel)
library(randomForest);
library(xgboost);
library(grf);
library(RLT);
library(ODRF)
library(obliqueRF);
library(PPforest);
library(gbm)
library(rotationForest);
library(rerf)

source('./Datasets/ClassDataSets.R')
depend_packages=c('randomForest',"grf","xgboost","RLT","obliqueRF","PPforest",
                  "ODRF","gbm","rotationForest","rerf")

cores=25;Rep=100
cores <-min(c(detectCores(logical = FALSE),cores))
cl <- makeCluster(cores)

if(Rep<=cores){
  chunks <- as.list(seq(Rep))
}else{
  chunks <- clusterSplit(cl,seq(Rep))
}
#####################################start######################################
print("start")
print(cores)
registerDoParallel(cl, cores=cores)

cnames = c()
#i.data = 9
dataC=seq(20)
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
  Err = foreach(co=seq(length(chunks)), .combine='rbind',.packages = depend_packages) %dopar%
    {
      parfun=function(ico){
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
        #####################################Axis-aligned#######################
        A <- randomForest(X,as.factor(y),ntree = 100)
        pred <- predict(A, X1)
        pred=as.character(pred)
        e.RF =mean(pred != y1)

        A = probability_forest(X, y,num.trees=100)
        pred = predict(A,X1)$predictions
        pred =colnames(pred)[apply(pred, 1, which.max)]
        e.GRF = mean(pred != y1)

        # Regular ensemble trees (Extremely Randomized Trees, Geurts, et. al., 2006)
        A = RLT(X, y, model = "classification", use.cores = 1, ntrees = 100,
                importance = FALSE, reinforcement = FALSE)
        pred=predict(A, X1)$Prediction
        e.RLT = mean(pred != y1)

        e.Axis=c(e.RF,e.GRF,e.RLT)


        ###################################Oblique#####################################
        A <- try(rotationForest(data.frame(X),y,L=100),silent = TRUE)
        if(inherits(A, "try-error")){
          A = ODRF(X,y, split= "gini",NodeRotateFun = "RotMatRF",TreeRandRotate = TRUE,parallel = FALSE,ntrees = 100)
          pred <- predict(A,X1)
          e.RotRF <- mean(pred != y1)
        }else{
          pred=predict(A,data.frame(X1))
          pred=(pred>0.5)+0
          e.RotRF <- mean(pred != y1)
        }

        A <- try(RerF(X, y,num.cores = 1L,FUN = RandMatBinary,trees=100),silent = TRUE)
        if(inherits(A, "try-error")){
          A = ODRF(X,y, split= "gini",NodeRotateFun = "RotMatRand",parallel = FALSE,ntrees = 100)
          pred <- predict(A,X1)
          e.SPORF <- mean(pred != y1)
        }else{
          pred <- Predict(X1,A, num.cores = 1L)
          e.SPORF <- mean(pred != y1)
        }

        A <- try(PPforest(data = data.frame(X,y=y), class = 'y',std = F,size.tr=1, m=100,size.p = 1/3,PPmethod="LDA"),silent = TRUE)
        if(inherits(A, "try-error")){
          A = ODRF(X,y, split= "entropy",paramList = list(model="LDA",dimProj="Rand"),parallel = FALSE,ntrees = 100)
          pred <- predict(A,X1)
          e.PPF <- mean(pred!=y1)
        }else{
          pred=trees_pred(A, xnew = data.frame(X1))$predforest
          pred=level[pred]
          e.PPF = mean(pred!=y1)
        }

        A <- obliqueRF(X,as.numeric(y),verbose=F, ntree=100)
        pred <- predict(A, X1)
        e.ORF <- mean(pred != as.numeric(y1))

        A = ODRF(X,y, split= "gini",parallel = FALSE,ntrees = 100)
        pred <- predict(A,X1)
        e.ODRF <- mean(pred != y1)

        e.Oblique=c(e.RotRF,e.SPORF,e.PPF,e.ORF,e.ODRF)


        #############################Boosting###################################
        # extreme gradient boosting (xgboost)
        if(length(level)>2){
          A = xgboost(data=X,label=as.numeric(y)-1,verbose = 0,nrounds=100,objective="multi:softmax",num_class=length(level))#,print_every_n= 0,save_period=1)
        }else{
          A = xgboost(data=X,label=as.numeric(y)-1,verbose = 0,nrounds=100,objective="binary:hinge")#,print_every_n= 0,save_period=1)
        }
        pred=predict(A, X1)
        e.XGB =mean(pred != as.numeric(y1)-1)

        # Train a boosted regression forest.
        yy=as.integer(y)-1
        yy=(yy>0.5)+0
        yy1=as.integer(y1)-1
        yy1=(yy1>0.5)+0
        A <- boosted_regression_forest(X, yy, num.trees=100, sample.fraction=0.5, min.node.size=5, boost.max.steps=100)
        pred <- predict(A, X1)
        pred=(pred>0.5)+0
        e.BRF = mean(pred != yy1)

        gbm1 <- gbm(y ~ ., data = data.frame(X,y=as.numeric(y)-1),
                    distribution =ifelse(nlevels(y)==2,"bernoulli","gaussian"), n.trees = 100, cv.folds = 5,
                    verbose = FALSE, n.cores = 1)
        best.iter <- gbm.perf(gbm1, method = "cv")
        pred <- predict(gbm1, newdata = data.frame(X1), n.trees = best.iter, type = "link")
        pred=(pred>0.5)+0
        e.GBM = mean(pred != y1)

        forest <- ODBT(X, y, X1, type = "class",NodeRotateFun = "RotMatPPO",
                       parallel = FALSE,MinLeaf=ceiling(sqrt(length(y))),
                       model = c("ODT", "rpart", "rpart.cpp")[1],
                       replacement=TRUE,numOOB=0.37,max.terms = 30)
        pred <- forest$results$prediction
        e.ODBT <- mean(pred != y1)


        e.Boosting=c(e.XGB,e.BRF,e.GBM,e.ODBT)
        ################################end#####################################


        #e.0 = mean((y1-mean(y))^2)
        e=c(e.Axis,e.Oblique,e.Boosting)
        return(e)
      }


      E=matrix(1, nrow=length(chunks[[co]]), ncol=12)
      for(ico in chunks[[co]]) {
        e = try(parfun(ico), silent=TRUE)
        if ('try-error' %in% class(e)){
          next
        }
        E[ico-(chunks[[co]][1]-1),] = e
      }

      # return local results
      E
      #Err[ico,] = e
    }

  #################################results######################################
  ne=ncol(Err)
  err=which(rowSums(Err==1)==ne)
  if((length(err)>0)&(length(err)<nrow(Err))){
    Err=Err[-err,]
  }


  OUT =  matrix(c(i.data, N, p,sn,colMeans(Err)), nrow=1)
  e.Axis=c("e.RF","e.GRF","e.RLT")
  e.Oblique=c("e.RotRF","e.SPORF","e.PPF","e.ORF","e.ODRF")
  e.Boosting=c("e.XGB","e.BRF","e.GBM","e.ODBT")
  colnames(OUT) = c("i.data", "n","p","sn",e.Axis,e.Oblique,e.Boosting)
  print(OUT, 4.4)

  write.table(OUT, file="ODRF_Class_Real.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  rm(Err)
  rm(X0)
  rm(y0)
  print(err)
  cnames=colnames(OUT)
}
