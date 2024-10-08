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

source('./Datasets/RegrDataSets.R')
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
dataR=seq(20)
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

  Err = foreach(co=seq(length(chunks)), .combine='rbind',.packages = depend_packages) %dopar%
    {
      parfun=function(ico){
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
        #####################################Axis-aligned########################
        A <- randomForest(X,y,ntree = 100)
        pred <- predict(A, X1)
        e.RF = mean((pred-y1)^2)

        A = regression_forest(X,y,num.trees=100) # grf1: regression forest
        pred = predict(A,X1)$predictions
        e.GRF =  mean((pred-y1)^2)

        # Regular ensemble trees (Extremely Randomized Trees, Geurts, et. al., 2006)
        A = RLT(X, y, model="regression", use.cores = 1, ntrees = 100,
                importance = FALSE, reinforcement = TRUE)
        pred=predict(A, X1)$Prediction
        e.RLT = mean((pred-y1)^2)

        e.Axis=c(e.RF,e.GRF,e.RLT)


        ###################################Oblique#####################################
        A = ODRF(X,y, split= "mse",NodeRotateFun = "RotMatRF",TreeRandRotate = TRUE,parallel = FALSE,ntrees = 100)
        pred <- predict(A,X1)
        e.RotRF <- mean((pred-y1)^2)

        A = ODRF(X,y, split= "mse",NodeRotateFun = "RotMatRand",parallel = FALSE,ntrees = 100)
        pred <- predict(A,X1)
        e.SPORF <- mean((pred-y1)^2)

        A = ODRF(X,y, split= "mse",parallel = FALSE,ntrees = 100)
        pred <- predict(A,X1)
        e.ODRF <- mean((pred-y1)^2)

        e.Oblique=c(e.RotRF,e.SPORF,e.ODRF)


        #############################Boosting###################################
        # extreme gradient boosting (xgboost)
        A = xgboost(data=X,label=y,verbose = 0,nrounds=100,objective="reg:squarederror")
        pred=predict(A,X1)
        e.XGB = mean((pred-y1)^2)

        # Train a boosted regression forest.
        A <- boosted_regression_forest(X, y, num.trees=100, sample.fraction=0.5, min.node.size=5, boost.max.steps=30)
        pred <- predict(A, X1)$predictions
        e.BRF = mean((pred-y1)^2)

        gbm1 <- gbm(y ~ ., data = data.frame(X,y=y),distribution = "gaussian", n.trees = 100, cv.folds = 5,
                    verbose = FALSE, n.cores = 1)
        best.iter <- gbm.perf(gbm1, method = "cv")
        pred <- predict(gbm1, newdata = data.frame(X1), n.trees = best.iter, type = "link")
        e.GBM = mean((pred-y1)^2)

        forest <- ODBT(X, y, X1, type = "reg",NodeRotateFun = "RotMatPPO",
                       parallel = FALSE,MinLeaf=ceiling(sqrt(length(y))),
                       model = c("ODT", "rpart", "rpart.cpp")[1],
                       replacement=TRUE,numOOB=0.37,max.terms = 30)
        pred <- forest$results$prediction
        e.ODBT <- mean((pred-y1)^2)

        e.Boosting=c(e.XGB,e.BRF,e.GBM,e.ODBT)
        ########################################################################


        e.0 = mean((y1-mean(y))^2)
        e=c(e.Axis,e.Oblique,e.Boosting)/e.0

        return(e)
      }


      E=matrix(1, nrow=length(chunks[[co]]), ncol=10)
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

  ##############################################################################
  ne=ncol(Err)
  err=which(rowSums(Err==1)==ne)
  if((length(err)>0)&(length(err)<nrow(Err))){
    Err=Err[-err,]
  }


  OUT =  matrix(c(i.data, N, p,sn,colMeans(Err)), nrow=1)
  e.Axis=c("e.RF","e.GRF","e.RLT")
  e.Oblique=c("e.RotRF","e.SPORF","e.ODRF")
  e.Boosting=c("e.XGB","e.BRF","e.GBM","e.ODBT")
  colnames(OUT) = c("i.data", "n","p","sn",e.Axis,e.Oblique,e.Boosting)
  print(OUT, 4.4)

  write.table(OUT, file="ODRF_Regr_Real.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))
  rm(Err)
  rm(X0)
  rm(y0)
  print(err)
  cnames=colnames(OUT)
}
