train_penGAM <-
function(X,y,pars = list(kCV = 3))
{
    # Meier, L., van de Geer, S. and Bühlmann, P. (2009). High-Dimensional Additive Modeling. Annals of Statistics 37, 3779-3821
    # code available from Lukas Meier [meier@stat.math.ethz.ch]
    # !! It is not so easy to find good values for cross-validation !!
    war <- library(penGAM, logical.return=TRUE)
    if(!war)
    {
        error("penGAM regression cannot be used because the package (based on Meier, L. et al. High-Dimensional Additive Modeling. 2009) cannot be loaded. Code is available from Lukas Meier [meier@stat.math.ethz.ch]")
    }
    if(is.matrix(X))
    {
        dimX <- dim(X)[1]
    } else if(is.vector(X))
    {
        dimX <- length(X)
        X <- as.matrix(X)
    }
    
    # Cross-Validation
    
    allSets <- getSetsForKFoldCV(dimX, pars$kCV)
    trainSets <- allSets$trainSets
    testSets <- allSets$testSets 
    
    lambda.pen <- 0.955^seq(1,120,by=30)
    #lambda.pen  <- 0.9^(1:20) ## 0.955^(1:110) 0.955^(1:110) ## Special!!!
    #lambda.pen  <- c(0.5,0.2,0.1) ## 0.955^(1:110) 0.955^(1:110) ## Special!!!
    #lambda.pen  <- 0.95 ## 0.955^(1:110) 0.955^(1:110) ## Special!!!
    #lambda.pen = c(1, 0.9, 0.8, 0.5, 0.3, 0.1),
    
    lambda.curv <- c(2^(4:(-4)), 0)
    #lambda.curv <- c(2^(6:(-6)), 0)
    #lambda.curv = c(6, 4, 2, 1), 
    
    SS <- matrix(0,length(lambda.pen), length(lambda.curv))
    for(i in 1:pars$kCV)
    {
        kkk <- round(sqrt(dimX))
        fit.GAM <- penGAM(as.matrix(X[trainSets[[i]],]), as.matrix(y[trainSets[[i]]]), 
                          lambda.pen,
                          lambda.curv,
                          knots = kkk, 
                          #########################
                          # is this a good idea??????
                          #########################
                          model = LinReg(), control = grpl.control(trace = 0))
        pred <- predict(fit.GAM, newdata = as.matrix(X[testSets[[i]],]))
        for(pp in 1:length(lambda.pen))
        {
            for(cc in 1:length(lambda.curv))
            {
                SS[pp,cc] <- SS[pp,cc] + sum((pred[pp,cc,] - y[testSets[[i]]])^2)
            }
        }
    }
    
    #choose best lambdas
    opt <- arrayInd(which.min(SS),dim(SS))
    lambda.pen.opt <- lambda.pen[opt[1]]
    lambda.curv.opt <- lambda.curv[opt[2]]
    cat("Optimal value for lambda.pen: ", lambda.pen.opt, "\n")
    cat("Optimal value for lambda.curv: ", lambda.curv.opt, "\n")
    
    
    # fit a gam model    
    fit.GAM <- penGAM(as.matrix(X), as.matrix(y), 
                      lambda.pen.opt,
                      lambda.curv.opt,
                      knots = round(sqrt(dim(X)[1])),
                      model = LinReg(), control = grpl.control(trace = 0))
    
    result <- list()
    result$Yfit <- predict(fit.GAM,X)
    result$residuals <- y - result$Yfit
    result$model <- fit.GAM
    return(result)
}
