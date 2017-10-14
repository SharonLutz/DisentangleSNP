train_GAMboost <-
function(X,y,pars = list()) #
{
    ## begin old version
    # EXPLANATION: surprisingly, it turned out that this cannot be applied to large p (private discussion with T. Hothorn in Sep 2013)
    # yy <- y
    # dat <- data.frame(cbind(yy,X))
    # gb <- gamboost(yy ~ .,data=dat, baselearner = "bbs")
    ## end old version
    
    ## begin new version
    dat <- as.data.frame(X)
    bl <- lapply(dat, bbs)
    gb <- mboost_fit(bl, y)
    ## end new version
    
    result <- list()
    result$Yfit <- gb$fitted()
    result$residuals <- gb$resid()
    result$model <- gb
    return(result)
}
