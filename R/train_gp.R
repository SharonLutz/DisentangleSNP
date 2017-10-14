train_gp <-
function(X,y,pars = list())
{
    library(gptk)
    mod <- gp_regression(as.matrix(X),as.matrix(y))
    result <- list()
    result$Yfit = mod$Yfit
    result$residuals = mod$residuals
    result$model = mod
    return(result)
}
