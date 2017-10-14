gp_regression <-
function(X,y, pars=list())
{
    options=gpOptions("ftc")
    options$kern$comp=list("rbf","white")
    #options$learnScales=TRUE
    model<-gpCreate(dim(X)[2],1,X,y,options)
    y2<-gpOut(model,X)
    model$Yfit<-y2
    model$residuals<-y-y2
    return(model)
}
