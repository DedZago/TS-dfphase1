VARmphase1.semip <- function(x, lag.max = 5, ...){
    #? Run mphase1 on time-series residuals, estimated on average of the second dimension
    d = dim(x)
    if (length(d)==2) dim(x) <- c(d[1],1,d[2])
    p = d[1]; n = d[2]; m = d[3];

    # Convert array to averaged dataframe
    dat = matrix(0, nrow = m, ncol = p)
    for(j in 1:n){
      for(i in 1:m){
        dat[i, ] = dat[i, ] + x[,j,i]
      }
    }
    dat = dat / n
    model = suppressWarnings(VAR(dat, lag.max = 5, ic = "SC"))
    p = model$p
    yhat = t(fitted(model))

    #? Remove observations from 1 to p, to get predictions
    newdat = x[,,-(1:p)]
    for(k in 1:(m-p)){
        for(i in 1:n){
            newdat[,i,k] = newdat[,i,k] - yhat[, k]           
        }
    }
    mphase1(newdat, ...)
}
