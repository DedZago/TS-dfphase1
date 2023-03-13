library(devtools)
library(usethis)
library(vars)

load_all("dfphase1")

# data(Student)
# mphase1(Student) 
# VARmphase1.semip(Student) 

data(ryan)
mphase1(ryan, indep=TRUE) 
mphase1(ryan, indep=FALSE) 


data(gravel)
mphase1(gravel, indep=TRUE) 
mphase1(gravel, indep=FALSE) 


nsim = 1000
ncores = 16L
pacman::p_load("parallel")
simulate = function(nsim, seed=11223344, ar = NULL, ma = NULL){
    pval.base = rep(NA, nsim)
    pval.vars = rep(NA, nsim)
    sims = mclapply(1:nsim, function(i) singleSim(i, seed, ar, ma), mc.cores=ncores)
    out = do.call("rbind", sims)
    colnames(out) = c("mphase1", "TSmphase1")
    out
}

singleSim = function(idx, seed, ar = NULL, ma = NULL){
    set.seed(seed + idx)
    m <- 50
    n <- 3
    p <- 4
    df <- 3
    Sigma <- outer(1:p,1:p,function(i,j) 0.8^abs(i-j))
    Sigma
    xnorm <- crossprod(chol(Sigma),matrix(rnorm(p*n*m),p))
    xchisq <- sqrt(rchisq(n*m,df)/(df-2))
    xt = sweep(xnorm,2,xchisq,"/")
    # xt = xnorm

    xt.ar = xt
    for(i in 1:NROW(xt)){
        xt.ar[i, ] = arima.sim(list(ar = ar, ma = ma), n = NCOL(xt), innov=xt[i, ])
    }

    # acf(t(xt))
    # acf(t(xt.ar))
    xx <- array(xt.ar,c(p,n,m))
    # Then, we add an isolated shift at time 10
    # (only for the first variable)
    xx[1,,10] <- xx[1,,10]+3.0

    # # and, a step shift starting at time 31
    # # (only for the third and fourth variable)
    xx[3:4,,31:50] <- xx[3:4,,31:50] + c(1.0,-0.75)

    # c(mphase1(x, post.signal=FALSE)$p.value, mphase1(x, post.signal=FALSE, indep=FALSE)$p.value)
    list("mphase1" = mphase1(xx, post.signal=TRUE, indep=TRUE, plot=FALSE),
        "TSmphase1" = mphase1(xx, post.signal=TRUE, indep=FALSE, plot=FALSE))
    # c(mphase1(xx, post.signal=FALSE, indep=TRUE)$p.value,
    #      mphase1(xx, post.signal=FALSE, indep=FALSE)$p.value)
}

out = singleSim(1, 12345, ar = NULL, ma = c(0.6, -1.0, 0.9))
# plot(out$mphase1)
# plot(out$TSmphase1)
sim = simulate(nsim, sample(1:1000000, 1), ar = NULL, ma = c(0.6, -0.1, 0.9))
    
mean(sim[, 1] <= 0.05)
mean(sim[, 2] <= 0.05)

# pv = matrix(0, nsim, 2)
# for(i in 1:nsim){
#     pv[i,1] = sim[i,1][[1]]$p.value
#     pv[i,2] = sim[i,2][[1]]$p.value
# }
