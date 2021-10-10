# kfold, Data: data
# Col: colum # of the qualitative variable,usually the response
Fold <- function(kfold = 5,Data,Col,seed = 1234)
  { 
    n  <- nrow(Data);
    d  <- 1:n;
    dd <- list() 
    e  <- levels(Data[,Col]);   # levels of the qualitative variable
    T  <- length(e)             # the number of class of response 
    
    set.seed(seed)              # new seed defined by user
    for(i in 1:T)
      {
       d0      <- d[Data[,Col]==e[i]];
       j       <- length(d0)
       ZT      <- rep(1:kfold,ceiling(j/kfold))[1:j]
       id      <- cbind(sample(ZT,length(ZT)),d0);
       dd[[i]] <- id
       }
     # dd[[i]] is a set of the index of 1:Z and the i_th class
     mm <- list()
     for(i in 1:kfold)
       {
         u <- NULL;
         for(j in 1:T) 
            u <- c(u, dd[[j]][dd[[j]][,1]==i, 2])
         mm[[i]] <- u
       }  #mm[[i]]:the i_th set of index,i=1,...,Z
     return(mm) # return index set of Z
  }   # end of Fold