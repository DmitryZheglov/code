randomInit <- function(X, K) {
  if (K < 2) stop("In randomInit: K must not be less than 2")
  X[sample(1:nrow(X), K, replace = FALSE),]
}



assignClasses <- function(X, Mu = NULL, Sigma = NULL, type = "centroid", distFunc = "Euclid") {
  
  if (!(type %in% c("centroid", "nn"))) stop("In assignClasses: type must be 'centroid' or 'nn'")
  if (!(distFunc %in% c("Euclid", "Mahalanobis"))) stop("In assignClasses: distFunc must be 'Euclid' or 'Mahalanobis'")
  if (type == "centroid" & is.null(Mu)) stop("In assignClasses: Mu must not be NULL when type == 'centroid'")  
  if (distFunc == "Mahalanobis" & is.null(Sigma)) stop("In assignClasses: Sigma must not be NULL when distFunc == 'Mahalanobis'")  
  
  m <- nrow(X); n <- ncol(X); K <- nrow(Mu)
  
  if (type == "centroid") {
    if (distFunc == "Euclid") {
      dist <- matrix(nrow = m, ncol = K)
      for (k in 1:K) dist[,k] <- apply((X - matrix(rep(Mu[k,],m), nrow = m, byrow = TRUE)) ^ 2, 1, sum)
      return(apply(dist, 1, which.min))
    } else {
      dist <- matrix(nrow = m, ncol = K)
      for (k in 1:K) {
        S.inv <- try(solve(Sigma[[k]]), silent = TRUE)
        if (class(S.inv) == "try-error") S.inv <- diag(n)
        md <- function(x) rbind(x - Mu[k,]) %*% S.inv %*% cbind(x - Mu[k,])
        dist[,k] <- apply(X, 1, md)
      }
      return(apply(dist, 1, which.min))
    }
  } else {
    if (distFunc == "Euclid") {
      return(NULL)
    } else {
      return(NULL)
    }
  }
  
}



renumClasses <- function(class) {
  u <- sort(unique(class))
  L <- length(u) 
  new_class <- class
  if (!all(u == 1:L)) for (k in 1:L) new_class[class == u[k]] <- k
  new_class
}



clusterMeans <- function(X, class) {
  u <- unique(class); K <- length(u)
  n <- ncol(X)
  Mu <- matrix(nrow = K, ncol = n)
  for (k in 1:K) Mu[k,] <- apply(rbind(X[class == u[k],]), 2, mean)
  Mu
}



SumSq <- function(X, Mu, class, norm = function(x) sum(x^2)) {
  K <- nrow(Mu); m <- nrow(X)
  x.bar <- apply(X, 2, mean)
  TSS_func <- function(x) norm(x - x.bar)
  TSS <- sum(apply(X, 1, TSS_func)) 
  WSS <- BSS <- numeric()
  for (k in 1:K) {
    xk.bar <- apply(rbind(X[class == k,]), 2, mean)
    WSS_func <- function(x) norm(x - xk.bar)
    WSS[k] <- sum(apply(rbind(X[class == k,]), 1, WSS_func)) 
    BSS[k] <- sum(class == k) * norm(xk.bar - x.bar)
  }
  list(wss = WSS, bss = BSS, tss = TSS)
}



KMeans <- function(X, K, type = "centroid", distFunc = "Euclid", n_init = 10, max_iter = 100, delta = 0.001) {
  
  K <- round(K, 0); n_init <- round(n_init, 0)
  if (K < 2) stop("In KMeans: K must not be less than 2")
  if (n_init < 1) stop("In KMeans: n_init must not be less than 1")
  
  n <- ncol(X)
  
  class <- Mu <- wss <- bss <- iter <- vector("list", n_init)
  Sigma <- vector("list", K);  for (k in 1:K) Sigma[[k]] <- diag(n)
  
  for (i in 1:n_init) {
    
    iter[[i]] <- 0; K0 <- K
    Mu[[i]] <- randomInit(X, K)
    
    wss0 <- Inf; wss1 <- Inf
    decrease <- Inf
    
    while (decrease >= delta & iter[[i]] <= max_iter) {
      iter[[i]] <- iter[[i]] + 1
      class[[i]] <- assignClasses(X, Mu[[i]], Sigma = Sigma, type = type, distFunc = distFunc)
      class[[i]] <- renumClasses(class[[i]])
      Mu[[i]] <- clusterMeans(X, class[[i]])
      if (distFunc == "Mahalanobis") {
        K0 <- nrow(Mu[[i]])
        Sigma <- vector("list", K0)
        for (k in 1:K0) {
          obs <- (class[[i]] == k)
          if (sum(obs) == 1) Sigma[[k]] <- diag(n) else Sigma[[k]] <- cov(X[obs,])
        }
      }
      ss <- SumSq(X, Mu[[i]], class[[i]])
      wss0 <- wss1; wss1 <- sum(ss$wss)
      decrease <- ifelse(wss0 == Inf, Inf, (wss0 - wss1) / wss0)
    }
    
    wss[[i]] <- ss$wss/ss$tss; bss[[i]] <- ss$bss/ss$tss
    
  }
  
  best.init <- which.min(lapply(wss, sum))
  list(class = class[[best.init]], Mu = Mu[[best.init]], wss = wss[[best.init]], bss = bss[[best.init]], iter = iter[[best.init]])
  
}  


  
  
  
  
  
  
  