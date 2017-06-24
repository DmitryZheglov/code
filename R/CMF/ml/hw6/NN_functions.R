gd <- function(fn, gr, par, alpha = 0.1, delta = 10^-6, max_epoch = 10^3) {
  g <- gr(par); epoch <- 0
  while (norm(g) >= delta & epoch < max_epoch) {
    epoch <- epoch + 1
    par <- par - alpha * g
    g <- gr(par)
  }
  mess <- NULL
  if (norm(g) < delta) {
    mess <- "converged"
  } else if (epoch == max_epoch) {
    mess <- "max_epoch is reached"
  }
  list(par = par, value = fn(par), n_epoch = epoch, message = mess)
}


sgd <- function(X, y, fn, gr, par, alpha = 0.1, delta = 10^-6, b_size = 100, max_epoch = 1) {
  epoch <- 0; m <- nrow(X)
  while (epoch < max_epoch) {
    epoch <- epoch + 1
    p <- 0
    while (p + b_size <= m) {
      Xb <- cbind(X[(p+1):(p+b_size),]); yb <- cbind(y[(p+1):(p+b_size),])
      g <- gr(Xb, yb, par)
      par <- par - alpha * g
      p <- p + b_size
    }
  }
  list(par = par, value = fn(X, y, par), n_epoch = epoch, grad = g)
}


InitPar <- function(model, eps = 10^-3) {
  L <- unlist(lapply(model, length))
  if (length(L) != 2) stop("In InitPar: invalid model")
  if (L[1] != L[2] + 1) stop("In InitPar: number of layers and number of activations don't match")
  if (L[2] < 2) stop("network must have at least 2 layers")
  Theta <- vector("list", L[2])
  for (i in 1:L[2]) {
    Theta[[i]] <- matrix(runif(n=(model$layer[i]+(i==1))*model$layer[i+1],-eps,eps), nrow = model$layer[i] + (i==1), ncol = model$layer[i+1])
  }
  Theta
}


vec <- function(Theta) {
  theta <- numeric()
  for (i in 1:length(Theta)) theta <- c(theta, as.vector(Theta[[i]]))
  theta
}


unvec <- function(theta, model) {
  
  L <- unlist(lapply(model, length))
  if (length(L) != 2) stop("In unvec: invalid model")
  if (L[1] != L[2] + 1) stop("In unvec: number of layers and number of activations don't match")
  if (L[2] < 2) stop("network must have at least 2 layers")
  
  Theta <- vector("list", L[2])
  p <- 0
  for (i in 1:L[2]) {
    Theta[[i]] <- matrix(theta[(p+1):(p+(model$layer[i]+(i==1))*model$layer[i+1])], nrow = model$layer[i]+(i==1), ncol = model$layer[i+1])
    p <- p + (model$layer[i]+(i==1))*model$layer[i+1]
  }
  Theta
}

