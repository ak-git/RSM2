mmToSI <- function(mm) {
  return(mm / 1000.0)
}

rhoToK <- function(rho1, rho2) {
  if (is.infinite(rho2)) {
    1 -> k
  }
  else {
    (rho2 - rho1) / (rho2 + rho1) -> k
  }
  return(k)
}

mp <- function(ls) {
  return(2 / abs(ls))
}

MP <- function(ls, n, h) {
  ls^2 + (4 * n * h)^2 -> result
  return(2 / sqrt(result))
}

MAX_SUM <- 1024 * 8 * 2

layer2Ohms <- function(rho1, rho2, hmm, smm, lmm) {
  key <- paste(hmm)
  if (exists(key, envir = cacheEnv)) {
    return(get(key, envir = cacheEnv))
  }

  rhoToK(rho1, rho2) -> k
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  sum(sapply(1:MAX_SUM, function(x) ((k^x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> R

  mp(l - s) - mp(l + s) + 2 * R -> R

  (rho1 / pi) * R -> R
  assign(key, R, envir = cacheEnv)
  return(R)
}

layer2OhmsHIterate <- function(rho1, rho2, hmm, smm, lmm) {
  return(
    sapply(1:(length(hmm)),
           function(x) {
             return(layer2Ohms(rho1 = rho1, rho2 = rho2, hmm = hmm[x], smm = smm, lmm = lmm))
           }
    )
  )
}

mmBase <- 7
rho1 <- 5
rho2 <- 1
hmm <- 6
df <- eval(parse(text = ls(pattern = "PureLogic$")))
cacheEnv <- new.env()
df$R1 <- layer2OhmsHIterate(rho1 = rho1, rho2 = rho2, hmm = hmm + df$POSITION, smm = mmBase, lmm = mmBase * 3.0)
cacheEnv <- new.env()
df$R2 <- layer2OhmsHIterate(rho1 = rho1, rho2 = rho2, hmm = hmm + df$POSITION, smm = mmBase * 5.0, lmm = mmBase * 3.0)
df$H <- hmm + df$POSITION

aper <- data.frame(df$TIME, df$R1, df$R2)
colnames(aper) <- c('TIME', 'R1', 'R2')
