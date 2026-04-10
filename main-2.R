R <- c(mean(df$R1), mean(df$R2))
paste("R =", round(R, 3), collapse = "; ")

mmToSI <- function(mm) {
  return(mm / 1000.0)
}

layer1Apparent <- function(smm, lmm, ohms) {
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  (ohms * pi / 2.0) / (1.0 / abs(l - s) - 1.0 / abs(l + s)) -> rho
  return(rho)
}

rho <- c(layer1Apparent(mmBase, mmBase * 3.0, R[1]), layer1Apparent(mmBase * 5.0, mmBase * 3.0, R[2]))
paste("rho =", round(rho, 3), collapse = "; ")

layer1InverseRelError <- function(smm, lmm, dLmm) {
  mmToSI(min(smm, lmm)) -> s
  mmToSI(max(smm, lmm)) -> L
  mmToSI(dLmm) -> dL

  x <- s / L
  return((1.0 + x) / (x * (1.0 - x)) * dL / L)
}

absErrRho <- c(rho[1] * layer1InverseRelError(mmBase, mmBase * 3.0, 0.1), rho[2] * layer1InverseRelError(mmBase * 5.0, mmBase * 3.0, 0.1))
paste("abs error rho =", round(absErrRho, 3), collapse = "; ")

d <- function(rho) {
  return(log(rho))
}

normErrorD <- norm((d(rho + absErrRho) - d(rho)), type = "2")
paste("norm error D =", round(normErrorD, 3))

layer2Apparent <- function(rho1, rho2, hmm, smm, lmm) {
  MAX_SUM <- 1024

  MP <- function(ls, n, h) {
    ls^2 + (4 * n * h)^2 -> result
    return(1 / sqrt(result))
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

  rhoToK(rho1, rho2) -> k
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  sum(sapply(1:MAX_SUM, function(x) ((k^x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> R

  rho1 * (1.0 + 2 * R / (1.0 / abs(l - s) - 1.0 / abs(l + s))) -> apparentRho
  return(apparentRho)
}

layer2Model <- function(rho1, hmm) {
  c(layer2Apparent(rho1, Inf, hmm, mmBase, mmBase * 3.0),
    layer2Apparent(rho1, Inf, hmm, mmBase * 5.0, mmBase * 3.0)) -> Am
  return(Am)
}

A <- function(rho1, hmm) {
  return(log(layer2Model(rho1, hmm)))
}

misfit <- function(rho1, hmm) {
  normResult <- norm(A(rho1, hmm) - log(rho), type = "2")
  return(normResult)
}

stab <- function(rho1, hmm) {
  r <- (log(2.0 * rho[1] - rho1) - log(rho1))^2
  h <- (log(2.0 * mmBase * 5 - hmm) - log(hmm))^2
  return(r + h)
}


alpha <- 0.003

# 1. Определяем целевую функцию
P_func <- function(x) {
  if (0 < x[1] && x[1] < min(rho[1], rho[2]) * 2 && 0 < x[2] && x[2] < mmBase * 5) {
    return(misfit(rho1 = x[1], hmm = x[2])^2 + alpha * stab(rho1 = x[1], hmm = x[2]))
  }
  else {
    return(Inf)
  }
}

# 2. Запускаем оптимизацию
# par — начальные значения (угадка)
# fn — функция невязки
result <- optim(par = c(min(rho[1], rho[2]), mmBase * 3), fn = P_func, method = "Nelder-Mead")

m <- list()
m$rho1 <- result$par[1]
m$h <- result$par[2]
m$value <- result$value

# 3. Смотрим результат
paste("rho =", round(m$rho1, 3), "h =", round(m$h, 3),
      "misfit =", round(misfit(rho1 = m$rho1, hmm = m$h), 3))