library('scales')
mmBase <- 6

interval <- (0 * 1000):(203 * 1000)
interval <- (201 * 1000):(202 * 1000)

source(file = 'read.R')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Time, s'
plot(df$TIME, df$R1, type = 'l', xlab = xlab, col = col[1], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, ylab = 'POSITION, mm')

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

drho <- c(rho[1] * layer1InverseRelError(mmBase, mmBase * 3.0, 0.1), rho[2] * layer1InverseRelError(mmBase * 5.0, mmBase * 3.0, 0.1))
paste("drho =", round(drho, 3), collapse = "; ")

normD <- norm((log(rho + drho) - log(rho)), type = "2")
paste("norm D =", round(normD, 3))


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

paste("A(Ohm-m, mm) =", round(layer2Model(1, 10.0), 3), collapse = "; ")

misfit <- function(rho1, hmm) {
  normResult <- norm(log(layer2Model(rho1, hmm)) - log(rho), type = "2")
  return(normResult)
}

# 1. Определяем целевую функцию
P_func <- function(x) {
  return(misfit(rho1 = x[1], hmm = x[2])^2)
}

# 2. Запускаем оптимизацию
# par — начальные значения (угадка)
# fn — функция невязки
result <- optim(par = c(rho[1], mmBase * 5), fn = P_func, method = "Nelder-Mead")

m <- list()
m$rho1 <- result$par[1]
m$h <- result$par[2]
m$value <- result$value

# 3. Смотрим результат
paste("rho =", round(m$rho1, 3), "h =", round(m$h, 3),
      "misfit =", round(misfit(rho1 = m$rho1, hmm = m$h), 5))

library(ggplot2)
grid <- expand.grid(x = exp(seq(log(rho[1] / 10.0), log(rho[1]), length.out = 10)),
                    y = exp(seq(log(1.0), log(20.0), length.out = 10)))
grid$z <- apply(grid, 1, function(row) {
  misfit(row["x"], row["y"])^2
})

# Визуализация с заливкой контуров
ggplot(grid, aes(x, y, z = z)) +
  geom_contour_filled() +
  scale_x_log10() +
  scale_y_log10() +
  geom_contour(color = "white", alpha = 0.2) + # Добавляем тонкие линии
  annotate("point", x = m$rho1, y = m$h, color = "red", size = 3) + # Глобальный минимум
  theme_minimal() +
  labs(title = "Level Plot", fill = "Value")
