library(ggplot2)
grid <- expand.grid(x = exp(seq(log(0.1), log(min(rho[1], rho[2]) * 1.5), length.out = 20)),
                    y = exp(seq(log(1.0), log(mmBase * 4), length.out = 20)))
grid$z <- apply(grid, 1, function(row) {
  alpha * stab(row["x"], row["y"])
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

grid$z <- apply(grid, 1, function(row) {
  misfit(row["x"], row["y"])^2
})
ggplot(grid, aes(x, y, z = z)) +
  geom_contour_filled() +
  scale_x_log10() +
  scale_y_log10() +
  geom_contour(color = "white", alpha = 0.2) + # Добавляем тонкие линии
  annotate("point", x = m$rho1, y = m$h, color = "red", size = 3) + # Глобальный минимум
  theme_minimal() +
  labs(title = "Level Plot", fill = "Value")

grid$z <- apply(grid, 1, function(row) {
  P_func(c(row["x"], row["y"]))
})
ggplot(grid, aes(x, y, z = z)) +
  geom_contour_filled() +
  scale_x_log10() +
  scale_y_log10() +
  geom_contour(color = "white", alpha = 0.2) + # Добавляем тонкие линии
  annotate("point", x = m$rho1, y = m$h, color = "red", size = 3) + # Глобальный минимум
  theme_minimal() +
  labs(title = "Level Plot", fill = "Value")

