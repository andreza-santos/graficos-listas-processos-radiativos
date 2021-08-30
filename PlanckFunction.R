# Comprimento de onda
lambda <- c(seq(from = 0, to = 30, by = 0.01))

# Variáveis possíveis
# c <- 299792458 <- 2.99793 * 10^8
# c <- 2.99793 * 10^8
# h <- 6.626*10^-34
# k <- 1.3806*10^-23
# f <- c / lambda
# E = h * f
# p = h / lambda
temp_sol <- 6000
temp_terra <- 255
# c1 <- 3.74 * 10^-16
# c1 <- 3.7415 * 10^8
# c2 <- 1.45 * 10^-2
# c2 <- 1.4388 * 10^4
c1 <- 1.191042 * 10**8
c2 <- 1.4387752 * 10**4

# Equações
# b_sol <- c1 * lambda^-5 / pi * (exp(c2 / lambda * temp_sol) - 1)
# B0 <- (2 * h * c**2 / lambda**5) / (exp(h * c / (lambda * k * temp_sol)) - 1)
# B1 <- (8 * pi * h * c / lambda**5) * (1 / (exp(h * c / (lambda * k * temp_sol)) - 1))
# B2 <- (2 * h * f / c**2) / (1 / (exp(h * f / (lambda * k * temp_sol)) - 1))
# B3 <- 2 * pi * h * c**2 / lambda**5 * (exp(h * c / lambda * k * temp_sol) - 1)
# B4 <- ((8 * pi * h * f**3 / c**3) * (1 / (exp(h * f / (k * temp_sol)) - 1)))
# B5 <- 2 * pi * h * f**3 / c**2 * (exp(h * f / k * temp_sol) - 1)
# B6 <- B4[-(1:2)]
# x <- lambda[-(1:2)]
# E <- c1 / lambda^5 * (exp(c2 / lambda  * temp_sol) -1)

# Equação usada
B_sol <- (1.191042*10**8)/((lambda**5)*(exp((1.4387752*10**4)/(lambda*temp_sol))-1))
# B_sol <- B_sol / 1e+6
B_terra <- (1.191042*10**8)/((lambda**5)*(exp((1.4387752*10**4)/(lambda*temp_terra))-1))
# B <- c(B_sol, B_terra)

# Gráfico da Função para Sol e Terra
plot(x = lambda,
     y = B_sol,
     type = "l",
     col = "red",
     # main = "",
     ylab = "Intensidade de radiação por comprimento de onda",
     xlab = "Comprimento de onda (μm)"
     )
par(new = TRUE)
plot(lambda,
     B_terra,
     type = "l",
     col = "blue",
     xaxt = "n",
     # ylab = "Intensidade de radiação por comprimento de onda",
     ylim = c(0, 15e+00),
     xlab = "",
     ylab = "",
     axes = FALSE)

# Notas: adicionar escala secundária ao lado direito e ajustar valor da escala