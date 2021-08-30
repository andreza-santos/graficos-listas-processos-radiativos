#################### Irradiância no topo e na superfície ####################
# Irradiância (Io): energia interceptada por unidade de área por unidade de tempo.
# Plotar Io para o topo da atmosfera para um local onde tenha estação do INMET.
# Plotar no mesmo gráfico a irradiância em superfície medida pelo INMET.

library(solrad)
options(digits = 5)

# Dados
HORA <- c(0:23)
DJ <- seq(1:365)                        # Dias juliano
n <- 147                                # Dia juliano para o dia 27/05.
lat_castanhal <- -1.297                 # Latitude (%phi) de Castanhal.
lon_castanhal <- -47.922                # Longitude (l) de Castahal.
lat_rad <- lat_castanhal * (pi / 180)   # Transformar em radiano

#Dados de Radiação do INMET para Castanhal em 27/05/2020 (= valor / 3,6 (Wm^-2))
ir_sup_INMET <- c(-0.95, -0.98 , -0.98, -0.96, -0.93, -0.95, -0.71, -0.88, -0.96, -0.87,
                  41.03, 241.69, 445.56, 458.89, 536.11, 610.00, 627.5, 480.56,
                  465, 361.39, 129.22, 31.58, 15.31, -0.97)
ir_sup <- ifelse(
  test = (ir_sup_INMET < 0),
  yes = 0,
  no = ir_sup_INMET
)
# ir_sup <- ir_sup[7:20]

# Declinação Solar para o dia juliano em questão.
# declin <- -23.45 * cos((2 * pi / 365) * (n + 10))
delta <- Declination(DJ)
delta <- delta[147]
declin_rad <- delta * (pi / 180)   # Convertendo para radiano
# plot(HORA, delta, type = "l")

#gama <- (2 * pi * (n - 1) / 365) * (180 * pi)
# Equação do tempo (Et).
# Et = [7*10^-5 - 0.0018 * cos(gama) - 0.032 * sin(gama) - 0.0146 * cos(2 * gama) - 0.0408 * sin(2 * gama)] * (1440 / 2 * pi)

eot <- EOT(DJ)
# plot(DJ, eot)
Et <- eot[147] / 60       # x / 60 = converter os minutos para hora

# Ângulo horário = h = [UTC + (l / 15) - (12 - Et)] * (360 / 24)
h <- (HORA + (lon_castanhal / 15) - (12 - Et)) * (360 / 24)
h_rad <- h * (pi / 180)   # Convertendo para radiano

# Ângulo zenital = cosZ = sin(fi) * sin(delta) + cos(fi) * cos(delta) * cos(h)
cosZ <- (sin(lat_rad) * sin(declin_rad)) + (cos(lat_rad) * cos(declin_rad) * cos(h_rad))
# cosZ <- cosZ * (180 / pi)

# Distância média = (dmed/d)^2
d_med <- 1 + 0.033 * cos(((2 * pi) * n) / 365)

# Irradiância no topo da atmosfera == 1360 * (d_med^2) * cosZ)
ir_topo <- 1360 * d_med * cosZ
ir_topo <- ifelse(
  test = ir_topo < 0,
  yes = 0,
  no = 1360 * d_med * cosZ
) 

plot(HORA,
     ir_topo,
     type = "l",
     lwd = 3,
     col = 2,
     main = "Irradiância no topo e na superfície (27/05/2020)",
     xlab = "Horas (UTC)",
     ylab = "Irradiância  (W m^-2)",
     yaxt = "n"
)
points(HORA, ir_sup, type = "l", col = 4)
grid(ny = 10, nx = 24)
box(lwd = 2)
axis(side=2, las = 1)
