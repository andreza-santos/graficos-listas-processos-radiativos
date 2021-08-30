library(solrad)

DJ <- seq(1:365)
HORA <- c(0:23)
lat_castanhal <- -1.297                 # Latitude (%phi) de Castanhal.
lon_castanhal <- -47.922                # Longitude (l) de Castahal.
lat_rad <- lat_castanhal * (pi / 180)   # Transformar em radiano
n1 <- 263                                # Dia juliano para o dia 27/05.
n2 <- 264                                # Dia juliano para o dia 27/05.

# Ir superfície em dia de céu claro
ir_sup_INMET_cc <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 208.25, 1012.944, 1858.598, 2507.683,
               2782.886, 2628.431, 2473.977, 2283.633, 1604.575, 1299.34, 1093.03,
               227.411, 1.361, 0.1)
ir_sup_INMET_cc <- ir_sup_INMET_cc / 3.6

# Ir superfície em dia nublado
ir_sup_INMET_cn <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 991.825, 1729.229, 2480.497,
                 2433.103, 1949.424, 2461.847, 2174.771, 1763.275, 1503.906, 805.8155,
                 107.725, 1.827, 0.1)
ir_sup_INMET_cn <- ir_sup_INMET_cn / 3.6

# a0 <- 0.4237 − 0.00821 * (6 - 0)^2
# a1 <- 0.5055 − 0.00595 * (6.5 - 0)^2
# k <- 0.2711 − 0.01858 * (2.5 - 0)^2

delta <- Declination(DJ)
delta1 <- delta[n1]
declin_rad <- delta1 * (pi / 180)   # Convertendo para radiano
delta2 <- delta[n2]
declin_rad2 <- delta2 * (pi / 180)   # Convertendo para radiano

eot <- EOT(DJ)
Et <- eot[n1] / 60       # x / 60 = converter os minutos para hora
Et2 <- eot[n2] / 60       # x / 60 = converter os minutos para hora

# Ângulo horário = h = [UTC + (l / 15) - (12 - Et)] * (360 / 24)
h <- (HORA + (lon_castanhal / 15) - (12 - Et)) * (360 / 24)
h_rad <- h * (pi / 180)   # Convertendo para radiano
h2 <- (HORA + (lon_castanhal / 15) - (12 - Et2)) * (360 / 24)
h_rad2 <- h2 * (pi / 180)   # Convertendo para radiano

# Ângulo zenital = cosZ = sin(fi) * sin(delta) + cos(fi) * cos(delta) * cos(h)
cosZ <- (sin(lat_rad) * sin(declin_rad)) + (cos(lat_rad) * cos(declin_rad) * cos(h_rad))
cosZ2 <- (sin(lat_rad) * sin(declin_rad2)) + (cos(lat_rad) * cos(declin_rad2) * cos(h_rad2))

d_med <- 1 + 0.033 * cos(((2 * pi) * n1) / 365)
d_med2 <- 1 + 0.033 * cos(((2 * pi) * n2) / 365)

ir_topo <- 1360 * d_med * cosZ
ir_topo <- ifelse(
  test = ir_topo < 0,
  yes = 0,
  no = 1360 * d_med * cosZ
)

ir_topo2 <- 1360 * d_med2 * cosZ2
ir_topo2 <- ifelse(
  test = ir_topo2 < 0,
  yes = 0,
  no = 1360 * d_med * cosZ
)

td_cc <- ir_sup_INMET_cc / ir_topo
td_cc <- ifelse(
  test = HORA > 18,
  yes = 0,
  no = td_cc
)
td_cc <- ifelse(
  test = HORA < 10,
  yes = 0,
  no = td_cc
)

td_cn <- ir_sup_INMET_cn / ir_topo2
td_cn <- ifelse(
  test = HORA > 18,
  yes = 0,
  no = td_cn
)
td_cn <- ifelse(
  test = HORA < 10,
  yes = 0,
  no = td_cn
)

plot(HORA,
     td_cc,
     type = "l",
     lwd = 3,
     col = 2,
     main = "Transmissividade para os dias 20 e 21 de Setembro de 2020",
     xlab = "Horas (UTC)",
     ylab = "Transsividade",
     yaxt = "n",
     ylim = 0:1
     )
points(HORA, td_cn, type = "l", col = 4)
grid(ny = 10, nx = 24)
# box(lwd = 2)
axis(side=2, las = 1)
axis(side=1, las = 1, at = 23)
legend(0, 0.9, legend = c("Céu Claro", "Céu Nublado"), col = c(2, 4), lty = 1)

# td <- a0 + a1 * exp(-k / cosZ)
# td2 <- a0 + a1 * exp(-k / cosZ2)