#################### Irradiância no topo e na superfície ####################
# Irradiância (Io): energia interceptada por unidade de área por unidade de tempo.
# Plotar Io para o topo da atmosfera para um local onde tenha estação do INMET.
# Plotar no mesmo gráfico a irradiância em superfície medida pelo INMET.

if(!require(solrad)) { install.packages("solrad")}
library(solrad)
options(digits = 5)

# Dados
# DIAS <- c(rep(0:23, 365))
DJ <- seq(1:365)                        # Dias juliano
lat_castanhal <- -1.297                 # Latitude (%phi) de Castanhal.
lon_castanhal <- -47.922                # Longitude (l) de Castahal.
lat_rad <- lat_castanhal * (pi / 180)   # Transformar em radiano

# Declinação Solar para o dia juliano em questão.
# declin <- -23.45 * cos((2 * pi / 365) * (n + 10))
delta <- Declination(DJ)
declin_rad <- delta * (pi / 180)   # Convertendo para radiano
# plot(DJ, delta, type = "l")

eot <- EOT(DJ)
# plot(DJ, eot)
Et <- eot / 60       # x / 60 = converter os minutos para hora

# Ângulo horário = h = [UTC + (l / 15) - (12 - Et)] * (360 / 24)
h <- (15 + (lon_castanhal / 15) - (12 - Et)) * (360 / 24)
h_rad <- h * (pi / 180)   # Convertendo para radiano
plot(DJ, h, type = "l")

# Ângulo zenital = cosZ = sin(fi) * sin(delta) + cos(fi) * cos(delta) * cos(h)
cosZ <- (sin(lat_rad) * sin(declin_rad)) + (cos(lat_rad) * cos(declin_rad) * cos(h_rad))
# cosZ <- cosZ * (180 / pi)

# Distância média = (dmed/d)^2
d_med <- 1 + 0.033 * cos(((2 * pi) * DJ) / 365)

# Irradiância no topo da atmosfera == 1360 * (d_med^2) * cosZ)
ir_topo <- 1360 * d_med * cosZ
# ir_topo <- ifelse(
#   test = ir_topo < 0,
#   yes = 0,
#   no = 1360 * d_med * cosZ
# )

# DataFrame
# Topo <- data.frame(HORA, ir_topo)
# Sup <- data.frame(HORA, ir_sup)
# Io <- merge(Topo, Sup, all = T)

plot(DJ,
     ir_topo,
     type = "l",
     lwd = 1,
     col = 1,
     main = "Irradiância no topo da atmosfera",
     xlab = "Dia juliano",
     ylab = "Irradiância (W/m²)",
     yaxt = "n",
)
legend(140, 1370,"Castanhal/PA", pch = 1)
grid(ny = 10, nx = 30)
axis(side = 1, las = 1, at = 365)
axis(side = 2, las = 1)
