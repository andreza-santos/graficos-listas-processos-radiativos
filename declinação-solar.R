####################### Dia Juliano #######################
DJ <- seq(1:365)
###########################################################

#################### Declinação do Sol ####################
# delta <- 23.45 * sin((360/365) * (284 + DJ))
###########################################################

##################### Package solrad ######################

library(solrad)
DOY <- 147        # Dia do ano
Lat <- -1.297     # Latitude
Lon <- -47.922    # Longitude
SLon <- 48        # Longitude padrão
DS <- 0           # Daylight saving - in minutes
Slope <- 0
Aspect <- 0

##################### Declinação do Sol #####################

Declination(DOY)
Declination(DJ)
delta <- Declination(DJ)
delta_min <- min(delta)
delta_max <- max(delta)
plot(DJ,
     delta,
     type = "l",
     lwd = 3,
     col = 2,
     main = "Declinação Solar Diária",
     xlab = "Dia juliano",
     ylab = "Declinação Solar (graus)",
     ylim = c(-25, 25),
     yaxt = "n"
)
grid(ny = 46)
abline(h = 0, col = 1)
box(lwd = 2)
axis(side=2, las = 1, at = -24:24)
