library(readxl)
library(rmarkdown)

# Paths are relative to the repo root. Run from there (or set cdf.path first).
if (!exists("cdf.path")) cdf.path <- "./"

# readxl-based stand-in for xlsx::read.xlsx (avoids the Java dependency)
read.xlsx <- function(file, sheetName, startRow = 1, header = TRUE) {
  d <- as.data.frame(read_excel(file, sheet = sheetName, skip = startRow - 1, col_names = header))
  names(d) <- make.names(names(d), unique = TRUE)
  d
}

# AGDRIFT DATA (FORTRAN2PY)
df_fortran2py <- read.csv(paste(cdf.path,"fortran2py_database.csv", sep=""), header = TRUE)
df_fortran2py <- df_fortran2py/100
df_fortran2py$distance_m <- c(0:100)
df_fortran2py$distance_ft <- df_fortran2py$distance_m * 3.28084

# AGDRIFT DATA (webversion) ####
df_python <- read.csv(paste(cdf.path, "python_database.csv", sep = ""), header = TRUE)

# AGDRIFT DATA (worksheet calculator)
df_aerial_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "aerial depostion", startRow = 2, header = TRUE)
df_ground_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "ground deposition", startRow = 2, header = TRUE)
df_airblast_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "airblast deposition", startRow = 2, header = TRUE)

# AGDRIFT DATA (exported from model)
df_model_aerial <- read.xlsx(file = paste(cdf.path, "agdrift_database.xlsx", sep = ""), sheetName = "Tier I Aerial", startRow = 1, header = TRUE)
df_model_ground <- read.xlsx(file = paste(cdf.path, "agdrift_database.xlsx", sep = ""), sheetName = "Tier I Ground", startRow = 1, header = TRUE)
df_model_airblast <- read.xlsx(file = paste(cdf.path, "agdrift_database.xlsx", sep = ""), sheetName = "Tier I Airblast", startRow = 1, header = TRUE)

# AGDRIFT CALCULATIONS ####
## TIER I AERIAL

## TIER I GROUND
# Low Boom -- D(x) = c/(1+ax)^b 
# x = downwind distance (ft)df_fortran2py <- read.csv(paste(cdf.path,"fortran2py_database.csv", sep=""), header = TRUE)

# x[1,1300] ft
df_ground_calc <- data.frame(distance_ft = numeric(length = 1001),
                             ground_low_vf2f_90 = numeric(1001),
                             ground_low_f2mc_90 = numeric(1001),
                             ground_low_vf2f_50 = numeric(1001),
                             ground_low_f2mc_50 = numeric(1001),
                             ground_high_vf2f_90 = numeric(1001),
                             ground_high_f2mc_90 = numeric(1001),
                             ground_high_vf2f_50 = numeric(1001),
                             ground_high_f2mc_50 = numeric(1001), 
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:1000) {
  # distance_ft
  df_ground_calc$distance_ft[x+1] <- x
  # vf2f_90
  if (x < 25) {df_ground_calc$ground_low_vf2f_90[x+1] <- 1.0/(1.0 + 0.4081*x)^1.5866} else {df_ground_calc$ground_low_vf2f_90[x+1] <- 1.3322/(1.0 + 0.5262*x)^1.5548}
  # f2mc_90
  if (x < 25) {df_ground_calc$ground_low_f2mc_90[x+1] <- 1.0/(1.0 + 2.0913*x)^1.2572} else {df_ground_calc$ground_low_f2mc_90[x+1] <- 0.1419/(1.0 + 0.3187*x)^1.3885}
  # vf2f_50
  if (x < 25) {df_ground_calc$ground_low_vf2f_50[x+1] <- 1.0/(1.0 + 0.8082*x)^1.5208} else {df_ground_calc$ground_low_vf2f_50[x+1] <- 1.2628/(1.0 + 0.9749*x)^1.5085}
  #f2mc_50
  if (x < 25) {df_ground_calc$ground_low_f2mc_50[x+1] <- 1.0/(1.0 + 1.3742*x)^1.4863} else {df_ground_calc$ground_low_f2mc_50[x+1] <- 1.2205/(1.0 + 1.6014*x)^1.4803}
  
  # High Boom -- D(x) = (c/(1+ax)^b) * (1+A exp(-Bx)) #exponential expression only for x > 25 ft
  #vf2f_90
  if (x < 25) {df_ground_calc$ground_high_vf2f_90[x+1] <- 1.0/(1.0 + 0.1243*x)^1.9100} else {df_ground_calc$ground_high_vf2f_90[x+1] <- (1.3322/(1.0 + 0.5262*x)^1.5548) * (1.0 + 2.1749*exp(-0.001185*x))}
  #f2mc_90
  if (x < 25) {df_ground_calc$ground_high_f2mc_90[x+1] <- 1.0/(1.0 + 1.3058*x)^1.2714} else {df_ground_calc$ground_high_f2mc_90[x+1] <- (0.1419/(1.0 + 0.3187*x)^1.3885) * (1.0 + 0.7089*exp(-0.000754*x))}
  #vf2f_50
  if (x < 25) {df_ground_calc$ground_high_vf2f_50[x+1] <- 1.0/(1.0 + 0.1409*x)^1.8605} else {df_ground_calc$ground_high_vf2f_50[x+1] <- (1.2628/(1.0 + 0.9749*x)^1.5085) * (1.0 + 5.4877*exp(-0.001541*x))}
  #f2mc_50
  if (x < 25) {df_ground_calc$ground_high_f2mc_50[x+1] <- 1.0/(1.0 + 0.7802*x)^1.5183} else {df_ground_calc$ground_high_f2mc_50[x+1] <- (1.2205/(1.0 + 1.6014*x)^1.4803) * (1.0 + 1.0639*exp(-0.000910*x))}
}

## TIER I ORCHARD/AIRBLAST -- D(x) = c/(1+ax)^b # x[1,600] ft
df_airblast_calc <- data.frame(distance_ft = numeric(length = 1001),
                             airblast_normal_outside = numeric(length = 1001),
                             airblast_normal_inside = numeric(length = 1001),
                             airblast_dense_outside = numeric(length = 1001),
                             airblast_dense_inside = numeric(length = 1001),
                             airblast_sparse_outside = numeric(length = 1001),
                             airblast_sparse_inside = numeric(length = 1001),
                             airblast_vineyard_outside = numeric(length = 1001),
                             airblast_vineyard_inside = numeric(length = 1001),
                             airblast_orchard_outside = numeric(length = 1001),
                             airblast_orchard_inside = numeric(length = 1001),
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:1000) {
  # distance_ft
  df_airblast_calc$distance_ft[x+1] <- x
  df_airblast_calc$airblast_normal_outside[x+1] <- 0.9737/(1.0 + 1.0291*x)^1.9286
  df_airblast_calc$airblast_normal_inside[x+1] <- 0.1320/(1.0 + 1.3961*x)^1.4996
  df_airblast_calc$airblast_dense_outside[x+1] <- 2.9451/(1.0 + 0.1300*x)^2.5037
  df_airblast_calc$airblast_dense_inside[x+1] <- 0.2190/(1.0 + 1.6150*x)^1.1886
  df_airblast_calc$airblast_sparse_outside[x+1] <- 5.1769/(1.0 + 0.0629*x)^3.3201
  df_airblast_calc$airblast_sparse_inside[x+1] <- 5.2652/(1.0 + 0.1582*x)^2.7868
  df_airblast_calc$airblast_vineyard_outside[x+1] <- 2.4409/(1.0 + 0.3581*x)^2.7104
  df_airblast_calc$airblast_vineyard_inside[x+1] <- 0.7219/(1.0 + 1.5541*x)^1.7194
  df_airblast_calc$airblast_orchard_outside[x+1] <- 5.8017/(1.0 + 0.1183*x)^2.7872
  df_airblast_calc$airblast_orchard_inside[x+1] <- 0.3620/(1.0 + 1.0699*x)^1.3649
}


# AGDRIFT PLOTTING ####
# Aerial
## Very Fine to Fine
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(df_aerial_excel$Distance..ft., df_aerial_excel$Very.Fine.to.Fine, main = "vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300), ylim=c(0, 0.55))
lines(df_fortran2py$distance_ft, df_fortran2py$Aerial.vf2f, lty = 2, col = "green3")
lines(df_model_aerial$Distance_ft, df_model_aerial$vf2f_dep, lty = 3, col = "red")
lines(df_model_aerial$Distance_ft, df_model_aerial$vf2f_ponddep, lty = 4, col = "purple")
legend("topright", c("Peck", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black","green3", "red", "purple"), lty = c(1, 2, 3, 4), bg = "gray90", cex=1)
## Fine to medium
plot(df_aerial_excel$Distance..ft., df_aerial_excel$Fine.to.Med, main = "f2m", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300), ylim=c(0, 0.55))
lines(df_fortran2py$distance_ft, df_fortran2py$Aerial.f2m, lty=2, col = "green3")
lines(df_model_aerial$Distance_ft, df_model_aerial$f2m_dep, lty=3, col = "red")
lines(df_model_aerial$Distance_ft, df_model_aerial$f2m_ponddep, lty=4, col = "purple")
legend("topright", c("Peck", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black", "green3", "red", "purple"), lty = c(1, 2, 3, 4), bg = "gray90", cex=1)
## Medium to coarse
plot(df_aerial_excel$Distance..ft., df_aerial_excel$Med.to.Coarse, type = "l", main = "m2c", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300), ylim=c(0, 0.55))
lines(df_fortran2py$distance_ft, df_fortran2py$Aerial.m2c, lty=2, col = "green3")
lines(df_model_aerial$Distance_ft, df_model_aerial$m2c_dep, lty=3, col= "red")
lines(df_model_aerial$Distance_ft, df_model_aerial$m2c_ponddep, lty=4, col = "purple")
legend("topright", c("Peck", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black","green3", "red", "purple"), lty = c(1, 2, 3, 4), bg = "gray90", cex=1)
## Coarse to very coarse
plot(df_aerial_excel$Distance..ft., df_aerial_excel$Coarse.to.VC, type = "l", main = "c2vc", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300), ylim=c(0, 0.55))
lines(df_fortran2py$distance_ft, df_fortran2py$aerial.c2vc, lty = 2, col="green3")
lines(df_model_aerial$Distance_ft, df_model_aerial$c2vc_dep, lty=3, col = "red")
lines(df_model_aerial$Distance_ft, df_model_aerial$c2vc_ponddep, lty=4, col ="purple")
legend("topright", c("Peck", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black","green3", "red", "purple"), lty = c(1, 2, 3,4), bg = "gray90", cex=1)

# Ground
## Low- very fine to fine
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(df_ground_excel$Distance..ft., df_ground_excel$Low.boom..vf, main = "Low vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300),ylim=c(0,0.5))
lines(df_ground_calc$distance_ft, df_ground_calc$ground_low_vf2f_90, lty=2, col = "blue")
lines(df_ground_calc$distance_ft, df_ground_calc$ground_low_vf2f_50, lty=2, col = "green3")
lines(df_fortran2py$distance_ft, df_fortran2py$Ground.low.fine.90, lty=3, col = "red")
lines(df_model_ground$Distance_ft, df_model_ground$low_vf2f_90_dep, lty = 5, col ="orange")
lines(df_model_ground$Distance_ft, df_model_ground$low_vf2f_50_dep, lty = 5, col = "brown")
lines(df_model_ground$Distance_ft, df_model_ground$low_vf2f_90_ponddep, lty = 6, col = "darkcyan")
lines(df_model_ground$Distance_ft, df_model_ground$low_vf2f_50_ponddep, lty = 6, col = "magenta")
legend("topright", c("Peck", "Calc 90", "Calc 50", "EXPRESS", "GUI 90 Dep", "GUI 50 Dep", "GUI 90 Ponddep", "GUI 50 Ponddep"), col = c("black", "blue","green3", "red", "orange", "brown", "darkcyan", "magenta"), lty = c(1, 2, 2, 3, 5,5,6,6), bg = "gray90", cex = 1)
## Low- fine to medium coarse
plot(df_ground_excel$Distance..ft., df_ground_excel$low.boom..fmc, main = "Low f2mc", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300),ylim=c(0,0.5))
lines(df_ground_calc$distance_ft, df_ground_calc$ground_low_f2mc_90, col = "blue", lty=2)
lines(df_ground_calc$distance_ft, df_ground_calc$ground_low_f2mc_50, col = "green3", lty=2)
lines(df_fortran2py$distance_ft, df_fortran2py$ground.low.m2c.90, col="red", lty=3)
lines(df_python$distance_ft, df_python$pond_ground_low_f2m, col = "purple", lty=4)
lines(df_model_ground$Distance_ft, df_model_ground$low_f2m_90_dep, lty = 5, col ="orange")
lines(df_model_ground$Distance_ft, df_model_ground$low_f2m_50_dep, lty = 5, col = "brown")
lines(df_model_ground$Distance_ft, df_model_ground$low_f2m_90_ponddep, lty = 6, col = "darkcyan")
lines(df_model_ground$Distance_ft, df_model_ground$low_f2m_50_ponddep, lty = 6, col = "magenta")
legend("topright", c("Peck", "Calc 90","Calc 50", "EXPRESS", "Python Pond", "GUI 90 Dep", "GUI 50 Dep", "GUI 90 Ponddep", "GUI 50 Ponddep"), col = c("black", "blue","green3", "red", "purple", "orange", "brown", "darkcyan", "magenta"), lty = c(1, 2, 2, 3, 4, 5,5,6,6), bg = "gray90", cex = 1)
## High- very fine to fine
plot(df_ground_excel$Distance..ft., df_ground_excel$High.boom..vf, main = "High Vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300),ylim=c(0,0.5))
lines(df_ground_calc$distance_ft, df_ground_calc$ground_high_vf2f_90, col = "blue", lty=2)
lines(df_ground_calc$distance_ft, df_ground_calc$ground_high_vf2f_50, col = "green3", lty=2)
lines(df_fortran2py$distance_ft, df_fortran2py$ground.high.fine.90, col="red",lty=3)
lines(df_model_ground$Distance_ft, df_model_ground$high_vf2f_90_dep, lty = 5, col ="orange")
lines(df_model_ground$Distance_ft, df_model_ground$high_vf2f_50_dep, lty = 5, col = "brown")
lines(df_model_ground$Distance_ft, df_model_ground$high_vf2f_90_ponddep, lty = 6, col = "darkcyan")
lines(df_model_ground$Distance_ft, df_model_ground$high_vf2f_50_ponddep, lty = 6, col = "magenta")
legend("topright", c("Peck","Calc 90", "Calc 50", "EXPRESS", "GUI 90 Dep", "GUI 50 Dep", "GUI 90 Ponddep", "GUI 50 Ponddep"), col = c("black", "blue","green3", "red", "orange", "brown", "darkcyan", "magenta"), lty = c(1, 2, 2, 3, 5,5,6,6), bg = "gray90", cex = 1)
## High- fine to medium coarse
plot(df_ground_excel$Distance..ft., df_ground_excel$high.boom..fmc, main = "High f2mc", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300),ylim=c(0,0.5))
lines(df_ground_calc$distance_ft, df_ground_calc$ground_high_f2mc_90, col = "blue", lty=2)
lines(df_ground_calc$distance_ft, df_ground_calc$ground_high_f2mc_50, col="green3",lty=2)
lines(df_fortran2py$distance_ft, df_fortran2py$Ground.high.m2c.90, col="red", lty=3)
lines(df_model_ground$Distance_ft, df_model_ground$high_f2m_90_dep, lty = 5, col ="orange")
lines(df_model_ground$Distance_ft, df_model_ground$high_f2m_50_dep, lty = 5, col = "brown")
lines(df_model_ground$Distance_ft, df_model_ground$high_f2m_90_ponddep, lty = 6, col = "darkcyan")
lines(df_model_ground$Distance_ft, df_model_ground$high_f2m_50_ponddep, lty = 6, col = "magenta")
legend("topright", c("Peck", "Calc 90", "Calc 50", "EXPRESS", "GUI 90 Dep", "GUI 50 Dep", "GUI 90 Ponddep", "GUI 50 Ponddep"), col = c("black", "blue","green3", "red", "orange", "brown", "darkcyan", "magenta"), lty = c(1, 2, 2, 3, 5,5,6,6), bg = "gray90", cex = 1)

# Airblast
## orchard
par(mfrow=c(5,1), mar = c(4,4,2,1))
plot(df_airblast_excel$Distance..ft., df_airblast_excel$Orchard, main = "Orchard", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,300))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_orchard_outside, col="blue", lty=2)
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_orchard_inside, col = "green3", lty=2)
lines(df_fortran2py$distance_ft, df_fortran2py$Airblast.Orchard.50, col="purple", lty=4)
lines(df_model_airblast$Distance_ft, df_model_airblast$orchard_dep, col = "orange", lty=5)
lines(df_model_airblast$Distance_ft, df_model_airblast$orchard_ponddep, col = "brown", lty =6)
legend("topright", c("Peck", "Calc Outside", "Calc Inside", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black", "blue", "green3", "purple", "orange", "brown"), lty = c(1, 2, 2, 4, 5,6), bg = "gray90", cex=1)
## Vineyard
plot(df_airblast_excel$Distance..ft., df_airblast_excel$Vineyard, main = "Vineyard", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,300))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_vineyard_outside, col="blue", lty=2)
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_vineyard_inside, col = "green3", lty=2)
lines(df_python$distance_ft, df_python$pond_airblast_vineyard, col = "red", lty=3)
lines(df_fortran2py$distance_ft, df_fortran2py$Airblast.vineyard.50, col="purple", lty=4)
lines(df_model_airblast$Distance_ft, df_model_airblast$vineyard_dep, col = "orange", lty=5)
lines(df_model_airblast$Distance_ft, df_model_airblast$vineyard_ponddep, col = "brown", lty =6)
legend("topright", c("Peck", "Calc Outside", "Calc Inside", "Python Pond", "EXPRESS", "GUI Dep", "GUI Ponddep"), col = c("black", "blue", "green3", "red", "purple", "orange", "brown"), lty = c(1, 2, 2, 3, 4, 5,6), bg = "gray90", cex=1)
## Normal
plot(df_airblast_excel$Distance..ft., df_airblast_excel$Normal, main = "Normal", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,300))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_normal_outside, col="blue", lty=2)
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_normal_inside, col = "green3", lty=2)
lines(df_model_airblast$Distance_ft, df_model_airblast$normal_dep, col = "red", lty = 3)
lines(df_model_airblast$Distance_ft, df_model_airblast$normal_ponddep, col = "purple", lty = 4)
legend("topright", c("Peck", "Calc Outside", "Calc Inside", "GUI Dep", "GUI Ponddep"), col = c("black", "blue", "green3", "red", "purple"), lty = c(1, 2, 2,3,4), bg = "gray90", cex = 1)
## Sparse
plot(df_airblast_excel$Distance..ft., df_airblast_excel$Sparse, main = "Sparse", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,300))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_sparse_outside, col="blue", lty=2)
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_sparse_inside, col = "green3", lty=2)
lines(df_model_airblast$Distance_ft, df_model_airblast$sparse_dep, col = "red", lty = 3)
lines(df_model_airblast$Distance_ft, df_model_airblast$sparse_ponddep, col = "purple", lty = 4)
legend("topright", c("Peck", "Calc Outside", "Calc Inside", "GUI Dep", "GUI Ponddep"), col = c("black", "blue", "green3", "red", "purple"), lty = c(1, 2, 2,3,4), bg = "gray90", cex = 1)
## Dense
plot(df_airblast_excel$Distance..ft., df_airblast_excel$Dense, main = "Dense", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,300))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_dense_outside, col="blue", lty=2)
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_dense_inside, col = "green3", lty=2)
lines(df_model_airblast$Distance_ft, df_model_airblast$dense_dep, col = "red", lty = 3)
lines(df_model_airblast$Distance_ft, df_model_airblast$dense_ponddep, col = "purple", lty = 4)
legend("topright", c("Peck", "Calc Outside", "Calc Inside", "GUI Dep", "GUI Ponddep"), col = c("black", "blue", "green3", "red", "purple"), lty = c(1, 2, 2,3,4), bg = "gray90", cex = 1)
