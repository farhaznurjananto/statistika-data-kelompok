# INSTAL PACKAGES
# install.packages("readxl") # nolint


# IMPORT DATA EXCEL
library(readxl)
data_kelompok <- read_excel("data_covid.xlsx")
print(data_kelompok)


# INPUT FREKUESI
fi <- c(16, 20, 15, 19, 17, 9, 2, 2)
fi


# INPUT TITIK TENGAH
xi <- c(25.5, 49.5, 73.5, 97.5, 121.5, 145.5, 169.5, 193.5)
xi


# MEAN ( RATA- RATA )
mean <- sum(fi * xi) / sum(fi)
mean
mean <- round(mean)
mean


# MEDIAN
#Letak Median
med <- 1 / 2 * sum(fi)
med <- round(med)
med
#Tb (tepi bawah kelas median)
tb <- 62 - 0.5
tb
#fkk (frekuensi komulatif kurang dari kelas median)
fkk <- 36
#fm (frekuensi kelas median) & l (panjang kelas)
fm <- 15
l <- 24
median <- tb + ((1 / 2 * sum(fi) - fkk) / fm) * l
median


# MODUS
#b (tepi bawah kelas interval dengan frekuensi terbanyak)
b <- 38 - 0.5
b
#b1 (frekuensi kelas modus - frekuensi kelas sebelumnya)
b1 <- 20 - 16
b1
#b2 (frekuensi kelas modus - frekuensi kelas setelahnya)
b2 <- 20 - 15
b2
#p (panjanng kelas interval)
p <- 24
p
modus <- b + (b1 / (b1 + b2)) * p
modus
modus <- round(modus)
modus


# RANGE
xmax <- max(xi)
xmin <- min(xi)
range <- xmax - xmin
range


# MEAN DEVIASI (SIMPANGAN RATA-RATA)
sr <- sum(fi * abs(xi - mean)) / sum(fi)
sr


# STANDAR DEVIASI
# mencari ragam / varian dahulu
# karena sd merupakan akar kuadrat dari ragam / varian
s2 <- sum(fi * (xi - mean) ^ 2) / sum(fi)
s2
sd <- sqrt(s2)
sd
