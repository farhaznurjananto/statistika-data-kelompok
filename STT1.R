# ==========================< Mean >================================
## Interval

interval <-c('14-37', '38-61', '62-85', '86-109', '110-133', '134-157','158-181', '182-205')
min_interval <- c (min(14:37), min(38:61), min(62:85), min(86:109), min(110:133), min(134:157), min(158:181), min(182:205))
max_interval <- c (max(14:37), max(38:61), max(62:85), max(86:109), max(110:133), max(134:157), max(158:181), max(182:205))

# => Xi Nilai Tengah Interval
# Rumus : (max_interval - min_interval)/2 + min_interval
Xi <- c((max_interval - min_interval)/2 + min_interval)

# => Fi Frekuensi
Fi <-c(16, 20, 15, 19, 17, 9, 2, 2)

# => FiXi Rata_rata data tunggal
FiXi = Fi*Xi

# => Total
Tfi = sum(Fi)
Tfixi = sum(FiXi)

# => Table
#------------------------------
#No|Interval| Xi  | Fi | FiXi |
#------------------------------
#1 |31 – 40 |35,5 | 38 |1349  |
#2 |41 - 50 |45,5 | 23 |1046,5|
#3 |51 - 60 |55,5 | 12 |666   |
#4 |61 - 70 |65,5 | 12 |786   |
#5 |71 - 80 |75,5 | 10 |755   |
#6 |81 - 90 |85,5 | 3  |256,5 |
#7 |91 - 100|95,5 | 2  |191   |
#------------------------------
# TOTAL     | 100 |    5050   |

# => Mean
# Rumus : Total FiXi/ Total Fi
Mean = Tfixi/ Tfi

# Create Table

table <- data.frame(interval,Xi,Fi,FiXi)

# table mean
table_mean <- data.frame(
  Total_FiXi = Tfixi, 
  Total_Frekuensi = Tfi,
  Mean)
# ==========================< Median >================================

# Fk(1) = Fi(1)
# Rumus FK(n) = Fk(n-1) + Fi(n)
Fk_1 <- Fk_2 <- Fk_3 <- Fk_4 <-Fk_5 <- Fk_6 <- Fk_7 <-Fk_8 <- 0
Fk <- c(Fk_1, Fk_2, Fk_3, Fk_4, Fk_5, Fk_6, Fk_7, Fk_8)
for( i in 1:(length(Fk))){
  if (i == 1){
    Fk[i] = Fi[i]
  }
  else if (i > 1){
    Fk [i] = Fk[i-1] + Fi[i]
  }
}

# +> Tb bawah

Tb = min_interval-0.5
# Jumlah Jarak Antar Interval 

P = Tb[2]- Tb[1]

#----------------------------
#No|Interval| tb  | Fi | Fk |
#----------------------------
#1 |31 – 40 |30,5 | 38 | 38 |
#2 |41 - 50 |40,5 | 23 | 61 |
#3 |51 - 60 |50,5 | 12 | 73 |
#4 |61 - 70 |60,5 | 12 | 85 |
#5 |71 - 80 |70,5 | 10 | 95 |
#6 |81 - 90 |80,5 | 3  | 98 |
#7 |91 - 100|90,5 | 2  |100 |
#----------------------------

# Nilai Tengah
NT = sum(Fi)/2
# total Frekuensi


P = 10
# fi_m = 23

# Jadi Median berada pada Interval 41 -50
#TB m
Tb_m = Tb [2]
# FK SEBELUM
Fks = Fk[2] - Fi[2]

#Fi_m
Fi_m = Fi[2]

# < Me = Median
# Rumus = tb + p ((Tfi/2-Fks)/ Fi[2])
Me = Tb_m + P *((sum(Fi)/2-Fks)/ Fi_m)

# Create Table

table_median<- data.frame(interval,Tb,Fi,Fk)

# table median
table_medianF <- data.frame(Interval_median = interval [2], Nilai_Tengah = NT,Jarak_Interval = P, Frekuensi = Fi_m, Batas_bawah=Tb_m, Frekuensi_Komulatif= Fks, Median = Me )


# ==========================< Modus >================================
# => Table
#-----------------
#No|Interval| Fi |
#-----------------
#1 |31 – 40 | 38 |
#2 |41 - 50 | 23 |
#3 |51 - 60 | 12 |
#4 |61 - 70 | 12 |
#5 |71 - 80 | 10 |
#6 |81 - 90 | 3  |
#7 |91 - 100| 2  |
#-----------------
# TOTAL     | 100|

## Interval
# +> Modus = Nilai Terbanyak 
#Diket
Tb = min_interval-0.5
Interval_Modus = interval[1]
Tb_modus <- Tb[1]
b1 <- (Fi[1]-0) #Jumlah Frekuensi dikurangi Frekuensi sebelumnya
b2 <- (Fi[1]-Fi[2]) #Jumlah Frekuensi dikurangi Frekuensi setelahnya
P = Tb[2]- Tb[1]

# => Modus
# Rumus : tb + p * (b1/(b1+b2))

modus <- Tb_modus + P * (b1/(b1+b2))

# Create Table

table_modus <- data.frame(interval,Fi)

# table modus
table_modusF <- data.frame(Interval_Modus, Jarak_Interval= P, b1, b2, Modus = modus)

#===============================Range=================================
## Interval



# => Xi Nilai Tengah Interval
Xi <- c((max_interval - min_interval)/2 + min_interval)

# => Fi Frekuensi
Fi <-c(16, 20, 15, 19, 17, 9, 2, 2)

# => Table
#-----------------------
#No|Interval| Xi  | Fi |
#-----------------------
#1 |31 – 40 |35,5 | 38 |
#2 |41 - 50 |45,5 | 23 |
#3 |51 - 60 |55,5 | 12 |
#4 |61 - 70 |65,5 | 12 |
#5 |71 - 80 |75,5 | 10 |
#6 |81 - 90 |85,5 | 3  |
#7 |91 - 100|95,5 | 2  |
#-----------------------
# TOTAL           | 100|

# => Range
# Rumus : Xi Maximal - Xi Minimal

# Mencari Xi Maksimal dan Xi Minimal
ximax <- c(max(Xi))
ximin <- c(min(Xi))

# Mencari Range
range <- c(ximax-ximin)

# Create Table

table_range<- data.frame(interval,Xi,Fi)

# table range
table_rangef <- data.frame(
  Xi_Max = ximax, 
  Xi_Min = ximin,
  Range = range)
#========================== < Mean deviasi >===========================
#. MEAN DEVIASI ( SIMPANGAN RATA – RATA)
# Diket 
FiXi = Fi*Xi
Xi <- c((max_interval - min_interval)/2 + min_interval)
#-----------------------------------------------------
#No|Interval| Xi  | Fi | FiXi | |Xi - X| | Fi |Xi-X| |
#-----------------------------------------------------
#1 |31 – 40 |35,5 | 38 |1349  |  15      |  570      |
#2 |41 - 50 |45,5 | 23 |1046,5|  5       |  115      |
#3 |51 - 60 |55,5 | 12 |666   |  5       |  60       |
#4 |61 - 70 |65,5 | 12 |786   |  15      |  180      |
#5 |71 - 80 |75,5 | 10 |256,5 |  25      |  250      |
#6 |81 - 90 |85,5 | 3  |256,5 |  35      |  105      |
#7 |91 - 100|95,5 | 2  |191   |  45      |  90       |
#-----------------------------------------------------
# TOTAL     | 100 |           5050                   |
#-----------------------------------------------------

# Xi-X
Xi_X <- abs(Xi-Mean) #merupakan Xi_X
#catatan coba 2 adalah |Xi-mean|

fi_Xi_X <- c(Fi*Xi_X)
# Total Xi_X
Sum_Xi_X<- sum(Xi_X)
Sum_fi_Xi_X <- sum(fi_Xi_X)
#Total Frekuensi
Tfi = sum(Fi)
#Simpangan Rata
# Rumus SR = fi(|Xi-X|)/Fi total
SR = Sum_fi_Xi_X / Tfi

# Table
table_mean_deviasi <- data.frame(interval,Xi, Fi,FiXi,Xi_X,fi_Xi_X)

table_mean_deviasif <- data.frame(Total_Xi_X = Sum_Xi_X, Total_fi_Xi_X = Sum_fi_Xi_X, Total_Frekuensi = Tfi, Mean_Deviasi = SR)

#========================================Standar Deviasi==================================

#. STANDAR DEVIASI ( SIMPANGAN BAKU)
#Diket 
# => Xi Nilai Tengah Interval
Xi <- c((max_interval - min_interval)/2 + min_interval)

# => Fi Frekuensi
Fi <-c(16, 20, 15, 19, 17, 9, 2, 2)

FiXi = Fi*Xi
# x = Mean
Mean = Tfixi/ Tfi


#-----------------------------------------------------------
#No|Interval| Xi  | Fi | fi xi | (xi-X)^2 | Fi*(Xi-X)^2
#-----------------------------------------------------------
#1 |31 – 40 |35,5 | 38 | 1349  | 225     | 8550
#2 |41 - 50 |45,5 | 23 | 1046,5| 25      | 575
#3 |51 - 60 |55,5 | 12 | 666   | 25      | 300
#4 |61 - 70 |65,5 | 12 | 786   | 225     | 2700
#5 |71 - 80 |75,5 | 10 | 755   | 625     | 6250
#6 |81 - 90 |85,5 | 3  | 256,5 | 1225    | 3675
#7 |91 - 100|95,5 | 2  | 191   | 2025    | 4050
#----------------------------------------------------
#TOTAL            |100 | 5050  | 4375    | 26100

# Menghitung Xi-X^2
Xi_min_X = (Xi - Mean)^2
#Menghitung Fi*(Xi-Mean)^2
Fi_Kali_Xi_min_X = Fi * Xi_min_X

# Total Xi-X^2
Total_Xi_min_X = sum(Xi_min_X)
# Total Fi*(Xi-Mean)^2
Total_Fi_Kali_Xi_min_X = sum(Fi_Kali_Xi_min_X)

# Simpangan Baku
SB <- sqrt(Total_Fi_Kali_Xi_min_X/Tfi)

#Table
table_standar_deviasi <- data.frame(interval,Xi, Fi,FiXi,Xi_min_X, Fi_Kali_Xi_min_X)

table_standar_deviasif <- data.frame(Total_Xi_min_X , Total_Fi_Kali_Xi_min_X , Total_Frekuensi = Tfi, Standar_Deviasi = SB)
