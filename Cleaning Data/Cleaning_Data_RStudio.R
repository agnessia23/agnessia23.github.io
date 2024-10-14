# Install and load necessary packages
install.packages("tidyverse")
install.packages("zoo")
install.packages("lubridate")  # Untuk memanipulasi tanggal

library(tidyverse)
library(zoo)
library(lubridate)

# Mengupload file CSV dari laptop
setwd("Downloads")  # Gantilah dengan path file CSV di laptop Anda
weather_data <- read.csv("Data-iklim-Sentani-1994-2023.csv", stringsAsFactors = TRUE)

# Menampilkan beberapa baris pertama dari data untuk memastikan file terupload dengan benar
head(weather_data)

# 3. Mengonversi Kolom Tanggal ke Format Date yang Tepat
weather_data$Tanggal <- dmy(weather_data$X.Tanggal)  # Mengonversi ke format tanggal

# 4. Membuat Kolom Baru untuk Bulan dan Tahun
weather_data$Year <- year(weather_data$Tanggal)
weather_data$Month <- month(weather_data$Tanggal, label = TRUE, abbr = TRUE)

# 5. Mengecek duplikat dan menghapus baris duplikat jika ditemukan
weather_data <- weather_data[!duplicated(weather_data), ]

# 6. Mengganti nilai 8888 dan 9999 di kolom Curah Hujan dengan NA
weather_data$Curah_Hujan <- ifelse(weather_data$Curah_Hujan == 8888 | weather_data$Curah_Hujan == 9999, NA, weather_data$Curah_Hujan)

# Mengganti nilai-nilai besar di kolom-kolom lain (jika ada nilai serupa) menjadi NA
weather_data$Temperatur_Minimum <- ifelse(weather_data$Temperatur_Minimum == 8888 | weather_data$Temperatur_Minimum == 9999, NA, weather_data$Temperatur_Minimum)
weather_data$Temperatur_Maksimum <- ifelse(weather_data$Temperatur_Maksimum == 8888 | weather_data$Temperatur_Maksimum == 9999, NA, weather_data$Temperatur_Maksimum)
weather_data$Temperatur_Ratarata <- ifelse(weather_data$Temperatur_Ratarata == 8888 | weather_data$Temperatur_Ratarata == 9999, NA, weather_data$Temperatur_Ratarata)
weather_data$Kelembapan_Ratarata <- ifelse(weather_data$Kelembapan_Ratarata == 8888 | weather_data$Kelembapan_Ratarata == 9999, NA, weather_data$Kelembapan_Ratarata)

# 7. Mengisi nilai NA dengan interpolasi linier untuk kolom numerik
weather_data <- weather_data %>%
  mutate(
    Curah_Hujan = na.approx(Curah_Hujan, na.rm = FALSE),
    Temperatur_Minimum = na.approx(Temperatur_Minimum, na.rm = FALSE),
    Temperatur_Maksimum = na.approx(Temperatur_Maksimum, na.rm = FALSE),
    Temperatur_Ratarata = na.approx(Temperatur_Ratarata, na.rm = FALSE),
    Kelembapan_Ratarata = na.approx(Kelembapan_Ratarata, na.rm = FALSE),
  )

# 8. Menyimpan data yang telah dibersihkan ke file CSV baru
write.csv(weather_data, "cleaned_weather_data_fixed2.csv", row.names = FALSE)

# Menampilkan beberapa baris pertama dari data untuk memastikan semuanya berfungsi dengan baik
head(weather_data)
