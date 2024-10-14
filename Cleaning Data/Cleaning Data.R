# Install dan load library yang diperlukan
install.packages("DT")
library(tidyverse)
library(DT)

# Membaca data dummy dari CSV
dummy_data_wide <- read.csv("Newwdummy_data_wide.csv")

# Preview data
datatable(dummy_data_wide)
colnames(dummy_data_wide)

#Wide Data
dummy_data_wide <- dummy_data_wide %>%
  mutate(
    Usia = as.character(Usia),
    Penghasilan = as.character(Penghasilan)
  )

#Mengubah data wide menjadi data long
dummy_data_long <- dummy_data_wide %>%
  pivot_longer(
    cols = c(JenisKelamin, Usia, StatusdenganAtasan, Penghasilan),
    names_to = "Variable",
    values_to = "Value"
  )

datatable(dummy_data_long)

# Simpan hasil dalam file long table ke dalam CSV
write.csv(dummy_data_long, "dummy_data_long_generated.csv", row.names = FALSE)

#Cek Missing value pada Data
missing_values <- dummy_data_long %>%
  filter(is.na(Value)) %>%
  select(Variable, Value)

# Tampilkan missing values jika ada
if (nrow(missing_values) > 0) {
  print("Missing Values:")
  print(missing_values)
} else {
  print("No missing data.")
}

#Nilai Mean
mean_values <- dummy_data_long %>%
  filter(Variable %in% c("Usia", "Penghasilan")) %>%
  group_by(Variable) %>%
  summarise(mean_value = mean(as.numeric(Value), na.rm = TRUE), .groups = 'drop')

#input missing value pada variabel Numerik
# Impute missing values for numeric variables
dummy_data_clean_numeric <- dummy_data_long %>%
  left_join(mean_values, by = "Variable") %>%
  mutate(Value = case_when(
    is.na(Value) & Variable %in% c("Usia", "Penghasilan") & !is.na(mean_value) ~ as.character(mean_value),  # Imputasi untuk variabel numerik sebagai karakter
    TRUE ~ Value
  )) %>%
  select(-mean_value)  # Hapus kolom mean_value

# Imputasi modus untuk variabel non-numerik (seperti JenisKelamin)
modus_values <- dummy_data_long %>%
  group_by(Variable) %>%
  summarise(modus_value = as.character(stats::na.omit(Value)[which.max(table(Value))]), .groups = 'drop')

dummy_data_clean <- dummy_data_clean_numeric %>%
  left_join(modus_values, by = "Variable") %>%
  mutate(Value = case_when(
    is.na(Value) & Variable == "JenisKelamin" ~ modus_value,  # Imputasi dengan modus untuk variabel karakter
    TRUE ~ Value
  )) %>%
  select(-modus_value)  # Hapus kolom modus_value

# Pastikan kolom Value menjadi karakter untuk konsistensi
dummy_data_clean <- dummy_data_clean %>%
  mutate(Value = as.character(Value))

# Tampilkan tabel hasil bersih
datatable(dummy_data_clean)

# Check for missing values in the cleaned data
missing_data <- dummy_data_clean %>%
  filter(is.na(Value)) %>%
  select(Variable, Value)

# Print the rows with missing values
if(nrow(missing_data) > 0) {
  print("Missing Data:")
  print(missing_data)
} else {
  print("No missing data.")
}


# Identifikasi data tidak konsisten
inconsistent_gender <- dummy_data_clean %>%
  filter(!(Value %in% c("Laki-laki", "Perempuan")) & Variable == "JenisKelamin")

# Tampilkan data yang tidak konsisten
if(nrow(inconsistent_gender) > 0) {
  print("Data JenisKelamin yang Tidak Konsisten:")
  print(inconsistent_gender)
  
  # Perbaiki data yang tidak konsisten
  dummy_data_clean <- dummy_data_clean %>%
    mutate(Value = case_when(
      Variable == "JenisKelamin" & Value == "Unknown" ~ NA_character_,  # Mengubah "Unknown" menjadi NA
      TRUE ~ Value
    ))
} else {
  print("Semua data JenisKelamin konsisten.")
}

# Hitung mode dari JenisKelamin
mode_gender <- dummy_data_clean %>%
  filter(Variable == "JenisKelamin" & !is.na(Value)) %>%
  group_by(Value) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Value)

# Perbaiki data yang tidak konsisten
dummy_data_clean <- dummy_data_clean %>%
  mutate(Value = case_when(
    Variable == "JenisKelamin" & Value == "Unknown" ~ mode_gender,  # Mengubah "Unknown" menjadi mode
    TRUE ~ Value
  ))

# Melihat nilai unik di kolom JenisKelamin
unique_values <- dummy_data_clean %>%
  filter(Variable == "JenisKelamin") %>%
  pull(Value) %>%
  unique()

print(unique_values)

# Identifikasi nilai ekstrim untuk Usia dan Penghasilan
outlier_check <- dummy_data_clean %>%
  filter(Variable %in% c("Usia", "Penghasilan")) %>%
  group_by(Variable) %>%
  summarise(
    Q1 = quantile(as.numeric(Value), 0.25, na.rm = TRUE),
    Q3 = quantile(as.numeric(Value), 0.75, na.rm = TRUE),
    IQR = IQR(as.numeric(Value), na.rm = TRUE),
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    .groups = 'drop'
  )

# Menampilkan nilai ekstrim
outliers <- dummy_data_clean %>%
  filter(Variable %in% c("Usia", "Penghasilan")) %>%
  left_join(outlier_check, by = "Variable") %>%
  filter(as.numeric(Value) < lower_bound | as.numeric(Value) > upper_bound)


# Tampilkan nilai ekstrim
if(nrow(outliers) > 0) {
  print("Nilai Ekstrim Ditemukan:")
  print(outliers)
  
  # Keputusan untuk menghapus atau melakukan imputasi
  # Contoh: menghapus outliers
  dummy_data_clean <- dummy_data_clean %>%
    filter(!(Variable %in% c("Usia", "Penghasilan") & Value %in% outliers$Value))
  
  # Atau, untuk imputasi,digunakan mean:
  mean_values_outliers <- dummy_data_clean %>%
    filter(Variable %in% c("Usia", "Penghasilan") & !is.na(Value)) %>%
    group_by(Variable) %>%
    summarise(mean_value = mean(as.numeric(Value), na.rm = TRUE), .groups = 'drop')
  
  # Imputasi nilai ekstrim dengan mean
  dummy_data_clean <- dummy_data_clean %>%
    left_join(outlier_check, by = "Variable") %>%
    mutate(Value = case_when(
      (Variable %in% c("Usia", "Penghasilan") & as.numeric(Value) < lower_bound) ~ as.character(mean_value),
      (Variable %in% c("Usia", "Penghasilan") & as.numeric(Value) > upper_bound) ~ as.character(mean_value),
      TRUE ~ Value
    )) %>%
    select(-mean_value, -lower_bound, -upper_bound)  # Hapus kolom yang tidak diperlukan
  
} else {
  print("Tidak ada nilai ekstrim yang ditemukan.")
}



