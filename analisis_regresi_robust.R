setwd("C:/Users/Sheilla Khairunnisa/Documents/kapita selekta statistika")
getwd()

library(readxl)
data = read_excel("data_pendidikan_2024.xlsx")
View(data)

data$`Persentase Siswa Bekerja (Y)` = as.numeric(gsub(",", ".", data$`Persentase Siswa Bekerja (Y)`))
data$`Angka Partisipasi Sekolah Umur 16-18  (%)` = as.numeric(gsub(",", ".", data$`Angka Partisipasi Sekolah Umur 16-18  (%)`))
data$`Angka Partisipasi Murni SMA (%)` = as.numeric(gsub(",", ".", data$`Angka Partisipasi Murni SMA (%)`))
data$`Penggunaan Internet (%)` = as.numeric(gsub(",", ".", data$`Penggunaan Internet (%)`))
data$`Rata-rata Lama Sekolah (tahun)` = as.numeric(gsub(",", ".", data$`Rata-rata Lama Sekolah (tahun)`))

colnames(data)
colnames(data) = c("Provinsi", "Y", "APS", "APM", "Internet", "RLS")

data_summary = data.frame(
  Variabel = c("Y", "APS", "APM", "Internet", "RLS"),
  Min = c(min(data$Y), min(data$APS), min(data$APM), min(data$Internet), min(data$RLS)),
  Max = c(max(data$Y), max(data$APS), max(data$APM), max(data$Internet), max(data$RLS)),
  Mean = c(mean(data$Y), mean(data$APS), mean(data$APM), mean(data$Internet), mean(data$RLS)),
  Std_Dev = c(sd(data$Y), sd(data$APS), sd(data$APM), sd(data$Internet), sd(data$RLS))
)
data_summary

library(MASS)
model = rlm(Y ~ APS + APM + Internet + RLS, data=data)
summary(model)

boxplot(data[,2:6],
        main="Boxplot Variabel Penelitian",
        ylab="Nilai",
        col="lightblue")

