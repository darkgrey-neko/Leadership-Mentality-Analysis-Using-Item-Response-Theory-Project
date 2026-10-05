# Mengatur direktori kerja
setwd("C:/Users/EliteBook/Downloads")  # Ubah sesuai lokasi file Anda

# Impor data
library(readxl)
data <- read_excel("Data_IRT_(Uji_Coba)_(1).xlsx", sheet = "data sudah di klasifikasi dikot")


# Pilih kolom yang relevan (menghapus kolom non-skala likert)
data_skor <- data[, 6:ncol(data)] # Sesuaikan indeks sesuai lokasi kolom skala likert

install.packages("stats4")
library(stats4)
install.packages("lattice")
library(lattice)
install.packages("mirt")
library(mirt)
library(psych)
## unidimensi
dimtest_result <- unidim(data_skor)
print(dimtest_result)

## independesi lokal
# Model 1PL atau 2PL
model <- mirt(data_skor, 1, itemtype = "Rasch")  

# Evaluasi residual
residuals <- residuals(model)
summary(residuals)
# Menghitung korelasi antar item
cor_matrix <- cor(data_skor)
print(cor_matrix)

##validitas dan reliabilitas
# Hitung validitas (korelasi item-total)
# Korelasi antara setiap item dengan skor total (tanpa menyertakan item itu sendiri dalam skor total)
item_total_corr <- apply(data_skor, 2, function(x) cor(x, rowSums(data_skor) - x, use = "complete.obs"))
print("Validitas Item (Korelasi Item-Total):")
print(item_total_corr)

# Hitung reliabilitas (Cronbach's Alpha)
reliability_alpha <- psych::alpha(data_skor)
print("Reliabilitas (Cronbach's Alpha):")
print(reliability_alpha$total$raw_alpha) # Hasil Alpha







# Install and load the necessary package
if (!require(ltm)) install.packages("ltm")
library(ltm)

# Input Data: Replace 'df_responses' with your actual dataset (binary responses: 0/1)
# Ensure your data is in a dataframe format with rows as respondents and columns as items

# 1PL Model (Rasch Model)
model_1pl <- rasch(data_skor)
print("1PL Model Summary:")
summary(model_1pl)

# 2PL Model
model_2pl <- ltm(data_skor ~ z1)
print("2PL Model Summary:")
summary(model_2pl)

# Compare Models (Model Fit Statistics: AIC and BIC)
aic_1pl <- AIC(model_1pl)
bic_1pl <- BIC(model_1pl)
aic_2pl <- AIC(model_2pl)
bic_2pl <- BIC(model_2pl)

print("Model Comparison (AIC and BIC):")
cat("1PL Model - AIC:", aic_1pl, "BIC:", bic_1pl, "\n")
cat("2PL Model - AIC:", aic_2pl, "BIC:", bic_2pl, "\n")

# Plot Item Characteristic Curves (ICC)
par(mfrow = c(1, 2))
plot(model_1pl, main = "1PL Model ICC", xlab = "Ability", ylab = "Probability")
plot(model_2pl, main = "2PL Model ICC", xlab = "Ability", ylab = "Probability")



mod_2pl <- mirt(data_skor, model = 1 , itemtype = "2PL")
coef(mod_2pl)

group1<- data_skor[data_skor == 'kelompok1', -ncol(data_skor)]
group2<- data_skor[data_skor == 'kelompok2', -ncol(data_skor)]
