#2415091038_I Kadek Ello Dwi Permana
library(readxl)
library(ggplot2)
library(car)
library(carData)
data <- read_excel("C:/Users/nowja/OneDrive/Documents/KULIAH/statistik/uas/data siswa.xlsx")

data$Kelompok <- as.factor(data$Kelompok)

# Uji Asumsi
# 1. Uji normalitas
normality_tests <- by(data$Nilai_Ujian, data$Kelompok, shapiro.test)
print(normality_tests)

# 2. Uji homogenitas varians
levene_test <- leveneTest(Nilai_Ujian ~ Kelompok, data = data)
print(levene_test)

# 3. Uji independensi
durbinWatsonTest(aov(Nilai_Ujian ~ Kelompok, data = data))

# Analisis ANOVA
anova_result <- aov(Nilai_Ujian ~ Kelompok, data = data)
summary(anova_result)

# Visualisasi
p <- ggplot(data, aes(x = Kelompok, y = Nilai_Ujian, fill = Kelompok)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot Nilai Ujian per Kelompok", x = "Kelompok", y = "Nilai Ujian")
print(p)

# Interpretasi
groups <- TukeyHSD(anova_result)
print(groups)
