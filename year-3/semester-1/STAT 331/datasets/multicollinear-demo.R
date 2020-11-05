## Coffee example  (Coffee Quality Institute, 2018) continued
coffee <- read.csv("coffee_arabica.csv")

cor(coffee) # doesn't work as there's a categorical variable
cor(coffee[,-1]) # e.g., remove first column
pairs(coffee[,-1])
pairs(~ Flavor + Aroma + Aftertaste + Body + 
        Acidity + Balance + Sweetness + Uniformity + Moisture, data=coffee)

# Code our own indicators, so that we can more easily interpret VIFs
coffee$wet <- ifelse(coffee$Processing.Method == 'Washed / Wet', 1, 0) # 1 = wet, 0 otherwise
coffee$semi <- ifelse(coffee$Processing.Method == 'Semi-washed / Semi-pulped', 1, 0) # 1 = semi/dry, 0 otherwise

mfull <- lm(Flavor~ wet + semi + Aroma + Aftertaste + 
      Body + Acidity + Balance + Sweetness + Uniformity + Moisture, dat=coffee)

wet_reg <- lm(wet ~ semi + Aroma + Aftertaste + 
                Body + Acidity + Balance + Sweetness + Uniformity + Moisture, dat=coffee)
r2_wet <- summary(wet_reg)$r.squared
VIF_wet <- 1 / (1 - r2_wet)

Aroma_reg <- lm(Aroma ~ wet + semi + Aftertaste + 
      Body + Acidity + Balance + Sweetness + Uniformity + Moisture, dat=coffee)
r2_Aroma <- summary(Aroma_reg)$r.squared
VIF_Aroma <- 1 / (1 - r2_Aroma)

Aftertaste_reg <- lm(Aftertaste ~ wet + semi + Aroma + 
       Body + Acidity + Balance + Sweetness + Uniformity + Moisture, dat=coffee)
r2_Aftertaste <- summary(Aftertaste_reg)$r.squared
VIF_Aftertaste <- 1 / (1 - r2_Aftertaste)

library(car)
# install.packages("car")  if you don't have the library
vif(mfull) # vif function in the "car" library

## Python in FL everglades example (2017)
## Sex, length, total mass, fat mass, and specimen condition data for 
## 248 Burmese pythons (Python bivittatus) collected in the Florida Everglades

python <- read.csv("FLpython.csv")
head(python)
python$male <- ifelse(python$sex == 'M', 1, 0) # 1 = M, 0 =F

cor(python[,-1])
pairs(python[,-1])
mpf <- lm(fat ~ male+svl+mass+length, data = python)
summary(mpf)
vif(mpf)

# remove "length" based on VIF
mpf2 <- lm(fat ~ male+mass+svl, data = python)
summary(mpf2)
vif(mpf2)
