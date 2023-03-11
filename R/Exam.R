# This is for the Exam

# # Data Wrangling
# ddt[ddt$RIVER == "TRM" & ddt$LENGTH == 52,]
# ddt[ddt$MILE == 5,]
# ddt[ddt$SPECIES == "CCATFISH" & ddt$WEIGHT > 1700,]
# ddt[ddt$SPECIES == "LMBASS" & ddt$RIVER == "TRM" & ddt$LENGTH == 36,]
library(readxl)
mtbe <- read.csv("MTBE.csv")
epagas <- read.csv("EPAGAS.csv")
ddt <- read.csv("DDT-1.csv")
head(ddt)
# head(mtbe)
tab <- table(mtbe$WellClass, mtbe$Aquifier)
tab <- table(mtbe$WellClass, mtbe$MTBE.Detect)
addmargins(tab)
tab
98/120
120/223
81/223

e = epagas$MPG
z1 = (e - mean(e)) / sd(e)
z1
ddtnew <- within(epagas , Z <- length(e[abs(z1) < 3]) / length(e))
e = ddtnew
exacttwo = e[abs(z1) >= 2 & abs(z1) <= 3]
exacttwo
y = length(e[abs(z1) < 1]) / length(e)
y


3/8
pnorm(13,12,4)-pnorm(8,12,4)
(3/9) * (80/3)
