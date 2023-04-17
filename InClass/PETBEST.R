library(dplyr)
pet <- Intro2R::myreadxl()$PETBOTTLE
names(pet)
pet200 <- pet %>% filter(STRENGTH > 200)
set.seed(32)
old <- pet200 %>% filter(DESIGN == "OLD") %>% sample_frac(size = 0.1)
new <- pet200 %>% filter(DESIGN == "NEW") %>% sample_frac(size = 0.1)
new

library(Jabs)
chains <- BESTmcmc(new$STRENGTH,old$STRENGTH)
windows();plot(chains)
summary(chains)
t.test(new$STRENGTH,old$STRENGTH)$conf.int
