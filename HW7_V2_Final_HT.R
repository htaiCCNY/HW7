##  HW 7 
##  Holli Tai 

###  In this exercise, I attempted to find a correlation of vaxx status, income, and onsite employment status, among other domographical variables

### I created a subset of households making between 35-49K, which is slightly over the NYC average total household income. 

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 


table(Household_Pulse_data$vaxx,Household_Pulse_data$Works_onsite)

summary(Household_Pulse_data$vaxx)

summary(as.numeric(Household_Pulse_data$vaxx))

vaxx_factor <- as.factor(Household_Pulse_data$vaxx)

levels(vaxx_factor)

levels(vaxx_factor) <- c("no","yes")

w_onsite <- (Household_Pulse_data$Works_onsite)


glm(vaxx_factor ~ w_onsite , family = binomial)

INC_v1 <- (Household_Pulse_data$INCOME == "HH income $35k - 49.9") 
dat_use2 <- subset(Household_Pulse_data, INC_v1)


dat_use1$RECVDVACC <- droplevels(dat_use2$RECVDVACC) 


model_logitX <- glm(vaxx ~ Works_onsite + ANXIOUS + RRACE + SEXUAL_ORIENTATION + GENID_DESCRIBE,
                    family = binomial, data = dat_use2)

summary(model_logitX)

install.packages("jtools")
summ(model_logitX)
library(jtools)
fit <-model_logitX 

summ(fit)


