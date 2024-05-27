# ###################################################
# RCBD single location analysis - multiple traits 
#####################################################
# Load packages to be used in the analysis

library(lme4)
library(emmeans)
library(tibble)
library(multcomp)
# Set the working directory 

setwd("/Users/tas286/Documents/R_Training_videos_for_Post")

#1. Import the data

dat <- read.csv(file = "RCBD_single_loc.csv", header = TRUE, sep = ",")
summary(dat)
#2. Change the factors into factor variables 
dat$rep <- as.factor(dat$rep)
dat$Genotype <- as.factor(dat$Genotype)

#3. list the traits to be analyzed 
Traits <- c("DH","DM","PHMean","TKW", "GYH")
#4. check the structure of the data
str(dat)
#5. Define the model 
# yij = m + block + genotype + eij 

#6. Create the tibble to store the output 

BLUE_mean <- tibble() # to store the output from the analysis

for(Trait in Traits){

eval(parse(text = paste(" av <- lmer(formula = ",Trait," ~ Genotype + (1|rep), data = dat)")))

em <- as.data.frame(cld(lsmeans(av, specs = "Genotype")))
em$Trait <- Trait
BLUE_mean <- rbind(BLUE_mean,em)
}

#Visualize the BLUE mean as genotype x Trait 
BLUE_mean
library(reshape2)

BLUE_mean_wide <- dcast(data = BLUE_mean, formula = Genotype ~ Trait, 
                        fun.aggregate = mean, value.var = "lsmean")
head(BLUE_mean_wide)

write.csv(x = BLUE_mean_wide, file = "BLUEmean_wide.csv", row.names = FALSE)

out <- list(single_trait = BLUE_mean, BLUWMean_wide = BLUE_mean_wide)

library(openxlsx)
write.xlsx(x = out, file = "Summary_BLUE_mean_Bekoji.xlsx",overwrite = T, rowNames = F)
#END
