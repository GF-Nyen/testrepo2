
PhotoPoint <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

head(PhotoPoint)

for(i in 1:nrow(PhotoPoint)){
  if(PhotoPoint$stand[i]=='6'|PhotoPoint$stand[i]=='7'|PhotoPoint$stand[i]=='8'|PhotoPoint$stand[i]=='9'){
    PhotoPoint$Treatment[i]=1} # seeeded
  else{PhotoPoint$Treatment[i]=0} # not seeded
  }

PhotoPoint <-  PhotoPoint[complete.cases(PhotoPoint$species_code),]

PhotoPoint <- PhotoPoint[complete.cases(PhotoPoint$row),]

unique(PhotoPoint$plot_id)

library("plyr")

PhotoPoint <- ddply(PhotoPoint, .(plot_id), function(x) x[c(nrow(x)),])

dim(PhotoPoint)

#####

Regen <- PhotoPoint[,c('plot_id','stand','Treatment','row','n_seedlings','gap_fraction')]

for(i in 1:nrow(Regen)){
  if(Regen$n_seedlings[i]==0){
    Regen$Presence[i]=0}
  else{Regen$Presence[i]=1}
  }

str(Regen)

Regen$Presence <- factor(Regen$Presence)

Regen$plot_id <- factor(Regen$plot_id)
Regen$stand <- factor(Regen$stand)
Regen$Treatment <- factor(Regen$Treatment)
Regen$row <- factor(Regen$row)

library(MASS)

# We must accounted for the nested structure (i.e., stands within seeded
# treatment):

PQL_1 <- glmmPQL(Presence ~ gap_fraction + Treatment,
  random = ~1 | Treatment/stand, family=binomial,
  data=Regen)

summary(PQL_1)
# I am not sure why the NaNs are produced for the p-value

# A less ideal model because we are not accounting for the nested structure:

PQL_A <- glmmPQL(Presence ~ gap_fraction + Treatment,
  random = ~1 | stand, family=binomial,
  data=Regen)

summary(PQL_A)

# When Treatment is not included as a fixed effect, the model runs okay:

PQL_B <- glmmPQL(Presence ~ gap_fraction,
  random = ~1 | Treatment/stand, family=binomial,
  data=Regen)

summary(PQL_B)

# Consider the clog-log link when there are considerably more zeros than
# ones!

PQL_2 <- glmmPQL(Presence ~ gap_fraction,
  random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
  data=Regen)

summary(PQL_2)

#############################################################################

install.packages("glmmADMB", repos="http://r-forge.r-project.org")

install.packages("glmmADMB", repos="http://R-Forge.R-project.org")

install.packages("R2admb")

install.packages("glmmADMB", 
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")

#############################################################################

install.packages("devtools")
install.packages("R2admb")
devtools::install_github("bbolker/glmmadmb")
library(devtools)
library(R2admb)
library(bbolker)
library(glmmADMB)
install.packages("glmmADMB", repos = "http://glmmadmb.r-forge.r-project.org/repos")

#############################################################################

# FOR COUNT DATA !!! So for regen, not for models of biomass.

# This follows a ZAP and ZAB approach, Alias Hurdle Models, PLUS ...
# a function that can handle random effects!

# In my opinion ZAP and ZAB models are the way to go (in comparison to
# ZIP and ZINB) because we are assuming perfect detection. That is, we
# did a quality job of searching for seedlings within sampling frames.

# glmmadmb seems to be able to test for a treatment effect:

fit_binary <- glmmadmb(Presence ~ gap_fraction + Treatment,
  random = ~1 | Treatment/stand, family='binomial', link='cloglog',
  data=Regen)

summary(fit_binary) # !!!

fit_hpoiss <- glmmadmb(n_seedlings ~ gap_fraction + Treatment,
  random = ~1 | Treatment/stand,
  data = subset(Regen, n_seedlings > 0),
  family = "truncpoiss")
  
summary(fit_hpoiss) # !!!
  
fit_hnbinom1 <- glmmadmb(n_seedlings ~ gap_fraction + Treatment,
  random = ~1 | Treatment/stand,
  data = subset(Regen, n_seedlings > 0),
  family = "truncnbinom1")
  
summary(fit_hnbinom1) # Pr(>|z|) for Treatment1 is not signficant

# Items to consider:
boxplot(n_seedlings ~ stand, data=Regen)
# For the most part, regen #s are greater for the non-seeded stands than
# for the seeded stands.

plot(n_seedlings ~ gap_fraction, data=Regen)
# The sweet spot you talk about. Is it just because we have so many
# observations for mid range values of gap fraction that that is why
# we are detecting regeneration there. Something to very much consider
# before you put too much emphasis on this.

# Try other explanatory variables for light in these models. Try biomass
# of wiregrass, etc. as explantatory variables. However, wiregrass may be
# correlated with Treatment, so we'll get the same results.

# So, what is it about the stands or no seeding treatment that is causing the
# regen in stands 3, 4, and 5, especially in stand 5? Did all the additional
# disturbance (sowing of seed) in the seeded stands somehow influence regen?
# Burning characteristics within individual stands?

###############################################################################

Biomass <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

unique(Biomass$species_code)

Biomass <- Biomass[complete.cases(Biomass$species_code),]

Biomass <- Biomass[complete.cases(Biomass$row),]

ARBE <- Biomass[Biomass$species_code=='ARBE',]

ARBE

ARBE_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], ARBE[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(ARBE_Biomass)

ARBE_Biomass$dried_weight_g[is.na(ARBE_Biomass$dried_weight_g)] <- 0

ARBE_Biomass

for(i in 1:nrow(ARBE_Biomass)){
  if(ARBE_Biomass$dried_weight_g[i]==0){
    ARBE_Biomass$Presence[i]=0}
  else{ARBE_Biomass$Presence[i]=1}
  }

str(ARBE_Biomass)

ARBE_Biomass$Presence <- factor(ARBE_Biomass$Presence)

ARBE_Biomass$plot_id <- factor(ARBE_Biomass$plot_id)
ARBE_Biomass$stand <- factor(ARBE_Biomass$stand)
ARBE_Biomass$Treatment <- factor(ARBE_Biomass$Treatment)
ARBE_Biomass$Row <- factor(ARBE_Biomass$row)

# I believe stand 8 (the southern most stand just south of RT 91) had a
# supplemental seeding of wiregrass. That could influence our findings!

boxplot(dried_weight_g ~ stand, data=ARBE_Biomass)

# Wiregrass in seeded stands only!!!

# ALSO:

boxplot(dried_weight_g ~ Row, data=ARBE_Biomass)

# Wiregrass in take out rows only!!!

# Not spreading into other areas.

# First, let's check presence-absence of wiregrass:

PQL_1 <- glmmPQL(Presence ~ gap_fraction,
  random = ~1 | Treatment/stand, family=binomial,
  data=ARBE_Biomass)

summary(PQL_1)

PQL_2 <- glmmPQL(Presence ~ gap_fraction,
  random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
  data=ARBE_Biomass)
  
summary(PQL_2)

fit_binary <- glmmadmb(Presence ~ Treatment,
  random = ~1 | Treatment/stand, family='binomial', link='cloglog',
  data=ARBE_Biomass)

summary(fit_binary) 
  
# For wiregrass biomass:

hist(ARBE_Biomass$dried_weight_g, breaks=20)

# I think the large number of 0 values is going to cause some issues with
# modeling the CONTINUOUS variable biomass. For some species, a transformation
# could be applied, but that may involve adding a small value to all values
# first. In the case of wiregrass, in appears to be presence-absence is the
# way to go, or model biomass without the zero values.

hist(ARBE_Biomass[ARBE_Biomass$dried_weight_g > 0,]$dried_weight_g, breaks=20)

library(nlme)

boxplot(E2~Row, data=ARBE_Biomass[ARBE_Biomass$dried_weight_g > 0,])

# Again, wiregrass only occurs in rows, so we cannot include it in models
# of biomass; contrasts for 2 or more levels in the data.

LME_M1 <- lme(dried_weight_g ~ gap_fraction,
  random = ~1|Treatment/stand,
  data=ARBE_Biomass[ARBE_Biomass$dried_weight_g > 0,])

E2 <- resid(LME_M1, type="normalized")
F2 <- fitted(LME_M1)
plot(E2~gap_fraction, data=ARBE_Biomass[ARBE_Biomass$dried_weight_g > 0,])

summary(LME_M1)

# I am not sure we will find anything statistically significant for
# wiregrass biomass.

###############################################################################

AN01 <- Biomass[Biomass$species_code=='AN01',]

AN01

AN01_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], AN01[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(AN01_Biomass)

AN01_Biomass$dried_weight_g[is.na(AN01_Biomass$dried_weight_g)] <- 0

AN01_Biomass

for(i in 1:nrow(AN01_Biomass)){
  if(AN01_Biomass$dried_weight_g[i]==0){
    AN01_Biomass$Presence[i]=0}
  else{AN01_Biomass$Presence[i]=1}
  }

str(AN01_Biomass)

AN01_Biomass$Presence <- factor(AN01_Biomass$Presence)

AN01_Biomass$plot_id <- factor(AN01_Biomass$plot_id)
AN01_Biomass$stand <- factor(AN01_Biomass$stand)
AN01_Biomass$Treatment <- factor(AN01_Biomass$Treatment)
AN01_Biomass$Row <- factor(AN01_Biomass$row)

AN01_Biomass

boxplot(dried_weight_g ~ stand, data=AN01_Biomass)
boxplot(dried_weight_g ~ Row, data=AN01_Biomass)

PQL_1 <- glmmPQL(Presence ~ gap_fraction + Row,
  random = ~1 | Treatment/stand, family=binomial,
  data=AN01_Biomass)

summary(PQL_1)

PQL_2 <- glmmPQL(Presence ~ gap_fraction + Row,
  random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
  data=AN01_Biomass)

summary(PQL_2)

fit_binary <- glmmadmb(Presence ~ Treatment,
  random = ~1 | Treatment/stand, family='binomial', link='cloglog',
  data=AN01_Biomass)

summary(fit_binary) 

# No statistically significant results for gap fraction, seeded treatment,
# and row. What about other variables? What else might be infleuncing AN01? 

###############################################################################

SONU <- Biomass[Biomass$species_code=='SONU',]

SONU

SOSE <- Biomass[Biomass$species_code=='SOSE',]

SOSE

PAVI <- Biomass[Biomass$species_code=='PAVI',]

PAVI

TEVI <- Biomass[Biomass$species_code=='TEVI',]

TEVI


#############GABE WORKING ON OTHER SPECIES############################################################
Biomass <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

unique(Biomass$species_code)

Biomass <- Biomass[complete.cases(Biomass$species_code),]

Biomass <- Biomass[complete.cases(Biomass$row),]

SONU <- Biomass[Biomass$species_code=='SONU',]
SONU

SONU_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], SONU[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(SONU_Biomass)

SONU_Biomass$dried_weight_g[is.na(SONU_Biomass$dried_weight_g)] <- 0

SONU_Biomass

for(i in 1:nrow(SONU_Biomass)){
  if(SONU_Biomass$dried_weight_g[i]==0){
    SONU_Biomass$Presence[i]=0}
  else{SONU_Biomass$Presence[i]=1}
}

str(SONU_Biomass)

SONU_Biomass$Presence <- factor(SONU_Biomass$Presence)

SONU_Biomass$plot_id <- factor(SONU_Biomass$plot_id)
SONU_Biomass$stand <- factor(SONU_Biomass$stand)
SONU_Biomass$Treatment <- factor(SONU_Biomass$Treatment)
SONU_Biomass$Row <- factor(SONU_Biomass$row)

# I believe stand 8 (the southern most stand just south of RT 91) had a
# supplemental seeding of wiregrass. That could influence our findings!

boxplot(dried_weight_g ~ stand, data=SONU_Biomass)

# Wiregrass in seeded stands only!!!

# ALSO:

boxplot(dried_weight_g ~ Row, data=SONU_Biomass)

# Wiregrass in take out rows only!!!

# Not spreading into other areas.

# First, let's check presence-absence of wiregrass:

SONU_PQL_1 <- glmmPQL(Presence ~ gap_fraction,
                 random = ~1 | Treatment/stand, family=binomial,
                 data=SONU_Biomass)

summary(SONU_PQL_1) # ! ! !

SONU_PQL_2 <- glmmPQL(Presence ~ gap_fraction,
                 random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
                 data=SONU_Biomass)

summary(SONU_PQL_2) # ! ! !

SONU_fit_binary <- glmmadmb(Presence ~ Treatment,
                       random = ~1 | Treatment/stand, family='binomial', link='cloglog',
                       data=SONU_Biomass)

summary(SONU_fit_binary) # ! ! ! ! ! ! !

# For YELLOW INDIAN GRASS (SONU) biomass:

hist(SONU_Biomass$dried_weight_g, breaks=20)

# I think the large number of 0 values is going to cause some issues with
# modeling the CONTINUOUS variable biomass. For some species, a transformation
# could be applied, but that may involve adding a small value to all values
# first. In the case of wiregrass, in appears to be presence-absence is the
# way to go, or model biomass without the zero values.

hist(SONU_Biomass[SONU_Biomass$dried_weight_g > 0,]$dried_weight_g, breaks=20)

library(nlme)

boxplot(E2~Row, data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

# Again, wiregrass only occurs in rows, so we cannot include it in models
# of biomass; contrasts for 2 or more levels in the data.

SONU_LME_M1 <- lme(dried_weight_g ~ gap_fraction,
              random = ~1|Treatment/stand,
              data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

E2 <- resid(SONU_LME_M1, type="normalized")
F2 <- fitted(SONU_LME_M1)
plot(E2~gap_fraction, data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

summary(SONU_LME_M1)

# I am not sure we will find anything statistically significant for
# wiregrass biomass.

############### TEVI GABE TESTING FOR GOATS RUE #################
Biomass <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

unique(Biomass$species_code)

Biomass <- Biomass[complete.cases(Biomass$species_code),]

Biomass <- Biomass[complete.cases(Biomass$row),]

TEVI <- Biomass[Biomass$species_code=='TEVI',]
TEVI

TEVI_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], TEVI[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(TEVI_Biomass)

TEVI_Biomass$dried_weight_g[is.na(TEVI_Biomass$dried_weight_g)] <- 0

TEVI_Biomass

for(i in 1:nrow(TEVI_Biomass)){
  if(TEVI_Biomass$dried_weight_g[i]==0){
    TEVI_Biomass$Presence[i]=0}
  else{TEVI_Biomass$Presence[i]=1}
}

str(TEVI_Biomass)

TEVI_Biomass$Presence <- factor(TEVI_Biomass$Presence)

TEVI_Biomass$plot_id <- factor(TEVI_Biomass$plot_id)
TEVI_Biomass$stand <- factor(TEVI_Biomass$stand)
TEVI_Biomass$Treatment <- factor(TEVI_Biomass$Treatment)
TEVI_Biomass$Row <- factor(TEVI_Biomass$row)

# I believe stand 8 (the southern most stand just south of RT 91) had a
# supplemental seeding of wiregrass. That could influence our findings!

boxplot(dried_weight_g ~ stand, data=TEVI_Biomass)

# Wiregrass in seeded stands only!!!

# ALSO:

boxplot(dried_weight_g ~ Row, data=TEVI_Biomass)

# Wiregrass in take out rows only!!!

# Not spreading into other areas.

# First, let's check presence-absence of wiregrass:

TEVI_PQL_1 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial,
                      data=TEVI_Biomass)

summary(TEVI_PQL_1) #

TEVI_PQL_2 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
                      data=TEVI_Biomass)

summary(TEVI_PQL_2) #

TEVI_fit_binary <- glmmadmb(Presence ~ Treatment,
                            random = ~1 | Treatment/stand, family='binomial', link='cloglog',
                            data=TEVI_Biomass)

summary(TEVI_fit_binary) #

# For TEVI - GOATS RUE biomass:

hist(TEVI_Biomass$dried_weight_g, breaks=20)

# I think the large number of 0 values is going to cause some issues with
# modeling the CONTINUOUS variable biomass. For some species, a transformation
# could be applied, but that may involve adding a small value to all values
# first. In the case of wiregrass, in appears to be presence-absence is the
# way to go, or model biomass without the zero values.

hist(TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,]$dried_weight_g, breaks=20)

library(nlme)

boxplot(E2~Row, data=TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,])

# Again, wiregrass only occurs in rows, so we cannot include it in models
# of biomass; contrasts for 2 or more levels in the data.

TEVI_LME_M1 <- lme(dried_weight_g ~ gap_fraction,
                   random = ~1|Treatment/stand,
                   data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

E2 <- resid(TEVI_LME_M1, type="normalized")
F2 <- fitted(TEVI_LME_M1)
plot(E2~gap_fraction, data=TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,])

summary(TEVI_LME_M1)

############### SOSE GABE TESTING FOR GOATS RUE #################
Biomass <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

unique(Biomass$species_code)

Biomass <- Biomass[complete.cases(Biomass$species_code),]

Biomass <- Biomass[complete.cases(Biomass$row),]

SOSE <- Biomass[Biomass$species_code=='SOSE',]
SOSE

SOSE_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], SOSE[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(SOSE_Biomass)

SOSE_Biomass$dried_weight_g[is.na(SOSE_Biomass$dried_weight_g)] <- 0

SOSE_Biomass

for(i in 1:nrow(SOSE_Biomass)){
  if(SOSE_Biomass$dried_weight_g[i]==0){
    SOSE_Biomass$Presence[i]=0}
  else{SOSE_Biomass$Presence[i]=1}
}

str(SOSE_Biomass)

SOSE_Biomass$Presence <- factor(SOSE_Biomass$Presence)

SOSE_Biomass$plot_id <- factor(SOSE_Biomass$plot_id)
SOSE_Biomass$stand <- factor(SOSE_Biomass$stand)
SOSE_Biomass$Treatment <- factor(SOSE_Biomass$Treatment)
SOSE_Biomass$Row <- factor(SOSE_Biomass$row)

# I believe stand 8 (the southern most stand just south of RT 91) had a
# supplemental seeding of wiregrass. That could influence our findings!

boxplot(dried_weight_g ~ stand, data=SOSE_Biomass)

# Wiregrass in seeded stands only!!!

# ALSO:

boxplot(dried_weight_g ~ Row, data=SOSE_Biomass)

# Wiregrass in take out rows only!!!

# Not spreading into other areas.

# First, let's check presence-absence of wiregrass:

SOSE_PQL_1 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial,
                      data=SOSE_Biomass)

summary(SOSE_PQL_1) #

SOSE_PQL_2 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
                      data=SOSE_Biomass)

summary(TEVI_PQL_2) #

SOSE_fit_binary <- glmmadmb(Presence ~ Treatment+ Row,
                            random = ~1 | Treatment/stand, family='binomial', link='cloglog',
                            data=SOSE_Biomass)

summary(SOSE_fit_binary) # !?! THE INTERCEPT IS SIG ?!?

# For TEVI - GOATS RUE biomass:

hist(SOSE_Biomass$dried_weight_g, breaks=20)

# I think the large number of 0 values is going to cause some issues with
# modeling the CONTINUOUS variable biomass. For some species, a transformation
# could be applied, but that may involve adding a small value to all values
# first. In the case of wiregrass, in appears to be presence-absence is the
# way to go, or model biomass without the zero values.

hist(SOSE_Biomass[SOSE_Biomass$dried_weight_g > 0,]$dried_weight_g, breaks=20)

library(nlme)

boxplot(E2~Row, data=SOSE_Biomass[SOSE_Biomass$dried_weight_g > 0,])

# Again, wiregrass only occurs in rows, so we cannot include it in models
# of biomass; contrasts for 2 or more levels in the data.

SOSE_LME_M1 <- lme(dried_weight_g ~ gap_fraction,
                   random = ~1|Treatment/stand,
                   data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

E2 <- resid(SOSE_LME_M1, type="normalized")
F2 <- fitted(SOSE_LME_M1)
plot(E2~gap_fraction, data=SOSE_Biomass[SOSE_Biomass$dried_weight_g > 0,])

summary(SOSE_LME_M1)

#######################Here do all seeded species together##################################################

Biomass <- read.csv("R:/graduate_students/Nyen/metadata/gn_metadata.csv", header=TRUE, na.strings="")

unique(Biomass$species_code)

Biomass <- Biomass[complete.cases(Biomass$species_code),]

Biomass <- Biomass[complete.cases(Biomass$row),]



TEVI <- Biomass[Biomass$species_code=='TEVI',]
TEVI

TEVI_Biomass <- merge(PhotoPoint[,c('plot_id','stand','Treatment','row','gap_fraction')], TEVI[,c('plot_id','dried_weight_g')], by=c('plot_id'), all=T)

str(TEVI_Biomass)

TEVI_Biomass$dried_weight_g[is.na(TEVI_Biomass$dried_weight_g)] <- 0

TEVI_Biomass

for(i in 1:nrow(TEVI_Biomass)){
  if(TEVI_Biomass$dried_weight_g[i]==0){
    TEVI_Biomass$Presence[i]=0}
  else{TEVI_Biomass$Presence[i]=1}
}

str(TEVI_Biomass)

TEVI_Biomass$Presence <- factor(TEVI_Biomass$Presence)

TEVI_Biomass$plot_id <- factor(TEVI_Biomass$plot_id)
TEVI_Biomass$stand <- factor(TEVI_Biomass$stand)
TEVI_Biomass$Treatment <- factor(TEVI_Biomass$Treatment)
TEVI_Biomass$Row <- factor(TEVI_Biomass$row)

# I believe stand 8 (the southern most stand just south of RT 91) had a
# supplemental seeding of wiregrass. That could influence our findings!

boxplot(dried_weight_g ~ stand, data=TEVI_Biomass)

# Wiregrass in seeded stands only!!!

# ALSO:

boxplot(dried_weight_g ~ Row, data=TEVI_Biomass)

# Wiregrass in take out rows only!!!

# Not spreading into other areas.

# First, let's check presence-absence of wiregrass:

TEVI_PQL_1 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial,
                      data=TEVI_Biomass)

summary(TEVI_PQL_1) #

TEVI_PQL_2 <- glmmPQL(Presence ~ gap_fraction,
                      random = ~1 | Treatment/stand, family=binomial(link="cloglog"),
                      data=TEVI_Biomass)

summary(TEVI_PQL_2) #

TEVI_fit_binary <- glmmadmb(Presence ~ Treatment,
                            random = ~1 | Treatment/stand, family='binomial', link='cloglog',
                            data=TEVI_Biomass)

summary(TEVI_fit_binary) #

# For TEVI - GOATS RUE biomass:

hist(TEVI_Biomass$dried_weight_g, breaks=20)

# I think the large number of 0 values is going to cause some issues with
# modeling the CONTINUOUS variable biomass. For some species, a transformation
# could be applied, but that may involve adding a small value to all values
# first. In the case of wiregrass, in appears to be presence-absence is the
# way to go, or model biomass without the zero values.

hist(TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,]$dried_weight_g, breaks=20)

library(nlme)

boxplot(E2~Row, data=TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,])

# Again, wiregrass only occurs in rows, so we cannot include it in models
# of biomass; contrasts for 2 or more levels in the data.

TEVI_LME_M1 <- lme(dried_weight_g ~ gap_fraction,
                   random = ~1|Treatment/stand,
                   data=SONU_Biomass[SONU_Biomass$dried_weight_g > 0,])

E2 <- resid(TEVI_LME_M1, type="normalized")
F2 <- fitted(TEVI_LME_M1)
plot(E2~gap_fraction, data=TEVI_Biomass[TEVI_Biomass$dried_weight_g > 0,])

summary(TEVI_LME_M1)

############### SOSE GABE TESTING FOR GOATS RUE #################

