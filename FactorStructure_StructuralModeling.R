#load packages
install.packages('DescTools')
install.packages('mice')
install.packages('graphics')
library(Hmisc)
library(psych)
library(lavaan)
library(DescTools)
library(stats)
#Load data
setwd("C:/Users/Timothy/Dropbox/Dissertation/Study 2")

dat2 <- read.csv("diss_data_prepared_v3.csv")
summary(dat2$disclosure)
View(dat2)


#===================================================#
#============= Scale Factor Structures =============#
#===================================================#


#Checking some factor structures

#YBOCS

#Factors
ybocmod <- '
f1  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
f2  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

#Var Covar
f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3'

fit1 = sem(ybocmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


#OCI
ocimod <- '
#Factors
f1 =~ 1*OCI_1 + OCI_6 + OCI_11
f2 =~ 1*OCI_2 + OCI_7 + OCI_12
f3 =~ 1*OCI_3 + OCI_8 + OCI_13
f4 =~ 1*OCI_4 + OCI_9 + OCI_14
f5 =~ 1*OCI_5 + OCI_10 + OCI_15
f6 =~ f1 + f2 + f3 + f4 + f5

#Var Covar
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f4 ~~ f4
f5 ~~ f5
f6 ~~ 1*f6
OCI_1 ~~ OCI_1
OCI_2 ~~ OCI_2
OCI_3 ~~ OCI_3
OCI_4 ~~ OCI_4
OCI_5 ~~ OCI_5
OCI_6 ~~ OCI_6
OCI_7 ~~ OCI_7
OCI_8 ~~ OCI_8
OCI_9 ~~ OCI_9
OCI_10 ~~ OCI_10
OCI_11 ~~ OCI_11
OCI_12 ~~ OCI_12
OCI_13 ~~ OCI_13
OCI_14 ~~ OCI_14
OCI_15 ~~ OCI_15'

fit1 = sem(ocimod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)




# Paranoid Cogntions 

pcmod <- '
#factors
f1 =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
f2 =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
f3 =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5
f4 =~ f1 + f2 + f3

#Var Covar
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f4 ~~ 1*f4
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5

'
fit1 = sem(pcmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


# Behavior Awareness

bhvawmod <- '
#factors
f1 =~ 1*BhvAwareness_1	+ BhvAwareness_2 + BhvAwareness_3
f2 =~ 1*BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8	+ BhvAwareness_9

#Var Covar
f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
BhvAwareness_1 ~~ BhvAwareness_1
BhvAwareness_2 ~~ BhvAwareness_2
BhvAwareness_3 ~~ BhvAwareness_3
#BhvAwareness_4 ~~ BhvAwareness_4
#BhvAwareness_5 ~~ BhvAwareness_5
BhvAwareness_6 ~~ BhvAwareness_6
BhvAwareness_7 ~~ BhvAwareness_7
BhvAwareness_8 ~~ BhvAwareness_8
BhvAwareness_9 ~~	BhvAwareness_9
#BhvAwareness_10 ~~ BhvAwareness_10
'
fit1 = sem(bhvawmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


# This is a model with the reduced items loading on a single factor

bhvawmod <- '
#factors
f1 =~ 1*BhvAwareness_3 + BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8 +  BhvAwareness_9

#Var Covar
f1 ~~ f1
BhvAwareness_3 ~~ BhvAwareness_3
BhvAwareness_6 ~~ BhvAwareness_6
BhvAwareness_7 ~~ BhvAwareness_7
BhvAwareness_8 ~~ BhvAwareness_8
BhvAwareness_9 ~~	BhvAwareness_9
'
fit1 = sem(bhvawmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


#Degree of Disclosure

# Doing some Explorator factor analysis

factanal(~DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_7 + DegDisc_8,  factors= 2, data = dat2)

# A two factor model exposes that Item 7 does not really jell with the others. 

factanal(~DegDisc_1  + DegDisc_2 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_7 + DegDisc_8,  factors= 1, data = dat2)

# removing item 5

factanal(~DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_6 + DegDisc_7 + DegDisc_8,  factors= 1, data = dat2)

# Removing 1 and 2

factanal(~DegDisc_3 + DegDisc_4 + DegDisc_6 + DegDisc_7 + DegDisc_8,  factors= 1, data = dat2)


ddmod <- '

#Factors
f1 =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_7 + DegDisc_8

#Var Covar
f1 ~~ f1
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8'

fit1 = sem(ddmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

# Reduced model

dat4 <- read.csv("diss_data_AC_done_imputed.csv")

ddmod <- '

#Factors
f1 =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

#Var Covar
f1 ~~ f1
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_8 ~~ DegDisc_8'

fit1 = sem(ddmod, data=dat4)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

#Burnout

burnmod <- '

#Factors

f1 =~ 1*Burn_personal1 + Burn_personal2 + Burn_personal3 + Burn_personal4 + Burn_personal5 + Burn_personal6
f2 =~ 1*Burn_work1 + Burn_work2 + Burn_work3 + Burn_work4 + Burn_work5 + Burn_work6 + Burn_work7

#Var Covar
f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
Burn_personal1 ~~ Burn_personal1
Burn_personal2 ~~ Burn_personal2
Burn_personal3 ~~ Burn_personal3
Burn_personal4 ~~ Burn_personal4
Burn_personal5 ~~ Burn_personal5
Burn_personal6 ~~ Burn_personal6
Burn_work1 ~~ Burn_work1
Burn_work2 ~~ Burn_work2
Burn_work3 ~~ Burn_work3
Burn_work4 ~~ Burn_work4
Burn_work5 ~~ Burn_work5
Burn_work6 ~~ Burn_work6
Burn_work7 ~~ Burn_work7
'
fit1 = sem(burnmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)




#Job Satisfaction 
jsmod <- '

# Factor
f1 =~ 1*JobSat1 + JobSat2 + JobSat3

#Var Covar
f1 ~~ f1
JobSat1 ~~ JobSat1
JobSat2 ~~ JobSat2
JobSat3 ~~ JobSat3
'
fit1 = sem(jsmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)




#=Turnover Intentions

timod <- '
f1 =~ 1*Turnover1 + Turnover2 + Turnover3

f1 ~~ f1
Turnover1 ~~ Turnover1
Turnover2 ~~ Turnover2
Turnover3 ~~ Turnover3
'


fit1 = sem(timod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


#============ Coworker support ============#
cwmod <- '

#Factors
f1 =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

f2 =~ 1*CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8

#Var Covar

f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
#CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
'
fit1 = sem(cwmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

# Removing CW_ESupport1 improves the model significantly



cwmod <- '

#Factors
f1 =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

f2 =~ 1*CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8


f3 =~ 1*Org_sup1 + Org_sup2 + Org_sup3 + Org_sup4

#Regressions

f3 ~  f1 + f2

#Var Covar

f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
#CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
Org_sup1 ~~ Org_sup1
Org_sup2 ~~ Org_sup2
Org_sup3 ~~ Org_sup3
Org_sup4 ~~ Org_sup4
'
fit1 = sem(cwmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


#============ Organizational Support ============#
orgmod <- '

#Factors

f1 =~ 1*Org_sup1 + Org_sup2 + Org_sup3 + Org_sup4


#Var Covar
f1 ~~ f1
Org_sup1 ~~ Org_sup1
Org_sup2 ~~ Org_sup2
Org_sup3 ~~ Org_sup3
Org_sup4 ~~ Org_sup4
'
fit1 = sem(orgmod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

#============ Internalized Stigma of Mental Illness ============#

ismimod <- '

#Factors
f1 =~ 1*ISMI_1 + ISMI_2 + ISMI_3 + ISMI_4 + ISMI_5 + ISMI_6 + ISMI_7 + ISMI_8 + ISMI_9 + ISMI_10 + ISMI_11 + ISMI_12 + ISMI_13 + ISMI_14 + ISMI_15 + ISMI_16

#Var Covar
f1 ~~ f1
ISMI_1 ~~ ISMI_1
ISMI_2 ~~ ISMI_2
ISMI_3 ~~ ISMI_3
ISMI_4 ~~ ISMI_4
ISMI_5 ~~ ISMI_5
ISMI_6 ~~ ISMI_6
ISMI_7 ~~ ISMI_7
ISMI_8 ~~ ISMI_8
ISMI_9 ~~ ISMI_9
ISMI_10 ~~ ISMI_10
ISMI_11 ~~ ISMI_11
ISMI_12 ~~ ISMI_12
ISMI_13 ~~ ISMI_13
ISMI_14 ~~ ISMI_14
ISMI_15 ~~ ISMI_15
ISMI_16 ~~ ISMI_16
'


fit1 = sem(ismimod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


factanal(~ISMI_1  + ISMI_2 + ISMI_3 + ISMI_4 + ISMI_5 + ISMI_6 + ISMI_7 + ISMI_8 + ISMI_9 + ISMI_10 + ISMI_11 + ISMI_12 + ISMI_13+ ISMI_14 + ISMI_5 + ISMI_16,  factors= 2, data = dat2)


# Item 4, 6?, 7, 8, 9. 10, 11, 12



#============= ASAM =============#

#Factors
asammod <- '
f1 =~ 1*ASAM1 + ASAM2 + ASAM3 + ASAM4
f2 =~ 1*ASAM5 + ASAM6 + ASAM7

#Var Covar
f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
ASAM1 ~~ ASAM1
ASAM2 ~~ ASAM2
ASAM3 ~~ ASAM3
ASAM4 ~~ ASAM4
ASAM5 ~~ ASAM5
ASAM6 ~~ ASAM6
ASAM7 ~~ ASAM7
'

fit1 = sem(asammod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)


#============= BAM =============#
bammod <- '
#factors
f1 =~ BAM1 + BAM2 + BAM3 + BAM4
f2 =~ BAM5 + BAM6 + BAM7

#var covar
f1 ~~ f1
f2 ~~ f2
f1 ~~ f2
BAM1 ~~ BAM1
BAM2 ~~ BAM2
BAM3 ~~ BAM3
BAM4 ~~ BAM4
BAM5 ~~ BAM5
BAM6 ~~ BAM6
BAM7 ~~ BAM7
'
fit1 = sem(bammod, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

View(dat2)

#===================================#
#============= Model 1 =============#
#===================================#

#YBOC O

mod1 <- '
# PC factors
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5
PC =~ RUM + HYP + SIN

#yboc factors
OBSESS  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
COMPEL  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

PC ~ OBSESS + COMPEL

#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
PC ~~ 1*PC
OBSESS ~~ OBSESS
COMPEL ~~ COMPEL
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
'

fit1 = sem(mod1, data=dat2, estimator = "MLM")
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

#=====================================#
#============= Model 1.5 =============#
#=====================================#

#YBOC O

mod1.5 <- '
# PC factors
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5

#yboc factors
OBSESS  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
COMPEL  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

#OCD second order
RUM ~ OBSESS + COMPEL
HYP ~ OBSESS + COMPEL
SIN ~ OBSESS + COMPEL

#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
RUM ~~ HYP
RUM ~~ SIN
HYP ~~ SIN
OBSESS ~~ OBSESS
COMPEL ~~ COMPEL
OBSESS ~~ COMPEL
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
'

fit1 = sem(mod1.5, data=dat2)
summary(fit1, fit.measures=TRUE, standardized=T)
res1 <- residuals(fit1, type='standardized')
knitr::kable(res1, digit=2)

#===================================#
#============= Model 2 =============#
#===================================#

# This is a moderation using the 5 items of behavior awareness

# Moderation Prep
dat2$ptc13   <- dat2$YBOC_C1 * dat2$BhvAwareness_3
dat2$ptc16   <- dat2$YBOC_C1 * dat2$BhvAwareness_6
dat2$ptc17   <- dat2$YBOC_C1 * dat2$BhvAwareness_7
dat2$ptc18   <- dat2$YBOC_C1 * dat2$BhvAwareness_8
dat2$ptc19   <- dat2$YBOC_C1 * dat2$BhvAwareness_9


dat2$ptc23   <- dat2$YBOC_C2 * dat2$BhvAwareness_3
dat2$ptc26   <- dat2$YBOC_C2 * dat2$BhvAwareness_6
dat2$ptc27   <- dat2$YBOC_C2 * dat2$BhvAwareness_7
dat2$ptc28   <- dat2$YBOC_C2 * dat2$BhvAwareness_8
dat2$ptc29   <- dat2$YBOC_C2 * dat2$BhvAwareness_9

dat2$ptc33   <- dat2$YBOC_C3 * dat2$BhvAwareness_3
dat2$ptc36   <- dat2$YBOC_C3 * dat2$BhvAwareness_6
dat2$ptc37   <- dat2$YBOC_C3 * dat2$BhvAwareness_7
dat2$ptc38   <- dat2$YBOC_C3 * dat2$BhvAwareness_8
dat2$ptc39   <- dat2$YBOC_C3 * dat2$BhvAwareness_9

dat2$pto13   <- dat2$YBOC_O1 * dat2$BhvAwareness_3
dat2$pto16   <- dat2$YBOC_O1 * dat2$BhvAwareness_6
dat2$pto17   <- dat2$YBOC_O1 * dat2$BhvAwareness_7
dat2$pto18   <- dat2$YBOC_O1 * dat2$BhvAwareness_8
dat2$pto19   <- dat2$YBOC_O1 * dat2$BhvAwareness_9


dat2$pto23   <- dat2$YBOC_O2 * dat2$BhvAwareness_3
dat2$pto26   <- dat2$YBOC_O2 * dat2$BhvAwareness_6
dat2$pto27   <- dat2$YBOC_O2 * dat2$BhvAwareness_7
dat2$pto28   <- dat2$YBOC_O2 * dat2$BhvAwareness_8
dat2$pto29   <- dat2$YBOC_O2 * dat2$BhvAwareness_9

dat2$pto33   <- dat2$YBOC_O3 * dat2$BhvAwareness_3
dat2$pto36   <- dat2$YBOC_O3 * dat2$BhvAwareness_6
dat2$pto37   <- dat2$YBOC_O3 * dat2$BhvAwareness_7
dat2$pto38   <- dat2$YBOC_O3 * dat2$BhvAwareness_8
dat2$pto39   <- dat2$YBOC_O3 * dat2$BhvAwareness_9


dat2$resc13 <- resid(lm(ptc13 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc16 <- resid(lm(ptc16 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc17 <- resid(lm(ptc17 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc18 <- resid(lm(ptc18 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc19 <- resid(lm(ptc19 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))




dat2$resc23 <- resid(lm(ptc13 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc26 <- resid(lm(ptc26 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc27 <- resid(lm(ptc27 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc28 <- resid(lm(ptc28 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc29 <- resid(lm(ptc29 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))




dat2$resc33 <- resid(lm(ptc33 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc36 <- resid(lm(ptc36 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc37 <- resid(lm(ptc37 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc38 <- resid(lm(ptc38 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$resc39 <- resid(lm(ptc39 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, data=dat2,na.action = na.exclude))





dat2$reso13 <- resid(lm(pto13 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso16 <- resid(lm(pto16 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso17 <- resid(lm(pto17 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso18 <- resid(lm(pto18 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso19 <- resid(lm(pto19 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))




dat2$reso23 <- resid(lm(pto13 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso26 <- resid(lm(pto26 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso27 <- resid(lm(pto27 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso28 <- resid(lm(pto28 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso29 <- resid(lm(pto29 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))




dat2$reso33 <- resid(lm(pto33 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso36 <- resid(lm(pto36 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso37 <- resid(lm(pto37 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso38 <- resid(lm(pto38 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso39 <- resid(lm(pto39 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))
View(dat2)

mod2 <- '
# PC factors
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5

#yboc factors
OBSESS  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
COMPEL  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

#Behavior awareness factors

#BhvAware =~ 1*BhvAwareness_3 + BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8 +  BhvAwareness_9

RXN =~ resc13 + resc16 + resc17 + resc18 + resc19 + resc23 + resc26 + resc27 + resc28 + resc29 + resc33 + resc36 + resc37 + resc38 + resc39 + reso13 + reso16 + reso17 + reso18 + reso19 + reso23 + reso26 + reso27 + reso28 + reso29 + reso33 + reso36 + reso37 + reso38 + reso39


# Regressions

RUM ~ OBSESS + COMPEL + RXN
HYP ~ OBSESS + COMPEL + RXN
SIN ~ OBSESS + COMPEL + RXN

# Product term does not correlate with first order effect variables
RXN ~~ 0*COMPEL
RXN ~~ 0*OBSESS
RXN ~~ 0*RUM
RXN ~~ 0*HYP
RXN ~~ 0*SIN

#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
RUM ~~ HYP
RUM ~~ SIN
HYP ~~ SIN
OBSESS ~~ OBSESS
COMPEL ~~ COMPEL
OBSESS ~~ COMPEL
#BhvAware ~~ BhvAware
RXN ~~ RXN
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
#BhvAwareness_3 ~~ BhvAwareness_3
#BhvAwareness_6 ~~ BhvAwareness_6
#BhvAwareness_7 ~~ BhvAwareness_7
#BhvAwareness_8 ~~ BhvAwareness_8
#BhvAwareness_9 ~~ BhvAwareness_9
resc13 ~~ resc16
resc13 ~~ resc17
resc13 ~~ resc18
resc13 ~~ resc19
resc16 ~~ resc17
resc16 ~~ resc18
resc16 ~~ resc19
resc17 ~~ resc18
resc17 ~~ resc19
resc18 ~~ resc19

resc23 ~~ resc26
resc23 ~~ resc27
resc23 ~~ resc28
resc23 ~~ resc29
resc26 ~~ resc27
resc26 ~~ resc28
resc26 ~~ resc29
resc27 ~~ resc28
resc27 ~~ resc29
resc28 ~~ resc29

resc33 ~~ resc36
resc33 ~~ resc37
resc33 ~~ resc38
resc33 ~~ resc39
resc36 ~~ resc37
resc36 ~~ resc38
resc36 ~~ resc39
resc37 ~~ resc38
resc37 ~~ resc39
resc38 ~~ resc39

reso13 ~~ reso16
reso13 ~~ reso17
reso13 ~~ reso18
reso13 ~~ reso19
reso16 ~~ reso17
reso16 ~~ reso18
reso16 ~~ reso19
reso17 ~~ reso18
reso17 ~~ reso19
reso18 ~~ reso19

reso23 ~~ reso26
reso23 ~~ reso27
reso23 ~~ reso28
reso23 ~~ reso29
reso26 ~~ reso27
reso26 ~~ reso28
reso26 ~~ reso29
reso27 ~~ reso28
reso27 ~~ reso29
reso28 ~~ reso29

reso33 ~~ reso36
reso33 ~~ reso37
reso33 ~~ reso38
reso33 ~~ reso39
reso36 ~~ reso37
reso36 ~~ reso38
reso36 ~~ reso39
reso37 ~~ reso38
reso37 ~~ reso39
reso38 ~~ reso39
'

fit2 = sem(mod2, data=dat2)
summary(fit2, fit.measures=TRUE, standardized=T)
res2 <- residuals(fit2, type='standardized')
knitr::kable(res2, digit=2)


#===================================#
#============= Model 2.5 =============#
#===================================#

# This is a moderation using the composite score of behavior awareness

# Moderation Prep
dat2$ptc11   <- dat2$YBOC_C1 * dat2$bhv_awareness

dat2$ptc21   <- dat2$YBOC_C2 * dat2$bhv_awareness

dat2$ptc31   <- dat2$YBOC_C3 * dat2$bhv_awareness

dat2$pto11   <- dat2$YBOC_O1 * dat2$bhv_awareness

dat2$pto21   <- dat2$YBOC_O2 * dat2$bhv_awareness

dat2$pto31   <- dat2$YBOC_O3 * dat2$bhv_awareness



dat2$resc11 <- resid(lm(ptc11 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$resc21 <- resid(lm(ptc21 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$resc31 <- resid(lm(ptc31 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$reso11 <- resid(lm(pto11 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso21 <- resid(lm(pto21 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso31 <- resid(lm(pto31 ~ YBOC_C1+YBOC_C2+YBOC_C3 + YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))





dat2$resc11 <- resid(lm(ptc11 ~ YBOC_C1+YBOC_C2+YBOC_C3 + bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$resc21 <- resid(lm(ptc21 ~ YBOC_C1+YBOC_C2+YBOC_C3 +  bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$resc31 <- resid(lm(ptc31 ~ YBOC_C1+YBOC_C2+YBOC_C3 + bhv_awareness, 
                        data=dat2,na.action = na.exclude))

dat2$reso11 <- resid(lm(pto11 ~ YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso21 <- resid(lm(pto21 ~ YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))

dat2$reso31 <- resid(lm(pto31 ~ YBOC_O1+YBOC_O2+YBOC_O3 + BhvAwareness_3+BhvAwareness_6+BhvAwareness_7+BhvAwareness_8+BhvAwareness_9, 
                        data=dat2,na.action = na.exclude))



View(dat2)

mod2 <- '
# PC factors
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5

#yboc factors
OBSESS  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
COMPEL  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

#Behavior awareness factors

#BhvAware =~ 1*BhvAwareness_3 + BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8 +  BhvAwareness_9

RXN =~ resc11 + resc21 + resc31 + reso11 + reso21 + reso31


# Regressions

RUM ~ OBSESS + COMPEL + RXN 
HYP ~ OBSESS + COMPEL + RXN 
SIN ~ OBSESS + COMPEL + RXN

# Product term does not correlate with first order effect variables
RXN ~~ 0*COMPEL
RXN ~~ 0*OBSESS
RXN ~~ 0*RUM
RXN ~~ 0*HYP
RXN ~~ 0*SIN

#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
RUM ~~ HYP
RUM ~~ SIN
HYP ~~ SIN
OBSESS ~~ OBSESS
COMPEL ~~ COMPEL
OBSESS ~~ COMPEL
#BhvAware ~~ BhvAware
RXN ~~ RXN
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
#BhvAwareness_3 ~~ BhvAwareness_3
#BhvAwareness_6 ~~ BhvAwareness_6
#BhvAwareness_7 ~~ BhvAwareness_7
#BhvAwareness_8 ~~ BhvAwareness_8
#BhvAwareness_9 ~~ BhvAwareness_9
resc11 ~~ resc21
resc11 ~~ resc31
resc31 ~~ resc21

reso11 ~~ reso21
reso11 ~~ reso31
reso31 ~~ reso21
'

fit2 = sem(mod2, data=dat2, estimator = "MLM")
summary(fit2, fit.measures=TRUE, standardized=T)
res2 <- residuals(fit2, type='standardized')
knitr::kable(res2, digit=2)


#===================================#
#=========== Model 2.75 ============#
#===================================#

modx <- '
#This is a model using behavior warness as a mediator rather than a moderator

# disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

BhvAware =~ 1*BhvAwareness_3 + BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8 +  BhvAwareness_9

# Direct effects

Disc ~ BhvAware

Disc ~~ Disc
BhvAware ~~ BhvAware
BhvAwareness_3 ~~ BhvAwareness_3
BhvAwareness_6 ~~ BhvAwareness_6
BhvAwareness_7 ~~ BhvAwareness_7
BhvAwareness_8 ~~ BhvAwareness_8
BhvAwareness_9 ~~ BhvAwareness_9
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_8 ~~ DegDisc_8
'
fitx = sem(modx, data=dat2)
summary(fitx, fit.measures=TRUE, standardized=T)
resx <- residuals(fitx, type='standardized')
knitr::kable(resx, digit=2)

#===================================#
#============= Model 3 =============#
#===================================#


mod3 <- '
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5

# disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

# Regressions

Disc ~ RUM + HYP + SIN

#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
RUM ~~ HYP
RUM ~~ SIN
HYP ~~ SIN
Disc ~~ Disc
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_8 ~~ DegDisc_8
'
fit3 = sem(mod3, data=dat2, estimator = "MLM")
summary(fit3, fit.measures=TRUE, standardized=T)
res3 <- residuals(fit3, type='standardized')
knitr::kable(res3, digit=2)

#===================================#
#============= Model 3.5 =============#
#===================================#

# Including OCD into this model

mod3.5 <- '


# PC factors
f1 =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
f2 =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
f3 =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5
f7 =~ f1 + f2 + f3

# disclosure factors
f4  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

#yboc factors
f5  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
f6  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3

#OCD second order
f7 ~ f5 + f6

# Regressions

f4 ~ f7

#Var Covar
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f4 ~~ f4
f5 ~~ f5
f6 ~~ f6
f7 ~~ f7
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_8 ~~ DegDisc_8
'
fit3.5 = sem(mod3.5, data=dat2)
summary(fit3.5, fit.measures=TRUE, standardized=T)
res3.5 <- residuals(fit3.5, type='standardized')
knitr::kable(res3.5, digit=2)


#===================================#
#============= Model 4 =============#
#===================================#


mod4 <- '
# Coworker Support factors
InstrSupp =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

EmoSupp =~ 1*CW_ESupport1 + CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8

# disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

# Regressions

InstrSupp ~ Disc
EmoSupp ~ Disc

#Var Covar
InstrSupp ~~ InstrSupp
EmoSupp ~~ EmoSupp
InstrSupp ~~ EmoSupp
Disc ~~ Disc
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
#DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
'
fit4 = sem(mod4, data=dat3, estimator = "MLM")
summary(fit4, fit.measures=TRUE, standardized=T)
res4 <- residuals(fit4, type='standardized')
knitr::kable(res4, digit=2)

mod4.5 <- '
# Coworker Support factors
InstrSupp =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

EmoSupp =~ 1*CW_ESupport1 + CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8

# disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

# Regressions

Disc ~ InstrSupp + EmoSupp

#Var Covar
InstrSupp ~~ InstrSupp
EmoSupp ~~ EmoSupp
InstrSupp ~~ EmoSupp
Disc ~~ Disc
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
#DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
'
fit4 = sem(mod4, data=dat2, estimator = "MLM")
summary(fit4, fit.measures=TRUE, standardized=T)
res4 <- residuals(fit4, type='standardized')
knitr::kable(res4, digit=2)

#===================================#
#============= Model 5 =============#
#===================================#


mod5 <- '
# Job Satisfaction Factor

JS =~ 1*JobSat1 + JobSat2 + JobSat3

# Burnout Factors

PersBurn =~ 1*Burn_personal1 + Burn_personal2 + Burn_personal3 + Burn_personal4 + Burn_personal5 + Burn_personal6
WorkBurn =~ 1*Burn_work1 + Burn_work2 + Burn_work3 + Burn_work4 + Burn_work5 + Burn_work6 + Burn_work7

# Turnover Intentions Factor

TI =~ 1*Turnover2 + Turnover3


# Disclosure factors
Disc =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8

# Regressions

JS ~ Disc
PersBurn ~ Disc
WorkBurn ~ Disc
TI ~ Disc


#Var Covar
JS ~~ JS
PersBurn ~~ PersBurn
WorkBurn ~~ WorkBurn
PersBurn ~~ WorkBurn
TI ~~ TI
Disc ~~ Disc
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
#DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
Burn_personal1 ~~ Burn_personal1
Burn_personal2 ~~ Burn_personal2
Burn_personal3 ~~ Burn_personal3
Burn_personal4 ~~ Burn_personal4
Burn_personal5 ~~ Burn_personal5
Burn_personal6 ~~ Burn_personal6
Burn_work1 ~~ Burn_work1
Burn_work2 ~~ Burn_work2
Burn_work3 ~~ Burn_work3
Burn_work4 ~~ Burn_work4
Burn_work5 ~~ Burn_work5
Burn_work6 ~~ Burn_work6
Burn_work7 ~~ Burn_work7
JobSat1 ~~ JobSat1
JobSat2 ~~ JobSat2
JobSat3 ~~ JobSat3
#Turnover1 ~~ Turnover1
Turnover2 ~~ Turnover2
Turnover3 ~~ Turnover3
'
fit5 = sem(mod5, data=dat2, estimator = "MLM")
summary(fit5, fit.measures=TRUE, standardized=T)
res5 <- residuals(fit5, type='standardized')
knitr::kable(res5, digit=2)

?sem

mod5burn <- '
# Burnout Factors

f1 =~ 1*Burn_personal1 + Burn_personal2 + Burn_personal3 + Burn_personal4 + Burn_personal5 + Burn_personal6
f2 =~ 1*Burn_work1 + Burn_work2 + Burn_work3 + Burn_work4 + Burn_work5 + Burn_work6 + Burn_work7
f4 =~ f1 + f2


# Disclosure factors
f3  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_7 + DegDisc_8

# Regressions

f4 ~ f3



#Var Covar
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f4 ~~ 1*f4
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
Burn_personal1 ~~ Burn_personal1
Burn_personal2 ~~ Burn_personal2
Burn_personal3 ~~ Burn_personal3
Burn_personal4 ~~ Burn_personal4
Burn_personal5 ~~ Burn_personal5
Burn_personal6 ~~ Burn_personal6
Burn_work1 ~~ Burn_work1
Burn_work2 ~~ Burn_work2
Burn_work3 ~~ Burn_work3
Burn_work4 ~~ Burn_work4
Burn_work5 ~~ Burn_work5
Burn_work6 ~~ Burn_work6
Burn_work7 ~~ Burn_work7
'
fit5burn = sem(mod5burn, data=dat2)
summary(fit5burn, fit.measures=TRUE, standardized=T)
res5burn <- residuals(fit5burn, type='standardized')
knitr::kable(res5burn, digit=2)


#===================================#
#============= Model 6 =============#
#===================================#


mod6 <- '
# Job Satisfaction Factor

JS =~ 1*JobSat1 + JobSat2 + JobSat3

# Burnout Factors

PersBurn =~ 1*Burn_personal1 + Burn_personal2 + Burn_personal3 + Burn_personal4 + Burn_personal5 + Burn_personal6
WorkBurn =~ 1*Burn_work1 + Burn_work2 + Burn_work3 + Burn_work4 + Burn_work5 + Burn_work6 + Burn_work7

# Turnover Intentions Factor

TI =~ 1*Turnover2 + Turnover3


# Disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8


# Coworker support factors
InstrSupp =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

EmoSupp =~ 1*CW_ESupport1 + CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8
CoworkSupp =~ InstrSupp + EmoSupp

# Direct effects

JS ~ c*Disc
PersBurn ~ c*Disc
WorkBurn ~ c*Disc
TI ~ c*Disc

# Mediator

CoworkSupp ~ a*Disc
JS ~ b*CoworkSupp
PersBurn ~ b*CoworkSupp
WorkBurn ~ b*CoworkSupp
TI ~ b*CoworkSupp


# Indirect effect 

ab := a*b

# Total Effect

total := c + (a*b)


#Var Covar
JS ~~ JS
PersBurn ~~ PersBurn
WorkBurn ~~ WorkBurn
JS ~~ PersBurn
PersBurn ~~ TI
PersBurn ~~ WorkBurn
WorkBurn ~~ TI
TI ~~ TI
Disc ~~ Disc
EmoSupp ~~ EmoSupp
InstrSupp ~~ InstrSupp
CoworkSupp ~~ 1*CoworkSupp
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
#DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
Burn_personal1 ~~ Burn_personal1
Burn_personal2 ~~ Burn_personal2
Burn_personal3 ~~ Burn_personal3
Burn_personal4 ~~ Burn_personal4
Burn_personal5 ~~ Burn_personal5
Burn_personal6 ~~ Burn_personal6
Burn_work1 ~~ Burn_work1
Burn_work2 ~~ Burn_work2
Burn_work3 ~~ Burn_work3
Burn_work4 ~~ Burn_work4
Burn_work5 ~~ Burn_work5
Burn_work6 ~~ Burn_work6
Burn_work7 ~~ Burn_work7
JobSat1 ~~ JobSat1
JobSat2 ~~ JobSat2
JobSat3 ~~ JobSat3
#Turnover1 ~~ Turnover1
Turnover2 ~~ Turnover2
Turnover3 ~~ Turnover3
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
'
fit6 = sem(mod6, data=dat3, estimator = 'MLM')
summary(fit6, fit.measures=TRUE, standardized=T)
res6 <- residuals(fit6, type='standardized')
knitr::kable(res6, digit=2)



#===================================#
#============= Model 7 =============#
#===================================#


mod7 <- '

#yboc factors
OBSESS  =~ 1*YBOC_O1 + YBOC_O2	+ YBOC_O3
COMPULS  =~ 1*YBOC_C1 + YBOC_C2	+ YBOC_C3


# PC factors
RUM =~ 1*PC_R1	+ PC_R2	+ PC_R3	+ PC_R4	+ PC_R5
HYP =~ 1*PC_H1	+ PC_H2	+ PC_H3	+ PC_H4	+ PC_H5
SIN =~ 1*PC_S1	+ PC_S2	+ PC_S3	+ PC_S4	+ PC_S5
PC =~ RUM + HYP + SIN

# Disclosure factors
Disc  =~ 1*DegDisc_1  + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6+ DegDisc_8


# Coworker support factors
EmoSupp =~ 1*CW_ESupport1 + CW_ESupport2	+ CW_ESupport3 + CW_ESupport4 + CW_ESupport5 + CW_ESupport6 + CW_ESupport7 + CW_ESupport8

InstrSupp =~ 1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6

1*CW_ISupport1 + CW_ISupport2 + CW_ISupport3 + CW_ISupport4 + CW_ISupport5 + CW_ISupport6
# Job Satisfaction Factor

JS =~ 1*JobSat1 + JobSat2 + JobSat3

# Burnout Factors

PersBurn =~ 1*Burn_personal1 + Burn_personal2 + Burn_personal3 + Burn_personal4 + Burn_personal5 + Burn_personal6
WorkBurn =~ 1*Burn_work1 + Burn_work2 + Burn_work3 + Burn_work4 + Burn_work5 + Burn_work6 + Burn_work7

# Turnover Intentions Factor

TI =~ 1*Turnover2 + Turnover3

# Direct effects

PC ~ OBSESS + COMPULS
Disc ~ PC
JS ~ c*Disc
PersBurn ~ c*Disc
WorkBurn ~ c*Disc
TI ~ c*Disc

# Mediator

EmoSupp ~ a*Disc
InstrSupp ~ a*Disc
JS ~ b*EmoSupp
PersBurn ~ b*EmoSupp
WorkBurn ~ b*EmoSupp
TI ~ b*EmoSupp
JS ~ b*InstrSupp
PersBurn ~ b*InstrSupp
WorkBurn ~ b*InstrSupp
TI ~ b*InstrSupp


# Indirect effect 

ab := a*b

# Total Effect

total := c + (a*b)


#Var Covar
RUM ~~ RUM
HYP ~~ HYP
SIN ~~ SIN
OBSESS ~~ OBSESS
COMPULS ~~ COMPULS
JS ~~ JS
PersBurn ~~ PersBurn
WorkBurn ~~ WorkBurn
TI ~~ TI
PersBurn ~~ WorkBurn
Disc ~~ Disc
EmoSupp ~~ EmoSupp
EmoSupp ~~ InstrSupp
InstrSupp ~~ InstrSupp
PC ~~ 1*PC
DegDisc_1 ~~ DegDisc_1 
DegDisc_2 ~~ DegDisc_2
DegDisc_3 ~~ DegDisc_3
DegDisc_4 ~~ DegDisc_4
DegDisc_5 ~~ DegDisc_5
DegDisc_6 ~~ DegDisc_6
#DegDisc_7 ~~ DegDisc_7
DegDisc_8 ~~ DegDisc_8
Burn_personal1 ~~ Burn_personal1
Burn_personal2 ~~ Burn_personal2
Burn_personal3 ~~ Burn_personal3
Burn_personal4 ~~ Burn_personal4
Burn_personal5 ~~ Burn_personal5
Burn_personal6 ~~ Burn_personal6
Burn_work1 ~~ Burn_work1
Burn_work2 ~~ Burn_work2
Burn_work3 ~~ Burn_work3
Burn_work4 ~~ Burn_work4
Burn_work5 ~~ Burn_work5
Burn_work6 ~~ Burn_work6
Burn_work7 ~~ Burn_work7
JobSat1 ~~ JobSat1
JobSat2 ~~ JobSat2
JobSat3 ~~ JobSat3
#Turnover1 ~~ Turnover1
Turnover2 ~~ Turnover2
Turnover3 ~~ Turnover3
CW_ISupport1 ~~ CW_ISupport1
CW_ISupport2 ~~ CW_ISupport2
CW_ISupport3 ~~ CW_ISupport3
CW_ISupport4 ~~ CW_ISupport4
CW_ISupport5 ~~ CW_ISupport5
CW_ISupport6 ~~ CW_ISupport6
CW_ESupport1 ~~ CW_ESupport1
CW_ESupport2 ~~ CW_ESupport2
CW_ESupport3 ~~ CW_ESupport3
CW_ESupport4 ~~ CW_ESupport4
CW_ESupport5 ~~ CW_ESupport5
CW_ESupport6 ~~ CW_ESupport6
CW_ESupport7 ~~ CW_ESupport7
CW_ESupport8 ~~ CW_ESupport8
YBOC_C1 ~~ YBOC_C1
YBOC_C2 ~~ YBOC_C2
YBOC_C3 ~~ YBOC_C3
YBOC_O1 ~~ YBOC_O1
YBOC_O2 ~~ YBOC_O2
YBOC_O3 ~~ YBOC_O3
PC_R1 ~~ PC_R1
PC_R2 ~~ PC_R2
PC_R3 ~~ PC_R3
PC_R4 ~~ PC_R4
PC_R5 ~~ PC_R5
PC_H1 ~~ PC_H1
PC_H2 ~~ PC_H2
PC_H3 ~~ PC_H3
PC_H4 ~~ PC_H4
PC_H5 ~~ PC_H5
PC_S1 ~~ PC_S1
PC_S2 ~~ PC_S2
PC_S3 ~~ PC_S3
PC_S4 ~~ PC_S4
PC_S5 ~~ PC_S5
'
fit7 = sem(mod7, data=dat2, estimator = "MLM")
summary(fit7, fit.measures=TRUE, standardized=T)
res7 <- residuals(fit7, type='standardized')
knitr::kable(res7, digit=2)

lavInspect(fit7, "cov.lv")