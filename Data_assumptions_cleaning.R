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

describe(dat2)

mean(dat2$disclosure)

View(dat2$disclosure)

write.csv(describedat, file='Diss_data_described.csv')

dat3 <- read.csv('diss_data_AC_done_imputed_mahaldist.csv')

#Attention Check stuff

#subsetting attention checks

dat2 <- dat[dat$AC_total < 7,]
View(dat2)

summary(dat2)

# Data prep: Outliers

# Univariate 
freqtab <- NULL
for(k in 1:ncol(dat2)){
  freqtab[[k]] <- table(dat2[,k])}
names(freqtab) <- as.list(names(dat2))
freqtab
View(freqtab)
write.csv(freqtab, file='diss_freqtab.csv')

#Multivariate outliers

d2 <- cov(dat2[,8:82])

dat2$D2 <- mahalanobis(dat2[,8:82],colMeans(dat2[,8:82]), d2) 

dat2$mahl_sig <- ifelse(dat2$D2 >= qchisq(.99, df = ncol(dat2[,8:82])), 1, 0)

dat3 <- dat2[dat2$mahl_sig != 1,]

View(dat3)

# Data prep: Normality

des_data <- describe(dat2)

write.csv(des_data, file='describe_data.csv')

for(i in 1:ncol(dat2)){
  windows()
  hist(dat2[,i],xlab=names(dat2[i]),main=paste("Histogram of",names(dat2[i])))
  windows()
  qqnorm(dat2[,i],col="red",main=names(dat2[i]))
  qqline(dat2[,i],lwd=2)
}


# Multicollinearity and Singularity
#Variables are multicolinear if correlation > .8
#Variables are singular if correlation is > .95
multico <- round(cor(dat2,y=NULL,use="pairwise.complete.obs"),3)
write.csv(multico, file='diss_multico.csv')

#Create sub-scales 

#============ YBOCS ============#
#YBOC O

dat2$yboc_o <- (dat2$YBOC_O1 + dat2$YBOC_O2	+ dat2$YBOC_O3)/3


#YBOC C

dat2$yboc_c <- (dat2$YBOC_C1 + dat2$YBOC_C2	+ dat2$YBOC_C3)/3

#============ OCI ============#

dat2$oci <- (dat2$OCI_1	+ dat2$OCI_2	+ dat2$OCI_3	+ dat2$OCI_4	+ dat2$OCI_5	+ dat2$OCI_6	+ dat2$OCI_7	+ dat2$OCI_8	+ dat2$OCI_9	+ dat2$OCI_10 + dat2$OCI_11 + dat2$OCI_12 + dat2$OCI_13	+ dat2$OCI_14	+ dat2$OCI_15)/15

dat2$oci_check <- (dat2$OCI_1 + dat2$OCI_6 + dat2$OCI_11)/3

dat2$oci_order <- (dat2$OCI_2 + dat2$OCI_7 + dat2$OCI_12)/3

dat2$oci_count <- (dat2$OCI_3 + dat2$OCI_8 + dat2$OCI_13)/3

dat2$oci_contam <- (dat2$OCI_4 + dat2$OCI_9 + dat2$OCI_14)/3

dat2$oci_obsess <- (dat2$OCI_5 + dat2$OCI_10 + dat2$OCI_15)/3


#============ Paranoid Cognitions ============#

dat2$pc_r <- (dat2$PC_R1	+ dat2$PC_R2	+ dat2$PC_R3	+ dat2$PC_R4	+ dat2$PC_R5)/5

dat2$pc_h <- (dat2$PC_H1	+ dat2$PC_H2	+ dat2$PC_H3	+ dat2$PC_H4	+ dat2$PC_H5)/5

dat2$pc_s <- (dat2$PC_S1	+ dat2$PC_S2	+ dat2$PC_S3	+ dat2$PC_S4	+ dat2$PC_S5)/5


#============ Behavior Awareness ============#

dat2$bhv_a_1 <- (dat2$BhvAwareness_1	+ dat2$BhvAwareness_2 + dat2$BhvAwareness_3	+ dat2$BhvAwareness_4	+ dat2$BhvAwareness_5)/5
dat2$bhv_a_2 <- (dat2$BhvAwareness_6	+ dat2$BhvAwareness_7	+ dat2$BhvAwareness_8	+ dat2$BhvAwareness_9 + 	dat2$BhvAwareness_10)/5

factanal(~BhvAwareness_1 + BhvAwareness_2 + BhvAwareness_3 + BhvAwareness_4 + BhvAwareness_5 + BhvAwareness_6 + BhvAwareness_7 + BhvAwareness_8 + BhvAwareness_9 + BhvAwareness_10,  factors= 2, data = dat2)
# Removing items 4 5 and and 10

factanal(~BhvAwareness_1 + BhvAwareness_2 + BhvAwareness_3 + BhvAwareness_6 + BhvAwareness_7 + BhvAwareness_8 + BhvAwareness_9,  factors= 2, data = dat2)

factanal(~BhvAwareness_3 + BhvAwareness_6	+ BhvAwareness_7	+ BhvAwareness_8 +  BhvAwareness_9, factors=1, dat=dat2)

dat2$bhv_awareness <- (dat2$BhvAwareness_3 + dat2$BhvAwareness_6	+ dat2$BhvAwareness_7	+ dat2$BhvAwareness_8 +  dat2$BhvAwareness_9)/5
#============ Degree of Disclosure ============#

dat2$disclosure <- (dat2$DegDisc_1  + dat2$DegDisc_2 + dat2$DegDisc_3 + dat2$DegDisc_4 + dat2$DegDisc_5 + dat2$DegDisc_6 + dat2$DegDisc_8)/7

summary(dat2$DegDisc_5)


#============ Burnout ============#

dat2$burn_personal <- (dat2$Burn_personal1 + dat2$Burn_personal2 + dat2$Burn_personal3 + dat2$Burn_personal4 + dat2$Burn_personal5 + dat2$Burn_personal6)/6

dat2$burn_work <- (dat2$Burn_work1 + dat2$Burn_work2 + dat2$Burn_work3 + abs(dat2$Burn_work4 - 8) + dat2$Burn_work5 + dat2$Burn_work6 + dat2$Burn_work7)/7


summary(abs(dat2$Burn_work4-8))

#============ Job Satisfaction ============#

dat2$jobsat <- (dat2$JobSat1 + dat2$JobSat2 + dat2$JobSat3)/3


#============ Turnover Intentions ============#

dat2$turnover <- (dat2$Turnover2 + dat2$Turnover3)/3

cor.test(dat2$Turnover3, (dat2$Turnover1 -8), method='pearson')

#============ Coworker support ============#

dat2$coworksupp_I <- (dat2$CW_ISupport1 + dat2$CW_ISupport2 + dat2$CW_ISupport3 + dat2$CW_ISupport4 + dat2$CW_ISupport5 + dat2$CW_ISupport6)/6

dat2$coworksupp_E <- (dat2$CW_ESupport1 + dat2$CW_ESupport2	+ dat2$CW_ESupport3 + dat2$CW_ESupport4 + dat2$CW_ESupport5 + dat2$CW_ESupport6 + dat2$CW_ESupport7 + dat2$CW_ESupport8)/8


df <- dat2[order(dat2$coworksupp_E),]
med <- median(df$coworksupp_E)
lower <- df[1:round(nrow(df)/2),]
upper <- df[round((nrow(df)/2)+1):nrow(df),]


View(med)

upperdat2 <- dat2[which(dat2 >=med),]

#============ Organizational Support ============#

dat2$orgsupp <- (dat2$Org_sup1 + dat2$Org_sup2 + dat2$Org_sup3 + dat2$Org_sup4)/4

CronbachAlpha(dat2[,c(dat2$Org_sup1, dat2$Org_sup2, dat2$Org_sup3, dat2$Org_sup4)],  conf.level = NA, cond= FALSE, na.rm=FALSE)

#============ Internalized Stigma of Mental Illness ============#

dat2$ismi <- (dat2$ISMI_1 + dat2$ISMI_2 + dat2$ISMI_3 + dat2$ISMI_4 + dat2$ISMI_5 + dat2$ISMI_6 + dat2$ISMI_7 + dat2$ISMI_8 + dat2$ISMI_9 + dat2$ISMI_10 + dat2$ISMI_11 + dat2$ISMI_12 + dat2$ISMI_13 + dat2$ISMI_14 + dat2$ISMI_15 + dat2$ISMI_16)/16

View(dat2)

CronbachAlpha(dat2[,c(dat2$ISMI_1, dat2$ISMI_2, dat2$ISMI_3, dat2$ISMI_4, dat2$ISMI_5, dat2$ISMI_6, dat2$ISMI_7, dat2$ISMI_8, dat2$ISMI_9, dat2$ISMI_10, dat2$ISMI_11, dat2$ISMI_12, dat2$ISMI_13, dat2$ISMI_14, dat2$ISMI_15, dat2$ISMI_16)], conf.level = NA, cond= FALSE, na.rm=FALSE)

#============ ASAM ============#

dat2$asam_1 <- (dat2$ASAM1 + dat2$ASAM2 + dat2$ASAM3 + dat2$ASAM4)/4
dat2$asam_2 <- (dat2$ASAM5 + dat2$ASAM6 + dat2$ASAM7)/3


CronbachAlpha(dat2[,c(dat2$ASAM1, dat2$ASAM2, dat2$ASAM3, dat2$ASAM4)],  conf.level = NA, cond= FALSE, na.rm=FALSE)

#============ BAM ============#

dat2$bam_1 <- (dat2$BAM1 + dat2$BAM2 + dat2$BAM3 + dat2$BAM4)/4
dat2$bam_2 <- (dat2$BAM5 + dat2$BAM6 + dat2$BAM7)/3