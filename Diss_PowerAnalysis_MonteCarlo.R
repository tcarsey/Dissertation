install.packages('simsem')
library(lavaan)
library(simsem)

pop.model <-'
RUM =~ .3*PC_R1	+ .3*PC_R2	+ .3*PC_R3	+ .3*PC_R4	+ .3*PC_R5
HYP =~ .3*PC_H1	+ .3*PC_H2	+ .3*PC_H3	+ .3*PC_H4	+ .3*PC_H5
SIN =~ .3*PC_S1	+ .3*PC_S2	+ .3*PC_S3	+ .3*PC_S4	+ .3*PC_S5
PC =~ .3*RUM + .3*HYP + .3*SIN

# disclosure factors
Disc  =~ .3*DegDisc_1  + .3*DegDisc_2 + .3*DegDisc_3 + .3*DegDisc_4 + .3*DegDisc_5 + .3*DegDisc_6 + .3*DegDisc_8

# Regressions

Disc ~ .3*PC

#Var Covar
RUM ~~ .51*RUM
HYP ~~ .51*HYP
SIN ~~ .51*SIN
RUM ~~ .51*HYP
RUM ~~ .51*SIN
HYP ~~ .51*SIN
Disc ~~ .51*Disc
PC_R1 ~~ .51*PC_R1
PC_R2 ~~ .51*PC_R2
PC_R3 ~~ .51*PC_R3
PC_R4 ~~ .51*PC_R4
PC_R5 ~~ .51*PC_R5
PC_H1 ~~ .51*PC_H1
PC_H2 ~~ .51*PC_H2
PC_H3 ~~ .51*PC_H3
PC_H4 ~~ .51*PC_H4
PC_H5 ~~ .51*PC_H5
PC_S1 ~~ .51*PC_S1
PC_S2 ~~ .51*PC_S2
PC_S3 ~~ .51*PC_S3
PC_S4 ~~ .51*PC_S4
PC_S5 ~~ .51*PC_S5
DegDisc_1 ~~ .51*DegDisc_1 
DegDisc_2 ~~ .51*DegDisc_2
DegDisc_3 ~~ .51*DegDisc_3
DegDisc_4 ~~ .51*DegDisc_4
DegDisc_5 ~~ .51*DegDisc_5
DegDisc_6 ~~ .51*DegDisc_6
DegDisc_8 ~~ .51*DegDisc_8
'

pop.fit <- sem(pop.model, fixed.x=FALSE)
summary(pop.fit, standardized=TRUE)

analyzeNull <- "
PC =~ PC_R1 + PC_R2 + PC_R3 + PC_R4 + PC_R5 + PC_H1 + PC_H2 + PC_H3 + PC_H4 + PC_H5 + PC_S1 + PC_S2 + PC_S3 + PC_S4 + PC_S5
Disc =~ DegDisc_1 + DegDisc_2 + DegDisc_3 + DegDisc_4 + DegDisc_5 + DegDisc_6 + DegDisc_8
Disc ~ PC
"


Output.ALT.500 <- sim(500, analyzeNull, n=350, generate=pop.model, std.lv=TRUE, lavaanfun = "sem")
Output.ALT.450 <- sim(500, analyzeNull, n=450, generate=pop.model, std.lv=TRUE, lavaanfun = "sem")
Output.ALT.1000.3 <- sim(500, analyzeNull, n=1000, generate=pop.model, std.lv=TRUE, lavaanfun = "sem")
getCutoff(Output.ALT.500, 0.05)
getCutoff(Output.ALT.450, 0.05)
getCutoff(Output.ALT.1000.3, 0.05)
plotCutoff(Output.ALT, 0.05)
summary(Output.ALT)
summaryPopulation(Output.ALT)
summaryParam(Output.ALT.1000.3, detail = TRUE, alpha=.05)


