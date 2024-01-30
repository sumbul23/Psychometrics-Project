##########################################################
# Seminar: Test construction applied - Summer term 2021
# Test/questionnaire evaluation and adaptation
# Test/questionnaire: Protestant Work Ethic Scale
# Student name:       Sumbul Jafri
##########################################################

# Packages
library("car")
library("Gifi")
library("psych")
library("mirt")
library("car")

# Working directory

setwd("C:/Users/HYGIA/Documents/TestAnalysis")


# Read in the data 
dat <- read.csv2("pwedat.csv", sep = "\t", header =T)
head(dat)

#removing rows with no gender information and 'other' category.
dat <- dat[dat$gender != 0 & dat$gender != 3, ] 
#dummy coding male and female as 0 and 1, respectively.
dat$gender <- recode(dat$gender, "1=0 ; 2=1")
# 0 for male and 1 for female
table(dat$gender)

#Selecting only response items
datneu <- dat[, c("Q1A","Q2A","Q3A","Q4A","Q5A","Q6A","Q7A","Q8A","Q9A","Q10A","Q11A","Q12A","Q13A","Q14A","Q15A","Q16A","Q17A","Q18A","Q19A" )]


#recoding Q9A,13A and 15A

datneu$Q9A <- recode(datneu$Q9A, "1=5; 2=4; 3=3; 4=2; 5=1")
datneu$Q13A <- recode(datneu$Q13A, "1=5; 2=4; 3=3; 4=2; 5=1" )
datneu$Q15A <- recode(datneu$Q15A,"1=5; 2=4; 3=3; 4=2; 5=1" )


# Plot item response distributions
varnames <-colnames(datneu)

v <- length(varnames) # Number of variables
n <- length(dat[,1]) # n

# Univariate Distributions

for (i in 1:v ) { 
  png(paste("His",varnames[i],".png"))
  hist(datneu[,i], breaks=20, )
  dev.off() }


##PRINCALS

prinPWE <- princals(datneu)
summary(prinPWE)
plot(prinPWE, main = "PWE Components")

#4A seems to be an outlier and it's not a reverse item.
#Loadings:Comp1  Comp2 
#Q4A      0.184 -0.983

#Importance (Variance Accounted For):
#                Comp1    Comp2
#Eigenvalues    18.0319   0.9681
#VAF            94.9048   5.0952
#Cumulative VAF 94.9000 100.0000


#Eigenvalue difference higher than 3 between the two components makes a case for unidimensionality with one outlier.
 
##EFA

datneu_efa <-polychoric(datneu)$rho
evals <- eigen(datneu_efa)$values
scree(datneu_efa, factors = FALSE)
(evals/sum(evals)*100)[1:2]
#38.707355 variance explained by first and 7.495938 explained by second factor.

#Higher than 30% variance explained by one factor again makes a case for unidimensionality. The scree plot however
# suggests four factors according to the Kaiser criterion with a strongly dominating first factor.

#parallel analysis
PA_PWE <- fa.parallel(datneu, fa = "both", cor = "poly",fm = "ml")

#Parallel analysis suggests that the number of factors =  5  and the number of components =  2 
#A factor is considered as "significant" if its eigenvalue is larger than the 95% quantile (red line) of those obtained
# from random or resampled data. In our case, we can see 2 components above the 95% quantile.

#VSS
resvss <- vss(datneu_efa, fm = "ml", n.obs = nrow(datneu), plot = TRUE)
resvss
## Very Simple Structure
# Call: vss(x = datneu_efa, fm = "ml", n.obs = nrow(datneu), plot = TRUE)
# VSS complexity 1 achieves a maximimum of 0.84  with  1  factors
# VSS complexity 2 achieves a maximimum of 0.87  with  2  factors

# The Velicer MAP achieves a minimum of 0.02  with  1  factors 
# BIC achieves a minimum of  -331.24  with  5  factors
# Sample Size adjusted BIC achieves a minimum of  -84.84  with  7  factors

## VSS is higher for 2 factors,VMAP suggests 1 factor, BIC suggests 5 and adjusted BIC suggests 7.

#Because the map value is the lowest with 1 and 3 factors. We do the following check :

fadep <- fa(datneu, 3, cor = "poly", fm = "ml")
summary(fadep) 

#Test of the hypothesis that 3 factor is sufficient.
#The degrees of freedom for the model is 117  and the objective function was  0.61 
#The number of observations was  1302  with Chi Square =  788.5  with prob <  4.1e-100 

# The root mean square of the residuals (RMSA) is  0.04 
# The df corrected root mean square of the residuals is  0.04 
# 
# Tucker Lewis Index of factoring reliability =  0.906
# RMSEA index =  0.066  and the 10 % confidence intervals are  0.062 0.071
# BIC =  -50.58
# With factor correlations of 
# ML3  ML1  ML2
# ML3 1.00 0.63 0.56
# ML1 0.63 1.00 0.60
# ML2 0.56 0.60 1.00

# The classical guidelines for RMSEA evaluations are given by Browne and Cudeck
# (1993): values smaller than 0.05 indicate good fit, 0.6-0.8 fair fit, and
# values larger than 0.10 poor fit.
#TLI and RMSEA fit  with 3 factors is better than 1 or 2 factors.

fadep1 <- fa(datneu, 1, cor = "poly", fm = "ml")
summary(fadep1) 

# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 152  and the objective function was  1.79 
# The number of observations was  1302  with Chi Square =  2315.6  with prob <  0 
# 
# The root mean square of the residuals (RMSA) is  0.07 
# The df corrected root mean square of the residuals is  0.07 
# 
# Tucker Lewis Index of factoring reliability =  0.766
# RMSEA index =  0.105  and the 10 % confidence intervals are  0.101 0.108
# BIC =  1225.51

fadep2 <- fa(datneu, 2, cor = "poly", fm = "ml")
summary(fadep2) 

# Test of the hypothesis that 2 factors are sufficient.
# The degrees of freedom for the model is 134  and the objective function was  1.17 
# The number of observations was  1302  with Chi Square =  1514.59  with prob <  2.8e-232 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.831
# RMSEA index =  0.089  and the 10 % confidence intervals are  0.085 0.093
# BIC =  553.59
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.73
# ML2 0.73 1.00


#EFA gave mixed results but we know PWE to be a unidimensional construct so we're gonna do some
# item factor analysis and check.


#IFA 
fitifa1 <- mirt(datneu, 2, verbose = FALSE)
fitifa3 <- mirt(datneu, 3, verbose = FALSE, TOL = 0.001)
anova(fitifa1, fitifa3, verbose = FALSE)

#   AIC       AICc    SABIC     HQ      BIC       logLik    X2      df    p
# 1 67543.18 67565.27 67770.63 67764.38 68132.75 -33657.59      NaN NaN NaN
# 2 66499.96 66538.76 66797.23 66789.06 67270.54 -33100.98 1113.226  35   0

#AIC and BIC are slightly lower in 3 factor solution but not significant.

# Using all these tools in combination, we conclude that there is no drastic
# unidimensionality violation in these data. Still, we obtained some hints that
# it might be slightly violated.

summary(fitifa1)

#7 and 14 are below 0.4.

summary(fitifa3)

# As indicated in the loading matrix:
# Item 1, 3, 4 to 8, 11,12,14 to 16,18 and 19 are higher on factor 1.[PWE]
# 2,9 and 15 are higher on factor 2. [leisure]
# 10,13 and 17 are higher on factor 3. [hard work]

# The problem with this questionnaire is the following: It uses 3 very
# similar = homogeneous "hard work" items and other three similar "leisure"
# items. These create "unwanted factors". All other items are more
# heterogeneous, with strong loadings onto the general PWE factor. Thus, other
# items are not "replicated" in slightly different forms in the questionnaire.

# So to finalize the one factorial structure, we will attempt to drop some items
# and continue with the IRT model and the DIF.

# [Deciding between leisure items - 2,9 and 15]


datwith2 <- datneu[, c("Q1A","Q2A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A",
                       "Q11A", "Q12A", "Q14A", "Q16A", "Q18A", "Q19A")]


letscheck2 <- factanal(datwith2, factor = 1, rotation = 'promax', scores = 'regression') 
letscheck2

# Q2A  0.571  
# 
# SS loadings      3.725

fawith2 <- fa(datwith2, 1, cor = "poly", fm = "ml")
summary(fawith2) 

#The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.852
# RMSEA index =  0.083  and the 10 % confidence intervals are  0.077 0.088
# BIC =  211.25

datwith9 <- datneu[, c("Q1A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A","Q9A", 
                       "Q11A", "Q12A", "Q14A", "Q16A", "Q18A", "Q19A")]

letscheck9 <- factanal(datwith9, factor = 1, rotation = 'promax', scores = 'regression') 
letscheck9

# Q9A  0.538  
# 
# SS loadings      3.690

fawith9 <- fa(datwith9, 1, cor = "poly", fm = "ml")
summary(fawith9) 

# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.55 
# The number of observations was  1302  with Chi Square =  709.01  with prob <  5.9e-103 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.859
# RMSEA index =  0.079  and the 10 % confidence intervals are  0.074 0.085
# BIC =  156.8

datwith15 <- datneu[, c("Q1A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A", 
                        "Q11A", "Q12A", "Q14A","Q15A", "Q16A", "Q18A", "Q19A")]

letscheck15 <- factanal(datwith15, factor = 1, rotation = 'promax', scores = 'regression') 
letscheck15

# Q15A 0.491  
# SS loadings      3.643

fawith15 <- fa(datwith15, 1, cor = "poly", fm = "ml")
summary(fawith15) 

# Factor analysis with Call: fa(r = datwith15, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.54 
# The number of observations was  1302  with Chi Square =  701.61  with prob <  1.6e-101 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.858
# RMSEA index =  0.079  and the 10 % confidence intervals are  0.074 0.084
# BIC =  149.39

## If we just look at the loadings, it would make sense to choose item 2 because it has the highest loading. But if we
# also consider Tucker Lewis Index and RMSEA fit, we have better results with items 9 or 15. (9 is a better choice because it has a good fit and
# the second highest loading which is much closer to the first highest loading of item 2.)


# [Deciding between hard work items - 10,13 and 17]

datwith10 <- datneu[, c("Q1A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A", "Q10A",
                        "Q11A", "Q12A",  "Q14A", "Q16A", "Q18A", "Q19A")]

letscheck10 <- factanal(datwith10, factor = 1, rotation = 'promax', scores = 'regression') 
letscheck10

#Q10A 0.611 
#ss loadings 3.770


fawith10 <- fa(datwith10, 1, cor = "poly", fm = "ml")
summary(fawith10)

# Factor analysis with Call: fa(r = datwith10, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.64 
# The number of observations was  1302  with Chi Square =  824.74  with prob <  1.3e-125 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.842
# RMSEA index =  0.086  and the 10 % confidence intervals are  0.081 0.092
# BIC =  272.52

datwith13 <- datneu[, c("Q1A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A", 
                        "Q11A", "Q12A", "Q13A", "Q14A", "Q16A", "Q18A", "Q19A")]

letscheck13 <- factanal(datwith13, factor = 1, rotation = 'promax', scores = 'regression') 
letscheck13

# Q13A 0.451  
# SS loadings   3.607

fawith13 <- fa(datwith13, 1, cor = "poly", fm = "ml")
summary(fawith13)
# Factor analysis with Call: fa(r = datwith13, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.53 
# The number of observations was  1302  with Chi Square =  688.35  with prob <  6e-99 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.858
# RMSEA index =  0.078  and the 10 % confidence intervals are  0.073 0.084
# BIC =  136.13



datwith17 <- datneu[, c("Q1A","Q3A","Q4A", "Q5A", "Q6A", "Q7A", "Q8A",
                        "Q11A", "Q12A","Q14A", "Q16A", "Q17A", "Q18A", "Q19A")]

letscheck17 <- factanal(datwith17, factor = 1, rotation = 'promax', scores = 'regression')
letscheck17


# Q17A 0.680
# SS loadings 3.856

fawith17 <- fa(datwith17, 1, cor = "poly", fm = "ml")
summary(fawith17)

# Factor analysis with Call: fa(r = datwith17, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.6 
# The number of observations was  1302  with Chi Square =  772.45  with prob <  2.4e-115 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.857
# RMSEA index =  0.083  and the 10 % confidence intervals are  0.078 0.089
# BIC =  220.23


## Item 17 has the highest loading but item 13 has a better fit. So, it's seems like a better choice.



##Checking for unidimensionality without certain items:

#[without 9,15 and 10,13]

dat1Is <- datneu[, c("Q1A","Q2A","Q3A", "Q4A", "Q5A", "Q6A", "Q7A", "Q8A", 
                     "Q11A", "Q12A", "Q14A", "Q16A", "Q17A", "Q18A", "Q19A")]

fainal <- fa(dat1Is, 1, cor = "poly", fm = "ml") #[with 2 and 17]
summary(fainal) 


# Factor analysis with Call: fa(r = dat1Is, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 90  and the objective function was  0.71 
# The number of observations was  1302  with Chi Square =  915.9  with prob <  6.4e-137 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.852
# RMSEA index =  0.084  and the 10 % confidence intervals are  0.079 0.089
# BIC =  270.45

#[without 2,15 and 10,17]

dat2Is <- datneu[, c("Q1A","Q3A", "Q4A", "Q5A", "Q6A", "Q7A", "Q8A", "Q9A",
                     "Q11A", "Q12A","Q13A", "Q14A", "Q16A", "Q18A", "Q19A")]

fainal2 <- fa(dat2Is, 1, cor = "poly", fm = "ml") #with 9 and 13
summary(fainal2) 
# Factor analysis with Call: fa(r = dat2Is, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 90  and the objective function was  0.61 
# The number of observations was  1302  with Chi Square =  785.57  with prob <  1.5e-111 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.857
# RMSEA index =  0.077  and the 10 % confidence intervals are  0.072 0.082
# BIC =  140.12

#[without 2,15 and 10,13]

dat3Is <- datneu[, c("Q1A","Q3A", "Q4A", "Q5A", "Q6A", "Q7A", "Q8A", "Q9A",
                     "Q11A", "Q12A", "Q14A", "Q16A", "Q17A","Q18A", "Q19A")]

fainal3 <- fa(dat3Is, 1, cor = "poly", fm = "ml") #with 9 and 17
summary(fainal3) 

# Factor analysis with Call: fa(r = dat3Is, nfactors = 1, fm = "ml", cor = "poly")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 90  and the objective function was  0.66 
# The number of observations was  1302  with Chi Square =  860.19  with prob <  5.1e-126 
# 
# The root mean square of the residuals (RMSA) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# Tucker Lewis Index of factoring reliability =  0.858
# RMSEA index =  0.081  and the 10 % confidence intervals are  0.076 0.086
# BIC =  214.74


#Best results with fainal2 -->  item 9 and 13 included. Also gives better fit and TLI than factor analysis without removing these items.
# Thus, going with unidimensionality after removing 4 items.


##CFA

library("MPsychoR")
library("lavaan")
library("eRm")

pweitems <- dat2Is   ## make a copy of the selected pwe items

fitpcm <- PCM(pweitems)
summary(fitpcm)

# Results of PCM estimation: 
#   
#   Call:  PCM(X = pweitems) 
# 
# Conditional log-likelihood: -22848.58 
# Number of iterations: 131 
# Number of parameters: 74 
# 
# Item (Category) Difficulty Parameters (eta): with 0.95 CI:
#           Estimate Std. Error lower CI upper CI
# Q1A.c2    -9.585        NaN      NaN      NaN
# Q1A.c3    -2.515        NaN      NaN      NaN
# Q1A.c4     3.956        NaN      NaN      NaN
# Q1A.c5    11.439        NaN      NaN      NaN
# Q3A.c1   -13.041        NaN      NaN      NaN
# Q3A.c2    -6.441        NaN      NaN      NaN
# Q3A.c3    -0.119        NaN      NaN      NaN
# Q3A.c4     6.264        NaN      NaN      NaN
# Q3A.c5    13.298        NaN      NaN      NaN
# Q4A.c1   -12.981        NaN      NaN      NaN
# Q4A.c2    -6.448        NaN      NaN      NaN
# Q4A.c3     0.242        NaN      NaN      NaN
# Q4A.c4     6.288        NaN      NaN      NaN
# Q4A.c5    13.250      1.448   10.412   16.088
# Q5A.c1   -13.596        NaN      NaN      NaN
# Q5A.c2    -6.734        NaN      NaN      NaN
# Q5A.c3    -0.392        NaN      NaN      NaN
# Q5A.c4     6.347        NaN      NaN      NaN
# Q5A.c5    13.390        NaN      NaN      NaN
# Q6A.c1   -13.905      6.965  -27.556   -0.254
# Q6A.c2    -6.324      6.827  -19.705    7.057
# Q6A.c3     1.432      6.708  -11.715   14.578
# Q6A.c4     8.206      6.604   -4.738   21.150
# Q6A.c5    16.130      6.519    3.354   28.907
# Q7A.c1   -13.417        NaN      NaN      NaN
# Q7A.c2    -6.328        NaN      NaN      NaN
# Q7A.c3    -0.177        NaN      NaN      NaN
# Q7A.c4     6.395        NaN      NaN      NaN
# Q7A.c5    13.246        NaN      NaN      NaN
# Q8A.c1   -13.398        NaN      NaN      NaN
# Q8A.c2    -6.705        NaN      NaN      NaN
# Q8A.c3     0.544        NaN      NaN      NaN
# Q8A.c4     6.274        NaN      NaN      NaN
# Q8A.c5    13.462        NaN      NaN      NaN
# Q9A.c1   -13.762        NaN      NaN      NaN
# Q9A.c2    -6.599        NaN      NaN      NaN
# Q9A.c3     0.945        NaN      NaN      NaN
# Q9A.c4     8.933        NaN      NaN      NaN
# Q9A.c5    16.995        NaN      NaN      NaN
# Q11A.c1  -13.968        NaN      NaN      NaN
# Q11A.c2   -7.087        NaN      NaN      NaN
# Q11A.c3    0.350        NaN      NaN      NaN
# Q11A.c4    7.069        NaN      NaN      NaN
# Q11A.c5   14.949        NaN      NaN      NaN
# Q12A.c1  -13.751        NaN      NaN      NaN
# Q12A.c2   -6.798        NaN      NaN      NaN
# Q12A.c3    0.191        NaN      NaN      NaN
# Q12A.c4    6.346        NaN      NaN      NaN
# Q12A.c5   13.432        NaN      NaN      NaN
# Q13A.c1  -13.723        NaN      NaN      NaN
# Q13A.c2   -7.223        NaN      NaN      NaN
# Q13A.c3    0.223        NaN      NaN      NaN
# Q13A.c4    6.452        NaN      NaN      NaN
# Q13A.c5   13.957        NaN      NaN      NaN
# Q14A.c1  -13.893        NaN      NaN      NaN
# Q14A.c2   -6.723        NaN      NaN      NaN
# Q14A.c3    0.195        NaN      NaN      NaN
# Q14A.c4    6.388        NaN      NaN      NaN
# Q14A.c5   13.761        NaN      NaN      NaN
# Q16A.c1  -12.070        NaN      NaN      NaN
# Q16A.c2   -5.739        NaN      NaN      NaN
# Q16A.c3    0.510        NaN      NaN      NaN
# Q16A.c4    6.417        NaN      NaN      NaN
# Q16A.c5   13.398        NaN      NaN      NaN
# Q18A.c1  -13.918        NaN      NaN      NaN
# Q18A.c2   -6.849        NaN      NaN      NaN
# Q18A.c3    0.330        NaN      NaN      NaN
# Q18A.c4    6.450        NaN      NaN      NaN
# Q18A.c5   13.701        NaN      NaN      NaN
# Q19A.c1  -13.924        NaN      NaN      NaN
# Q19A.c2   -6.941        NaN      NaN      NaN
# Q19A.c3    0.155        NaN      NaN      NaN
# Q19A.c4    6.422        NaN      NaN      NaN
# Q19A.c5   13.709        NaN      NaN      NaN
# 
# Item Easiness Parameters (beta) with 0.95 CI:
#               Estimate Std. Error lower CI upper CI
# beta Q1A.c1    16.368        NaN      NaN      NaN
# beta Q1A.c2     9.585        NaN      NaN      NaN
# beta Q1A.c3     2.515        NaN      NaN      NaN
# beta Q1A.c4    -3.956        NaN      NaN      NaN
# beta Q1A.c5   -11.439        NaN      NaN      NaN
# beta Q3A.c1    13.041        NaN      NaN      NaN
# beta Q3A.c2     6.441        NaN      NaN      NaN
# beta Q3A.c3     0.119        NaN      NaN      NaN
# beta Q3A.c4    -6.264        NaN      NaN      NaN
# beta Q3A.c5   -13.298        NaN      NaN      NaN
# beta Q4A.c1    12.981        NaN      NaN      NaN
# beta Q4A.c2     6.448        NaN      NaN      NaN
# beta Q4A.c3    -0.242        NaN      NaN      NaN
# beta Q4A.c4    -6.288        NaN      NaN      NaN
# beta Q4A.c5   -13.250      1.448  -16.088  -10.412
# beta Q5A.c1    13.596        NaN      NaN      NaN
# beta Q5A.c2     6.734        NaN      NaN      NaN
# beta Q5A.c3     0.392        NaN      NaN      NaN
# beta Q5A.c4    -6.347        NaN      NaN      NaN
# beta Q5A.c5   -13.390        NaN      NaN      NaN
# beta Q6A.c1    13.905      6.965    0.254   27.556
# beta Q6A.c2     6.324      6.827   -7.057   19.705
# beta Q6A.c3    -1.432      6.708  -14.578   11.715
# beta Q6A.c4    -8.206      6.604  -21.150    4.738
# beta Q6A.c5   -16.130      6.519  -28.907   -3.354
# beta Q7A.c1    13.417        NaN      NaN      NaN
# beta Q7A.c2     6.328        NaN      NaN      NaN
# beta Q7A.c3     0.177        NaN      NaN      NaN
# beta Q7A.c4    -6.395        NaN      NaN      NaN
# beta Q7A.c5   -13.246        NaN      NaN      NaN
# beta Q8A.c1    13.398        NaN      NaN      NaN
# beta Q8A.c2     6.705        NaN      NaN      NaN
# beta Q8A.c3    -0.544        NaN      NaN      NaN
# beta Q8A.c4    -6.274        NaN      NaN      NaN
# beta Q8A.c5   -13.462        NaN      NaN      NaN
# beta Q9A.c1    13.762        NaN      NaN      NaN
# beta Q9A.c2     6.599        NaN      NaN      NaN
# beta Q9A.c3    -0.945        NaN      NaN      NaN
# beta Q9A.c4    -8.933        NaN      NaN      NaN
# beta Q9A.c5   -16.995        NaN      NaN      NaN
# beta Q11A.c1   13.968        NaN      NaN      NaN
# beta Q11A.c2    7.087        NaN      NaN      NaN
# beta Q11A.c3   -0.350        NaN      NaN      NaN
# beta Q11A.c4   -7.069        NaN      NaN      NaN
# beta Q11A.c5  -14.949        NaN      NaN      NaN
# beta Q12A.c1   13.751        NaN      NaN      NaN
# beta Q12A.c2    6.798        NaN      NaN      NaN
# beta Q12A.c3   -0.191        NaN      NaN      NaN
# beta Q12A.c4   -6.346        NaN      NaN      NaN
# beta Q12A.c5  -13.432        NaN      NaN      NaN
# beta Q13A.c1   13.723        NaN      NaN      NaN
# beta Q13A.c2    7.223        NaN      NaN      NaN
# beta Q13A.c3   -0.223        NaN      NaN      NaN
# beta Q13A.c4   -6.452        NaN      NaN      NaN
# beta Q13A.c5  -13.957        NaN      NaN      NaN
# beta Q14A.c1   13.893        NaN      NaN      NaN
# beta Q14A.c2    6.723        NaN      NaN      NaN
# beta Q14A.c3   -0.195        NaN      NaN      NaN
# beta Q14A.c4   -6.388        NaN      NaN      NaN
# beta Q14A.c5  -13.761        NaN      NaN      NaN
# beta Q16A.c1   12.070        NaN      NaN      NaN
# beta Q16A.c2    5.739        NaN      NaN      NaN
# beta Q16A.c3   -0.510        NaN      NaN      NaN
# beta Q16A.c4   -6.417        NaN      NaN      NaN
# beta Q16A.c5  -13.398        NaN      NaN      NaN
# beta Q18A.c1   13.918        NaN      NaN      NaN
# beta Q18A.c2    6.849        NaN      NaN      NaN
# beta Q18A.c3   -0.330        NaN      NaN      NaN
# beta Q18A.c4   -6.450        NaN      NaN      NaN
# beta Q18A.c5  -13.701        NaN      NaN      NaN
# beta Q19A.c1   13.924        NaN      NaN      NaN
# beta Q19A.c2    6.941        NaN      NaN      NaN
# beta Q19A.c3   -0.155        NaN      NaN      NaN
# beta Q19A.c4   -6.422        NaN      NaN      NaN
# beta Q19A.c5  -13.709        NaN      NaN      NaN

thresholds(fitpcm)

#Design Matrix Block 1:
#       Location Threshold 1 Threshold 2 Threshold 3 Threshold 4 Threshold 5
# Q1A   2.28784   -16.36848     6.78391     7.06993     6.47113     7.48270
# Q3A   2.65969   -13.04098     6.59984     6.32221     6.38298     7.03440
# Q4A   2.65004   -12.98122     6.53362     6.68945     6.04638     6.96195
# Q5A   2.67805   -13.59596     6.86165     6.34235     6.73920     7.04299
# Q6A   3.22604   -13.90493     7.58123     7.75565     6.77405     7.92422
# Q7A   2.64924   -13.41683     7.08923     6.15079     6.57137     6.85164
# Q8A   2.69234   -13.39842     6.69382     7.24836     5.72996     7.18798
# Q9A   3.39906   -13.76189     7.16311     7.54415     7.98758     8.06233
# Q11A  2.98979   -13.96797     6.88115     7.43639     6.71935     7.88005
# Q12A  2.68641   -13.75115     6.95338     6.98876     6.15510     7.08597
# Q13A  2.79141   -13.72331     6.50068     7.44541     6.22892     7.50533
# Q14A  2.75225   -13.89338     7.17055     6.91764     6.19288     7.37359
# Q16A  2.67954   -12.06998     6.33078     6.24905     5.90714     6.98073
# Q18A  2.74015   -13.91841     7.06943     7.17942     6.11938     7.25093
# Q19A  2.74186   -13.92385     6.98244     7.09646     6.26655     7.28770

plotPImap(fitpcm, latdim = "PWE",
          main = "Person-Item Map PWE")

plotICC(fitpcm, item.subset = "all" , empICC = NULL, empCI = NULL,
        mplot = NULL, xlim = c(2, 10), ylim = c(0, 1),
        xlab = "Latent Dimension", ylab = "Probability to Solve", main=NULL,
        col = NULL, lty = 1, legpos = "left", ask = TRUE)

#A steep ICC indicates that the item discriminates well, a shallow slope, that it discriminates poorly. 

# Assessing Dimensionality: Martin-Loef Test
MLoef(fitpcm, splitcr = "median")

# Martin-Loef-Test (split criterion: median)
# LR-value: 717.843 
# Chi-square df: 1399 
# p-value: 1 

# When a negative number object is put through a log or even root r function the result will be the nan value 
# triggering a warning message. This results from the fact the default functions are not designed to deal with imaginary numbers.
# P value close to 1 suggests no difference between the dimensions other than due to chance.
#Therfore, supporting unidimensionality of the scale.


##DIF 
library("lordif")
library("MPsychoR")

cdi <- dat2Is
cdiDIF <- lordif(cdi, dat$gender, criterion = "Chisqr")

# Iteration: 31, Log-Lik: -27375.350, Max-Change: 0.00008
# (mirt) | Iteration: 1, 5 items flagged for DIF (5,7,9,14,15)
# Iteration: 175, Log-Lik: -27323.699, Max-Change: 0.00009
# (mirt) | Iteration: 2, 6 items flagged for DIF (1,5,7,9,14,15)
# Iteration: 199, Log-Lik: -27317.947, Max-Change: 0.00010
# (mirt) | Iteration: 3, 6 items flagged for DIF (1,5,7,9,14,15)

#(1,5,7,9,14,15) have been flagged as biased.

cdiDIF$stats
 # 1  has uniform dif, m1 vs m2 is significant whereas m3 vs m2 is not
 # 5  has non uniform dif, all p-values are significant
 # 7  has uniform dif, m1 vs m2 is significant whereas m3 vs m2 is not
 # 9  has uniform dif, m1 vs m2 is significant whereas m3 vs m2 is not
 # 14 has uniform dif, m1 vs m2 is significant whereas m3 vs m2 is not
 # 15 has non uniform dif, m1 vs m2 is not significant but m3 vs m2 is
 

#As an alternative to the LR-test, we can also look at differences in pseudo-R2 values (e.g., McFadden’s R2) 
#with corresponding effect sizes: < 0.13 “negligible,” 0.13–0.26 “moderate,” and > 0.26 “large” (Zumbo, 1999).

plot(cdiDIF ,labels = c("Male", "Female"))

cdiDIF$ipar.sparse
#             a         cb1        cb2         cb3       cb4
# I2    1.1423562 -2.43796167 -1.5911257 -0.68487386 0.6853274
# I3    1.2735611 -2.24388248 -1.4042804 -0.77741347 0.5107051
# I4    0.8380582 -2.53006860 -1.5358502 -0.37148124 1.0940402
# I6    0.6115153 -3.36978830 -2.3116593 -0.88152114 0.9832280
# I8    1.3765039 -0.49528003  0.6651680  1.63521773 2.5739204
# I10   1.1123178 -1.78789403 -0.9783463 -0.42480069 0.8449737
# I11   1.0419509 -1.90403448 -0.6534065 -0.07589140 1.4008001
# I12   0.4561213 -3.29539069 -1.7760561 -0.56930412 2.3520772
# I13   1.2782364 -2.74153059 -1.9074192 -1.12361303 0.3800455
# I1.1  1.5368880 -1.50005202 -0.6278350 -0.08387754 1.0205275
# I1.2  1.5408597 -1.23832160 -0.4282795  0.16644608 1.2664313
# I5.1  1.6467629 -0.50719609  0.3115445  0.71721579 1.6961923
# I5.2  2.1172810 -0.09452853  0.5287540  0.91028282 1.7271477
# I7.1  0.8962487 -2.60276960 -1.6380596 -0.99492011 0.7219905
# I7.2  0.9209541 -2.16451578 -1.0025939 -0.57278387 1.1465351
# I9.1  1.6385561 -1.12764133 -0.2331784  0.34547151 1.4280845
# I9.2  1.7730465 -0.79719862  0.1092208  0.59037617 1.6698322
# I14.1 0.9246865 -1.49326497 -0.7122440 -0.10521894 1.3477777
# I14.2 1.0804364 -1.87775995 -0.8946857 -0.42540465 0.9258045
# I15.1 2.3434549 -1.05198073 -0.4622459 -0.03874633 0.7300390
# I15.2 1.7616173 -1.26554826 -0.6009944 -0.19712457 0.9150562


# Q1	Most people spend too much time in unprofitable amusements. 
# 5Q6	Most people who don’t succeed in life are just plain lazy. 
# 7Q8	I often feel I would be more successful if I sacrificed certain pleasures. 
# 9Q11	People who fail at a job have usually not tried hard enough. 
# 14Q18	I feel uneasy when there is little work for me to do. 
# 15Q19	A distaste for hard work usually reflects a weakness of character.

#5 and 15 have a noticeable difference in estimates, rest of them are very negligent differences

ppar <- cdiDIF$calib.sparse$theta
head(ppar)

MC <- montecarlo(cdiDIF, alpha = 0.01, nr = 1000)
print(MC)
summary(MC)
plot(MC)















