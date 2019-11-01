**Exercises of Analysis of Longitudinal Data III**
================
C.-P. Cheng and C.-F. Sheu
01 November, 2019

# Exercises

``` r
# load packages for use later assuming pacman is available
pacman::p_load(gee, lme4, tidyverse)
```

## Problem 1

For a poll of a random sample of 1,600 voting-age British citizens, 794
indicated approval of the Prime Minister’s performance in office for the
first and the follow-up survey six months later. Of these same 1,600
people, 150 indicated approval in the first survey but disapproval in
the second; 86 people indicated disapproval first then approval later;
570 people indicated disapproval both times. Four different models are
used to account for the data set with the R codes given below. Identify
and explain the similarities and differences in the output for each of
the four different models.

``` r
# construct the data set
n <- c(794, 150, 86, 570)
tot <- sum(n)
dta <- data.frame(ID = rep(1:tot, rep(2, tot)),
                  Time = rep(c(1,2), tot),
                  Resp = c(rep(1, n[1]*2),
                           rep(c(1, 0), n[2]),
                           rep(c(0, 1), n[3]),
                           rep(0, n[4]*2)))
```

``` r
# check the first 6 lines of data
head(dta)
```

``` 
  ID Time Resp
1  1    1    1
2  1    2    1
3  2    1    1
4  2    2    1
5  3    1    1
6  3    2    1
```

``` r
with(dta, table(Time, Resp)) %>% addmargins(., 2)
```

``` 
    Resp
Time    0    1  Sum
   1  656  944 1600
   2  720  880 1600
```

``` r
# approval ratings by time of survey
with(dta, prop.table(table(Time, Resp), 1))
```

``` 
    Resp
Time    0    1
   1 0.41 0.59
   2 0.45 0.55
```

``` r
summary(glm(Resp ~ as.factor(Time), data = dta, family="binomial"))
```

``` 

Call:
glm(formula = Resp ~ as.factor(Time), family = "binomial", data = dta)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
 -1.34   -1.26    1.03    1.09    1.09  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)
(Intercept)        0.3640     0.0508    7.16    8e-13
as.factor(Time)2  -0.1633     0.0715   -2.28    0.022

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4373.2  on 3199  degrees of freedom
Residual deviance: 4368.0  on 3198  degrees of freedom
AIC: 4372

Number of Fisher Scoring iterations: 4
```

``` r
summary(gee(Resp ~ as.factor(Time), data = dta, id = ID, family="binomial"))
```

``` 
     (Intercept) as.factor(Time)2 
         0.36397         -0.16329 
```

``` 

 GEE:  GENERALIZED LINEAR MODELS FOR DEPENDENT DATA
 gee S-function, version 4.13 modified 98/01/27 (1998) 

Model:
 Link:                      Logit 
 Variance to Mean Relation: Binomial 
 Correlation Structure:     Independent 

Call:
gee(formula = Resp ~ as.factor(Time), id = ID, data = dta, family = "binomial")

Summary of Residuals:
   Min     1Q Median     3Q    Max 
 -0.59  -0.55   0.41   0.45   0.45 


Coefficients:
                 Estimate Naive S.E. Naive z Robust S.E. Robust z
(Intercept)       0.36397   0.050846  7.1582    0.050830   7.1604
as.factor(Time)2 -0.16329   0.071499 -2.2839    0.039027  -4.1842

Estimated Scale Parameter:  1.0006
Number of Iterations:  1

Working Correlation
     [,1] [,2]
[1,]    1    0
[2,]    0    1
```

``` r
summary(gee(Resp ~ as.factor(Time), data = dta, id = ID, family="binomial",
            corstr = "exchangeable"))
```

``` 
     (Intercept) as.factor(Time)2 
         0.36397         -0.16329 
```

``` 

 GEE:  GENERALIZED LINEAR MODELS FOR DEPENDENT DATA
 gee S-function, version 4.13 modified 98/01/27 (1998) 

Model:
 Link:                      Logit 
 Variance to Mean Relation: Binomial 
 Correlation Structure:     Exchangeable 

Call:
gee(formula = Resp ~ as.factor(Time), id = ID, data = dta, family = "binomial", 
    corstr = "exchangeable")

Summary of Residuals:
   Min     1Q Median     3Q    Max 
 -0.59  -0.55   0.41   0.45   0.45 


Coefficients:
                 Estimate Naive S.E. Naive z Robust S.E. Robust z
(Intercept)       0.36397   0.050846  7.1582    0.050830   7.1604
as.factor(Time)2 -0.16329   0.039010 -4.1860    0.039027  -4.1842

Estimated Scale Parameter:  1.0006
Number of Iterations:  1

Working Correlation
        [,1]    [,2]
[1,] 1.00000 0.70237
[2,] 0.70237 1.00000
```

``` r
summary(glmer(Resp ~ as.factor(Time) + (1 | ID), data = dta, family="binomial"))
```

    Generalized linear mixed model fit by maximum likelihood (Laplace
      Approximation) [glmerMod]
     Family: binomial  ( logit )
    Formula: Resp ~ as.factor(Time) + (1 | ID)
       Data: dta
    
         AIC      BIC   logLik deviance df.resid 
      3693.6   3711.8  -1843.8   3687.6     3197 
    
    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -1.179 -0.382  0.228  0.282  1.047 
    
    Random effects:
     Groups Name        Variance Std.Dev.
     ID     (Intercept) 14.4     3.8     
    Number of obs: 3200, groups:  ID, 1600
    
    Fixed effects:
                     Estimate Std. Error z value Pr(>|z|)
    (Intercept)         1.177      0.162    7.24  4.3e-13
    as.factor(Time)2   -0.422      0.116   -3.65  0.00027
    
    Correlation of Fixed Effects:
                (Intr)
    as.fctr(T)2 -0.409

## Problem 2

The Law School Admission Test (LSAT) consists of a number of dichotomous
items, which can be answered correctly or incorrectly. These items are
constructed so that a person who scores high on the test is more likely
to do well in a law school. The data set here contains answers by 1,000
persons to 5 test items.

The data set is available as a data frame object *LSAT* in the R package
*ltm*. Fit the Rasch model to the data set.

``` r
# install and load package assuming pacman package is available
pacman::p_load(ltm)
```

``` r
# load data 
data(LSAT)
# check document on data
?LSAT
```

``` r
# make a copy of the orignal dataset
dta <- LSAT
```

``` r
# show top and bottom 9 lines of data frame
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
  Item 1 Item 2 Item 3 Item 4 Item 5
1      0      0      0      0      0
2      0      0      0      0      0
3      0      0      0      0      0
4      0      0      0      0      1
5      0      0      0      0      1
6      0      0      0      0      1
7      0      0      0      0      1
8      0      0      0      0      1
9      0      0      0      0      1
```

``` 
     Item 1 Item 2 Item 3 Item 4 Item 5
992       1      1      1      1      1
993       1      1      1      1      1
994       1      1      1      1      1
995       1      1      1      1      1
996       1      1      1      1      1
997       1      1      1      1      1
998       1      1      1      1      1
999       1      1      1      1      1
1000      1      1      1      1      1
```

``` r
# create person ID
dta$ID <- factor(paste0("P", 1000+(1:dim(dta)[1])))
```

``` r
# wide to long format
dtaL <- dta %>% gather(Item, Answer, 1:5) %>% arrange(ID)
```

``` r
# proportion of correct answer for each item
with(dtaL, table(Item, Answer))
```

``` 
        Answer
Item       0   1
  Item 1  76 924
  Item 2 291 709
  Item 3 447 553
  Item 4 237 763
  Item 5 130 870
```

## Problem 3

An intervention study on problematic drinking in college students.
Alcohol-related problems, as measured by the Rutgers Alcohol Problem
Index (RAPI), were recorded across 2 years (5 time points, time in
months) for a sample of 881 students.

Use the RAPI data set and modify the R codes given in the course
materials to create suitable graphics of the data and the analyze the
data with an appropriate
model.

``` r
fLoc <- "https://www.researchgate.net/profile/David_Atkins2/publication/259182857_RAPIFinal_--_Data_for_Long_Count_Reg_tutorial/data/00b7d52a34018702a3000000/RAPIFinal.csv"
```

``` r
dta <- read.csv(fLoc)
```

``` r
str(dta)
```

    'data.frame':   3616 obs. of  4 variables:
     $ id    : int  1 1 1 2 2 2 2 2 3 3 ...
     $ rapi  : int  0 0 0 3 6 5 4 5 9 1 ...
     $ gender: int  1 1 1 0 0 0 0 0 1 1 ...
     $ time  : int  0 6 18 0 6 12 18 24 0 12 ...

``` r
# recode the gender variable
dta$gender <- as.factor(ifelse(dta$gender == 1, "M", "F"))
```

``` r
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
  id rapi gender time
1  1    0      M    0
2  1    0      M    6
3  1    0      M   18
4  2    3      F    0
5  2    6      F    6
6  2    5      F   12
7  2    4      F   18
8  2    5      F   24
9  3    9      M    0
```

``` 
      id rapi gender time
3608 817   20      F    0
3609 817   10      F    6
3610 817    1      F   12
3611 817    1      F   18
3612 817    1      F   24
3613 818   12      M    0
3614 818    8      M    6
3615 818   11      M   12
3616 818    8      M   18
```

## References

Agresti, A. (2007). *An Introduction to Categorical Data Analysis*.
Wiley.

Atkins, D.C., Baldwin, S.A., Zheng, C., Gallop, R.J., & Neighbors, C.
(2013). A tutorial on count regression and zero-altered count models for
longitudinal substance use data. *Psychology of Addictive Behavior*.
27(1), 166-177.

Bock, R., & Lieberman, M. (1970). Fitting a response model for n
dichotomously scored items. *Psychometrika*, 35, 179–197.
