 *Multivariate Analysis for the Behavioral Sciences,*  
 **Examples of Chapter 9:**  
 **Analysis of Longitudinal Data II: Linear Mixed Effects Models for
Normal Response Variables**
================
“Kimmo Vehkalahti, Brian S. Everitt; edited by C.-F. Sheu”
07 September, 2019

## Example: “Beat the Blues (BtB)”

Longitudinal data from a clinical trial of an interactive, multimedia
program known as “Beat the Blues” designed to deliver cognitive
behavioral therapy to depressed patients via a computer terminal.
Patients with depression recruited in primary care were randomised to
either the Beating the Blues program, or to “Treatment as Usual (TAU)”.

A number of outcome measures were used in the trial, but here we
concentrate on the Beck Depression Inventory II (BDI; Beck et al.,
1996). Measurements on this variable were made on the following five
occasions:

  - Prior to treatment,

  - 2, 4, 6, and 8 months after treatment began.

Two additional explanatory variables are also available for each
patient: the first, drug, is whether the patient was taking
antidepressant drugs (yes or no), and the second, length, is the length
of the current episode of depression categorized into less than six
months (\<6m) or more than six months (\>6m). The NAs (not available) in
Table 9.7 indicate where a protocol-specified measurement of the BDI was
not made; here, all the NAs are due to patients dropping out of the
study. How dropouts might affect the results obtained from the analysis
of the data will be discussed later in the chapter. The main question of
interest here is to estimate the treatment effect of the BtB program.

``` r
# check to see if the pacman package is there
# if not install it and then use it manage packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lme4)
```

## Table 9.7: Patients in Each Treatment Group of the “Beat the Blues” Clinical Trial of CBT for Depression

``` r
# file location
fLoc <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BtB.txt"
```

``` r
# data input
BtB <- read.table(fLoc, header = TRUE, sep = '\t')
```

``` r
# to make sure that the factor levels are logical (esp. Treatment):
BtB <- within(BtB, {
  Drug <- factor(Drug, levels = c("No", "Yes")) # default
  Length <- factor(Length, levels = c("<6m", ">6m")) # default
  Treatment <- factor(Treatment, levels = c("TAU", "BtheB")) # NOT default!
})
```

``` r
glimpse(BtB); head(BtB); tail(BtB)
```

    Observations: 100
    Variables: 9
    $ Subject   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    $ Drug      <fct> No, Yes, Yes, No, Yes, Yes, Yes, No, Yes, Yes, No, Yes…
    $ Length    <fct> >6m, >6m, <6m, >6m, >6m, <6m, <6m, >6m, <6m, >6m, >6m,…
    $ Treatment <fct> TAU, BtheB, TAU, BtheB, BtheB, BtheB, TAU, TAU, BtheB,…
    $ BDIpre    <int> 29, 32, 25, 21, 26, 7, 17, 20, 18, 20, 30, 49, 26, 30,…
    $ BDI2m     <int> 2, 16, 20, 17, 23, 0, 7, 20, 13, 5, 32, 35, 27, 26, 13…
    $ BDI4m     <int> 2, 24, NA, 16, NA, 0, 7, 21, 14, 5, 24, NA, 23, 36, 13…
    $ BDI6m     <int> NA, 17, NA, 10, NA, 0, 3, 19, 20, 8, 12, NA, NA, 27, 1…
    $ BDI8m     <int> NA, 20, NA, 9, NA, 0, 7, 13, 11, 12, 2, NA, NA, 22, 23…

``` 
  Subject Drug Length Treatment BDIpre BDI2m BDI4m BDI6m BDI8m
1       1   No    >6m       TAU     29     2     2    NA    NA
2       2  Yes    >6m     BtheB     32    16    24    17    20
3       3  Yes    <6m       TAU     25    20    NA    NA    NA
4       4   No    >6m     BtheB     21    17    16    10     9
5       5  Yes    >6m     BtheB     26    23    NA    NA    NA
6       6  Yes    <6m     BtheB      7     0     0     0     0
```

``` 
    Subject Drug Length Treatment BDIpre BDI2m BDI4m BDI6m BDI8m
95       95   No    >6m     BtheB     16    11     4     2     3
96       96  Yes    >6m     BtheB     16    16    10    10     8
97       97  Yes    <6m       TAU     28    NA    NA    NA    NA
98       98   No    >6m     BtheB     11    22     9    11    11
99       99   No    <6m       TAU     13     5     5     0     6
100     100  Yes    <6m       TAU     43    NA    NA    NA    NA
```

Note that the data are stored in the wide form, i.e., repeated
measurments are represented by additional columns in the data frame.

## Figure 9.5

``` r
# Convert data to long form, including the baseline BDI measurement:
BtBL0 <- gather(BtB, key = Visit, value = BDI, BDIpre, BDI2m, BDI4m, BDI6m, BDI8m)
```

``` r
glimpse(BtBL0); head(BtBL0); tail(BtBL0)
```

    Observations: 500
    Variables: 6
    $ Subject   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    $ Drug      <fct> No, Yes, Yes, No, Yes, Yes, Yes, No, Yes, Yes, No, Yes…
    $ Length    <fct> >6m, >6m, <6m, >6m, >6m, <6m, <6m, >6m, <6m, >6m, >6m,…
    $ Treatment <fct> TAU, BtheB, TAU, BtheB, BtheB, BtheB, TAU, TAU, BtheB,…
    $ Visit     <chr> "BDIpre", "BDIpre", "BDIpre", "BDIpre", "BDIpre", "BDI…
    $ BDI       <int> 29, 32, 25, 21, 26, 7, 17, 20, 18, 20, 30, 49, 26, 30,…

``` 
  Subject Drug Length Treatment  Visit BDI
1       1   No    >6m       TAU BDIpre  29
2       2  Yes    >6m     BtheB BDIpre  32
3       3  Yes    <6m       TAU BDIpre  25
4       4   No    >6m     BtheB BDIpre  21
5       5  Yes    >6m     BtheB BDIpre  26
6       6  Yes    <6m     BtheB BDIpre   7
```

``` 
    Subject Drug Length Treatment Visit BDI
495      95   No    >6m     BtheB BDI8m   3
496      96  Yes    >6m     BtheB BDI8m   8
497      97  Yes    <6m       TAU BDI8m  NA
498      98   No    >6m     BtheB BDI8m  11
499      99   No    <6m       TAU BDI8m   6
500     100  Yes    <6m       TAU BDI8m  NA
```

``` r
# set black and white theme
ot <- theme_set(theme_bw())
```

``` r
ggplot(BtBL0, aes(x = factor(Visit), y = BDI, fill = Treatment)) +
  geom_boxplot() + 
  facet_grid(Treatment ~., labeller = label_parsed) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "none") + 
  # BDIpre first!
  scale_x_discrete(name = "", 
                   limits = c("BDIpre", "BDI2m", "BDI4m", "BDI6m", "BDI8m")) + 
  scale_fill_grey(start = 1, end = 1) +
  labs(y = "Beck Depression Inventory II score")
```

<img src="Chapter-09-examples2_files/figure-gfm/fig9.5-1.png" style="display: block; margin: auto;" />

## Figure 9.6

``` r
pairs(BtB[, -c(1:4)], cex = 0.8, cex.labels = 1.0)
```

<img src="Chapter-09-examples2_files/figure-gfm/fig9.6-1.png" style="display: block; margin: auto;" />

## Table 9.8

``` r
# Convert data to long form for the analyses, adding Time:
BtBL <- gather(BtB, key = Visit, value = BDI, BDI2m, BDI4m, BDI6m, BDI8m) %>%
  mutate(Time = as.integer(substr(Visit, 4, 4))) 
```

``` r
glimpse(BtBL); head(BtBL); tail(BtBL)
```

    Observations: 400
    Variables: 8
    $ Subject   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    $ Drug      <fct> No, Yes, Yes, No, Yes, Yes, Yes, No, Yes, Yes, No, Yes…
    $ Length    <fct> >6m, >6m, <6m, >6m, >6m, <6m, <6m, >6m, <6m, >6m, >6m,…
    $ Treatment <fct> TAU, BtheB, TAU, BtheB, BtheB, BtheB, TAU, TAU, BtheB,…
    $ BDIpre    <int> 29, 32, 25, 21, 26, 7, 17, 20, 18, 20, 30, 49, 26, 30,…
    $ Visit     <chr> "BDI2m", "BDI2m", "BDI2m", "BDI2m", "BDI2m", "BDI2m", …
    $ BDI       <int> 2, 16, 20, 17, 23, 0, 7, 20, 13, 5, 32, 35, 27, 26, 13…
    $ Time      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …

``` 
  Subject Drug Length Treatment BDIpre Visit BDI Time
1       1   No    >6m       TAU     29 BDI2m   2    2
2       2  Yes    >6m     BtheB     32 BDI2m  16    2
3       3  Yes    <6m       TAU     25 BDI2m  20    2
4       4   No    >6m     BtheB     21 BDI2m  17    2
5       5  Yes    >6m     BtheB     26 BDI2m  23    2
6       6  Yes    <6m     BtheB      7 BDI2m   0    2
```

``` 
    Subject Drug Length Treatment BDIpre Visit BDI Time
395      95   No    >6m     BtheB     16 BDI8m   3    8
396      96  Yes    >6m     BtheB     16 BDI8m   8    8
397      97  Yes    <6m       TAU     28 BDI8m  NA    8
398      98   No    >6m     BtheB     11 BDI8m  11    8
399      99   No    <6m       TAU     13 BDI8m   6    8
400     100  Yes    <6m       TAU     43 BDI8m  NA    8
```

``` r
BtB_fit0 <- lm(BDI ~ BDIpre + Time + Treatment + Drug + Length, data = BtBL, na.action = na.omit)
summary(BtB_fit0)
```

``` 

Call:
lm(formula = BDI ~ BDIpre + Time + Treatment + Drug + Length, 
    data = BtBL, na.action = na.omit)

Residuals:
    Min      1Q  Median      3Q     Max 
-24.202  -5.312   0.011   5.295  27.778 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)
(Intercept)      7.8831     1.7805    4.43  1.4e-05
BDIpre           0.5724     0.0549   10.43  < 2e-16
Time            -0.9608     0.2326   -4.13  4.8e-05
TreatmentBtheB  -3.3540     1.0983   -3.05   0.0025
DrugYes         -3.5460     1.1447   -3.10   0.0022
Length>6m        1.7531     1.1085    1.58   0.1149

Residual standard error: 8.65 on 274 degrees of freedom
  (120 observations deleted due to missingness)
Multiple R-squared:  0.398, Adjusted R-squared:  0.387 
F-statistic: 36.2 on 5 and 274 DF,  p-value: <2e-16
```

## Table 9.9

``` r
BtB_fit1 <- lme4::lmer(BDI ~ BDIpre + Time + Treatment + Drug + Length + (1 | Subject), 
                 data = BtBL, na.action = na.omit)
summary(BtB_fit1)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: BDI ~ BDIpre + Time + Treatment + Drug + Length + (1 | Subject)
       Data: BtBL
    
    REML criterion at convergence: 1866.1
    
    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -2.750 -0.475 -0.093  0.400  3.738 
    
    Random effects:
     Groups   Name        Variance Std.Dev.
     Subject  (Intercept) 51.4     7.17    
     Residual             25.3     5.03    
    Number of obs: 280, groups:  Subject, 97
    
    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)      5.9215     2.3059    2.57
    BDIpre           0.6389     0.0796    8.03
    Time            -0.7135     0.1466   -4.87
    TreatmentBtheB  -2.3590     1.7084   -1.38
    DrugYes         -2.7889     1.7659   -1.58
    Length>6m        0.2381     1.6754    0.14
    
    Correlation of Fixed Effects:
                (Intr) BDIpre Time   TrtmBB DrugYs
    BDIpre      -0.679                            
    Time        -0.258  0.023                     
    TretmntBthB -0.389  0.121  0.022              
    DrugYes     -0.072 -0.236 -0.025 -0.323       
    Length>6m   -0.239 -0.241 -0.042  0.002  0.158

``` r
BtB_fit2 <- lme4::lmer(BDI ~ BDIpre + Time + Treatment + Drug + Length + (Time | Subject), 
                 data = BtBL, na.action = na.omit)
```

    Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    control$checkConv, : Model failed to converge with max|grad| = 0.00231089
    (tol = 0.002, component 1)

``` r
summary(BtB_fit2)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: 
    BDI ~ BDIpre + Time + Treatment + Drug + Length + (Time | Subject)
       Data: BtBL
    
    REML criterion at convergence: 1865.2
    
    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -2.327 -0.464 -0.081  0.369  3.540 
    
    Random effects:
     Groups   Name        Variance Std.Dev. Corr 
     Subject  (Intercept) 50.562   7.111         
              Time         0.232   0.481    -0.09
     Residual             23.866   4.885         
    Number of obs: 280, groups:  Subject, 97
    
    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)      5.9439     2.2987    2.59
    BDIpre           0.6444     0.0793    8.13
    Time            -0.6995     0.1562   -4.48
    TreatmentBtheB  -2.4859     1.7078   -1.46
    DrugYes         -2.8875     1.7648   -1.64
    Length>6m        0.0972     1.6724    0.06
    
    Correlation of Fixed Effects:
                (Intr) BDIpre Time   TrtmBB DrugYs
    BDIpre      -0.680                            
    Time        -0.250  0.019                     
    TretmntBthB -0.388  0.120  0.020              
    DrugYes     -0.076 -0.233 -0.021 -0.324       
    Length>6m   -0.244 -0.237 -0.036  0.000  0.159
    convergence code: 0
    Model failed to converge with max|grad| = 0.00231089 (tol = 0.002, component 1)

``` r
anova(BtB_fit2, BtB_fit1)
```

    Data: BtBL
    Models:
    BtB_fit1: BDI ~ BDIpre + Time + Treatment + Drug + Length + (1 | Subject)
    BtB_fit2: BDI ~ BDIpre + Time + Treatment + Drug + Length + (Time | Subject)
             Df  AIC  BIC logLik deviance Chisq Chi Df Pr(>Chisq)
    BtB_fit1  8 1887 1916   -935     1871                        
    BtB_fit2 10 1890 1926   -935     1870  0.82      2       0.66

## Session information
