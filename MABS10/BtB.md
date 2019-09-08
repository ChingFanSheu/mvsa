*Multivariate Analysis for the Behavioral Sciences,*  
**Examples of Chapter 10:**  
**Analysis of Longitudinal Data III: Non-Normal Responses**
================
“Kimmo Vehkalahti, Brian S. Everitt; edited by C.-F. Sheu”
08 September, 2019

``` r
# check to see if the pacman package is there
# if not install it and then use it manage packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, geepack)
```

## Example: Beat the Blues Revisited

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

``` r
# Convert data to long form for the analyses, adding Time:
# important to order the data with respect to the subjects:
BtBL <- gather(BtB, key = Visit, value = BDI, BDI2m, BDI4m, BDI6m, BDI8m) %>%
  mutate(Time = as.integer(substr(Visit, 4, 4))) %>%
  arrange(Subject)
```

## Table 10.3

``` r
# independence correlation structure
BtB_gee1 <- geepack::geeglm(BDI ~ BDIpre + Time + Treatment + Drug + Length,
                     id = Subject, 
                     data = BtBL, 
                     family = gaussian, 
                     corstr = "independence", std.err="san.se")
```

``` r
summary(BtB_gee1)
```

``` 

Call:
geepack::geeglm(formula = BDI ~ BDIpre + Time + Treatment + Drug + 
    Length, family = gaussian, data = BtBL, id = Subject, corstr = "independence", 
    std.err = "san.se")

 Coefficients:
               Estimate Std.err  Wald Pr(>|W|)
(Intercept)      7.8831  2.1997 12.84  0.00034
BDIpre           0.5724  0.0885 41.80  1.0e-10
Time            -0.9608  0.1769 29.50  5.6e-08
TreatmentBtheB  -3.3540  1.7139  3.83  0.05036
DrugYes         -3.5460  1.7307  4.20  0.04047
Length>6m        1.7531  1.4195  1.53  0.21684

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)     73.3    10.4

Correlation: Structure = independenceNumber of clusters:   97   Maximum cluster size: 4 
```

## Table 10.4

``` r
# exchangeable correlation structure
BtB_gee2 <- geepack::geeglm(BDI ~ BDIpre + Time + Treatment + Drug + Length,
                     id = Subject, 
                     data = BtBL, 
                     family = gaussian, 
                     corstr = "exchangeable", std.err="san.se")
```

``` r
summary(BtB_gee2)
```

``` 

Call:
geepack::geeglm(formula = BDI ~ BDIpre + Time + Treatment + Drug + 
    Length, family = gaussian, data = BtBL, id = Subject, corstr = "exchangeable", 
    std.err = "san.se")

 Coefficients:
               Estimate Std.err  Wald Pr(>|W|)
(Intercept)      5.8804  2.1069  7.79   0.0053
BDIpre           0.6402  0.0793 65.19  6.7e-16
Time            -0.7070  0.1540 21.08  4.4e-06
TreatmentBtheB  -2.3327  1.6621  1.97   0.1605
DrugYes         -2.7722  1.6482  2.83   0.0926
Length>6m        0.2042  1.4808  0.02   0.8903

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)     75.5    10.7

Correlation: Structure = exchangeable  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha    0.695   0.111
Number of clusters:   97   Maximum cluster size: 4 
```

## Auto-regressive lag-one correlation structure

Not shown in the
textbook

``` r
BtB_gee3 <- geepack::geeglm(BDI ~ BDIpre + Time + Treatment + Drug + Length,
                     id = Subject, 
                     data = BtBL, 
                     family = gaussian, 
                     corstr = "ar1", std.err="san.se")
```

``` r
summary(BtB_gee3)
```

``` 

Call:
geepack::geeglm(formula = BDI ~ BDIpre + Time + Treatment + Drug + 
    Length, family = gaussian, data = BtBL, id = Subject, corstr = "ar1", 
    std.err = "san.se")

 Coefficients:
               Estimate Std.err  Wald Pr(>|W|)
(Intercept)      6.6196  2.2015  9.04   0.0026
BDIpre           0.5956  0.0812 53.80  2.2e-13
Time            -0.7357  0.1604 21.03  4.5e-06
TreatmentBtheB  -2.5093  1.6482  2.32   0.1279
DrugYes         -2.3661  1.6259  2.12   0.1456
Length>6m        0.7102  1.5141  0.22   0.6390

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)       75    10.7

Correlation: Structure = ar1  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha    0.799  0.0814
Number of clusters:   97   Maximum cluster size: 4 
```

## Unstructured correlation structure

Not shown in the
textbook

``` r
BtB_gee4 <- geepack::geeglm(BDI ~ BDIpre + Time + Treatment + Drug + Length,
                     id = Subject, 
                     data = BtBL, 
                     family = gaussian, 
                     corstr = "unstructured", std.err="san.se")
```

``` r
summary(BtB_gee4)
```

``` 

Call:
geepack::geeglm(formula = BDI ~ BDIpre + Time + Treatment + Drug + 
    Length, family = gaussian, data = BtBL, id = Subject, corstr = "unstructured", 
    std.err = "san.se")

 Coefficients:
               Estimate Std.err  Wald Pr(>|W|)
(Intercept)      6.0681  2.1228  8.17   0.0043
BDIpre           0.6307  0.0798 62.43  2.8e-15
Time            -0.7061  0.1547 20.83  5.0e-06
TreatmentBtheB  -2.3772  1.6530  2.07   0.1504
DrugYes         -2.7227  1.6379  2.76   0.0964
Length>6m        0.2178  1.4749  0.02   0.8826

Estimated Scale Parameters:
            Estimate Std.err
(Intercept)     75.4    10.7

Correlation: Structure = unstructured  Link = identity 

Estimated Correlation Parameters:
          Estimate Std.err
alpha.1:2    0.677   0.111
alpha.1:3    0.699   0.133
alpha.1:4    0.651   0.152
alpha.2:3    0.741   0.150
alpha.2:4    0.638   0.136
alpha.3:4    0.749   0.125
Number of clusters:   97   Maximum cluster size: 4 
```

## References

Højsgaard, S. Halekoh, U., & Yan, J. (2006). [The R Package geepack for
Generalized Estimating
Equations](https://www.jstatsoft.org/article/view/v015i02) *Journal of
Statistical Software*, 15(2), 1-11.

Sheu, C.-F. (2000). [Regression analysis of correlated binary
outcomes.](https://link.springer.com/content/pdf/10.3758/BF03207794.pdf)
*Behavior Research Methods, Instruments, & Computers*, 32(2), 269-273.

## Session information
