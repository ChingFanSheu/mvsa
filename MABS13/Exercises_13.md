**Exercises: Principal Components Analysis**
================
C.-P. Cheng and C.-F. Sheu
14 November, 2019

# Exercises

## Problem 1

Four hundred and eighty seniors were tested on three mental ability
measures and three educational motivation sub-scales. It is of interest
to find out whether the resulting variability in the 480 by 6 observed
data matrix (standardized) could be reduced to a few principal
components. Suppressing of any component weights that are lower than
.20, how would you interpret the first two principal components?

``` r
# get a data set from a zip file over the Internet
temp <- tempfile()
download.file("http://s3-euw1-ap-pe-ws4-cws-documents.ri-prod.s3.amazonaws.com/9780805863758/Data_TF.zip", temp)
dta <- read.table(unz(temp, "Data Files/ch7ex2.dat"), h = TRUE)
unlink(temp)
```

``` r
names(dta) <- c("Ab1", "Ab2", "Ab3", "EM1","EM2", "EM3")
```

``` r
head(dta); tail(dta)
```

``` 
        Ab1      Ab2       Ab3      EM1      EM2      EM3
1 -0.893238 -1.46591 -0.025422 -0.79025  1.61490  0.11267
2  2.109855 -0.28618 -0.531977  1.32913  0.71551  3.71500
3 -2.781437 -1.05577  0.261856 -0.91470  0.11801 -1.41683
4 -0.624744 -0.63718  1.508250 -0.91573  0.93478  0.71992
5  0.001633 -0.27862 -1.119591  0.39693 -0.40601 -0.10771
6  1.610423  0.74873  0.210555  0.28104  1.93441  1.85984
```

``` 
         Ab1      Ab2       Ab3      EM1      EM2       EM3
475 -3.08122 -1.55398 -0.686690 -1.69667  0.19127 -1.199815
476 -0.81595 -0.83628 -1.502773  1.09078  2.30043  1.574503
477 -1.32624 -0.13631 -0.016311 -0.70744  0.48325 -0.739297
478 -0.33000 -0.63686 -0.696167  0.60575  1.23030 -1.416747
479 -1.83450 -1.56242 -0.336267  0.42917 -2.23373  0.010375
480 -0.82474 -0.67180 -0.562705 -0.12613 -1.43304  1.321737
```

## Problem 2

A data set contains estimates of the percentage of body fat determined
by underwater weighing and various body circumference measurements for
252 men. Detailed description of the data set is found
[here](http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.html).

A principal component analysis of the body fat data set is conducted
with the R code segments below. Interpret the results.

``` r
# input data
dta <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
```

``` r
head(dta)
```

``` 
  Density bodyfat Age Weight Height Neck Chest Abdomen   Hip Thigh Knee
1  1.0708    12.3  23 154.25  67.75 36.2  93.1    85.2  94.5  59.0 37.3
2  1.0853     6.1  22 173.25  72.25 38.5  93.6    83.0  98.7  58.7 37.3
3  1.0414    25.3  22 154.00  66.25 34.0  95.8    87.9  99.2  59.6 38.9
4  1.0751    10.4  26 184.75  72.25 37.4 101.8    86.4 101.2  60.1 37.3
5  1.0340    28.7  24 184.25  71.25 34.4  97.3   100.0 101.9  63.2 42.2
6  1.0502    20.9  24 210.25  74.75 39.0 104.5    94.4 107.8  66.0 42.0
  Ankle Biceps Forearm Wrist
1  21.9   32.0    27.4  17.1
2  23.4   30.5    28.9  18.2
3  24.0   28.8    25.2  16.6
4  22.8   32.4    29.4  18.2
5  24.0   32.2    27.7  17.7
6  25.6   35.7    30.6  18.8
```

The Body Mass Index (BMI) is calculated from variables height and
weight.

``` r
# Compute the BMI index from height and weight
dta$BMI <- 703*(dta$Weight/dta$Height^2)
```

Four categories (groups) are created according to the BMI.

``` r
dta$Group <- cut(dta$BMI, breaks = c(0, 18.5, 25, 30, Inf), 
                 labels = c("Underweight", "Normal", "Overweight", "Obese"))
```

``` r
str(dta)
```

    'data.frame':   252 obs. of  17 variables:
     $ Density: num  1.07 1.09 1.04 1.08 1.03 ...
     $ bodyfat: num  12.3 6.1 25.3 10.4 28.7 20.9 19.2 12.4 4.1 11.7 ...
     $ Age    : int  23 22 22 26 24 24 26 25 25 23 ...
     $ Weight : num  154 173 154 185 184 ...
     $ Height : num  67.8 72.2 66.2 72.2 71.2 ...
     $ Neck   : num  36.2 38.5 34 37.4 34.4 39 36.4 37.8 38.1 42.1 ...
     $ Chest  : num  93.1 93.6 95.8 101.8 97.3 ...
     $ Abdomen: num  85.2 83 87.9 86.4 100 94.4 90.7 88.5 82.5 88.6 ...
     $ Hip    : num  94.5 98.7 99.2 101.2 101.9 ...
     $ Thigh  : num  59 58.7 59.6 60.1 63.2 66 58.4 60 62.9 63.1 ...
     $ Knee   : num  37.3 37.3 38.9 37.3 42.2 42 38.3 39.4 38.3 41.7 ...
     $ Ankle  : num  21.9 23.4 24 22.8 24 25.6 22.9 23.2 23.8 25 ...
     $ Biceps : num  32 30.5 28.8 32.4 32.2 35.7 31.9 30.5 35.9 35.6 ...
     $ Forearm: num  27.4 28.9 25.2 29.4 27.7 30.6 27.8 29 31.1 30 ...
     $ Wrist  : num  17.1 18.2 16.6 18.2 17.7 18.8 17.7 18.8 18.2 19.2 ...
     $ BMI    : num  23.6 23.3 24.7 24.9 25.5 ...
     $ Group  : Factor w/ 4 levels "Underweight",..: 2 2 2 2 3 3 3 2 2 3 ...

``` r
res_pc <- princomp(dta[,-c(1:5, 16:17)], cor = TRUE)
```

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggfortify, tidyverse)
```

``` r
autoplot(res_pc, data = dta, loadings = TRUE, colour ='Group',
         loadings.label = TRUE, 
         loadings.colour = 'peru',
         loadings.label.colour = 'peru',
         loadings.label.size = 4,
         loadings.label.repel=T) + 
  ggtitle("Body fat measurements") + 
  theme_bw()
```

<img src="Exercises_13_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
# show weights on variables
loadings(res_pc)
```

``` 

Loadings:
        Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9
Neck     0.327         0.259  0.339         0.288  0.719  0.318       
Chest    0.339  0.273         0.243 -0.447        -0.235  0.127 -0.543
Abdomen  0.334  0.398         0.216 -0.310 -0.147 -0.134         0.303
Hip      0.348  0.255 -0.210 -0.119                      -0.349  0.551
Thigh    0.333  0.191 -0.180 -0.411  0.255  0.105  0.289 -0.404 -0.524
Knee     0.329        -0.273 -0.135  0.446 -0.442 -0.118  0.624       
Ankle    0.247 -0.625 -0.583        -0.416  0.168                     
Biceps   0.322         0.256 -0.304         0.671 -0.471  0.197  0.130
Forearm  0.270 -0.363  0.590 -0.404 -0.262 -0.440                     
Wrist    0.299 -0.377  0.141  0.568  0.429        -0.271 -0.396       
        Comp.10
Neck           
Chest    0.419 
Abdomen -0.669 
Hip      0.563 
Thigh   -0.234 
Knee           
Ankle          
Biceps         
Forearm        
Wrist          

               Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8
SS loadings       1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0
Proportion Var    0.1    0.1    0.1    0.1    0.1    0.1    0.1    0.1
Cumulative Var    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8
               Comp.9 Comp.10
SS loadings       1.0     1.0
Proportion Var    0.1     0.1
Cumulative Var    0.9     1.0
```

## Problem 3

*College* is the name of a data frame in the R package *ISLR*. It
contains 777 observations on 18 variables for a sample of US colleges
from the 1995 issue of US News and World Report. Perform a principal
component analysis on the data and use the results to summarize how
private and public colleges differ based on a few components. Redefining
some of the variables before performing the principal component analysis
might improve the results.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR)
```

``` r
data(College)
?College
```

``` r
# make a copy of the data frame for subsequent analysis
dta <- College
```

``` r
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
                             Private Apps Accept Enroll Top10perc
Abilene Christian University     Yes 1660   1232    721        23
Adelphi University               Yes 2186   1924    512        16
Adrian College                   Yes 1428   1097    336        22
Agnes Scott College              Yes  417    349    137        60
Alaska Pacific University        Yes  193    146     55        16
Albertson College                Yes  587    479    158        38
Albertus Magnus College          Yes  353    340    103        17
Albion College                   Yes 1899   1720    489        37
Albright College                 Yes 1038    839    227        30
                             Top25perc F.Undergrad P.Undergrad Outstate
Abilene Christian University        52        2885         537     7440
Adelphi University                  29        2683        1227    12280
Adrian College                      50        1036          99    11250
Agnes Scott College                 89         510          63    12960
Alaska Pacific University           44         249         869     7560
Albertson College                   62         678          41    13500
Albertus Magnus College             45         416         230    13290
Albion College                      68        1594          32    13868
Albright College                    63         973         306    15595
                             Room.Board Books Personal PhD Terminal
Abilene Christian University       3300   450     2200  70       78
Adelphi University                 6450   750     1500  29       30
Adrian College                     3750   400     1165  53       66
Agnes Scott College                5450   450      875  92       97
Alaska Pacific University          4120   800     1500  76       72
Albertson College                  3335   500      675  67       73
Albertus Magnus College            5720   500     1500  90       93
Albion College                     4826   450      850  89      100
Albright College                   4400   300      500  79       84
                             S.F.Ratio perc.alumni Expend Grad.Rate
Abilene Christian University      18.1          12   7041        60
Adelphi University                12.2          16  10527        56
Adrian College                    12.9          30   8735        54
Agnes Scott College                7.7          37  19016        59
Alaska Pacific University         11.9           2  10922        15
Albertson College                  9.4          11   9727        55
Albertus Magnus College           11.5          26   8861        63
Albion College                    13.7          37  11487        73
Albright College                  11.3          23  11644        80
```

``` 
                                Private  Apps Accept Enroll Top10perc
Wisconsin Lutheran College          Yes   152    128     75        17
Wittenberg University               Yes  1979   1739    575        42
Wofford College                     Yes  1501    935    273        51
Worcester Polytechnic Institute     Yes  2768   2314    682        49
Worcester State College              No  2197   1515    543         4
Xavier University                   Yes  1959   1805    695        24
Xavier University of Louisiana      Yes  2097   1915    695        34
Yale University                     Yes 10705   2453   1317        95
York College of Pennsylvania        Yes  2989   1855    691        28
                                Top25perc F.Undergrad P.Undergrad Outstate
Wisconsin Lutheran College             41         282          22     9100
Wittenberg University                  68        1980         144    15948
Wofford College                        83        1059          34    12680
Worcester Polytechnic Institute        86        2802          86    15884
Worcester State College                26        3089        2029     6797
Xavier University                      47        2849        1107    11520
Xavier University of Louisiana         61        2793         166     6900
Yale University                        99        5217          83    19840
York College of Pennsylvania           63        2988        1726     4990
                                Room.Board Books Personal PhD Terminal
Wisconsin Lutheran College            3700   500     1400  48       48
Wittenberg University                 4404   400      800  82       95
Wofford College                       4150   605     1440  91       92
Worcester Polytechnic Institute       5370   530      730  92       94
Worcester State College               3900   500     1200  60       60
Xavier University                     4960   600     1250  73       75
Xavier University of Louisiana        4200   617      781  67       75
Yale University                       6510   630     2115  96       96
York College of Pennsylvania          3560   500     1250  75       75
                                S.F.Ratio perc.alumni Expend Grad.Rate
Wisconsin Lutheran College            8.5          26   8960        50
Wittenberg University                12.8          29  10414        78
Wofford College                      15.3          42   7875        75
Worcester Polytechnic Institute      15.2          34  10774        82
Worcester State College              21.0          14   4469        40
Xavier University                    13.3          31   9189        83
Xavier University of Louisiana       14.4          20   8323        49
Yale University                       5.8          49  40386        99
York College of Pennsylvania         18.1          28   4509        99
```

## References

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013) *An
Introduction to Statistical Learning with applications in R*.
Springer-Verlag.

Raykov, T., & George A. Marcoulides, G.A. (2008). *An Introduction to
Applied Multivariate Analysis* Routledge.

Rencher, A., & Christensen, W.F. (2012). *Methods of Multivariate
Analysis*. 3rd Ed. Wiley.
