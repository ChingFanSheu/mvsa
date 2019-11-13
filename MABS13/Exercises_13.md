**Exercises: Principal Components Analysis**
================
C.-P. Cheng and C.-F. Sheu
13 November, 2019

# Exercises

## Problem 1

Four hundred and eighty seniors were tested on three mental ability
measures and three educational motivation subscales. It is of interest
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

Twenty engineer apprentices and twenty pilots were given six tests that
are designed to measure the following attributes: (1) Intelligence (2)
Form Relations (3) Dynamometer (4) Dotting (5) Sensory Motor
Coordination (6) Perservation

Could a principal component analysis of the data provide a useful summary
of attributes for either engineer apprentices or pilots or both?

``` r
temp <- tempfile()
fLoc <- "ftp://ftp.wiley.com/public/sci_tech_med/multivariate_analysis_3e/multivariate_analysis - 3rd Ed.zip"
download.file(url=fLoc, destfile = temp)
```

``` r
dta <- read.table(unz(temp, "multivariate_analysis - 3rd Ed/T5_6_PILOT.DAT"), col.names =  c('Group', 'Intelligence', 'FormRelations', 'Dynamometer', 'Dotting', 'SensoryMotorCoordination', 'Perservation'))
unlink(temp)
```

``` r
dta$Group <- factor(ifelse(dta$Group == 1, 'Apprentice', 'Pilot'))
```

``` r
# show top and bottom 9 lines of data frame
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
       Group Intelligence FormRelations Dynamometer Dotting
1 Apprentice          121            22          74     223
2 Apprentice          108            30          80     175
3 Apprentice          122            49          87     266
4 Apprentice           77            37          66     178
5 Apprentice          140            35          71     175
6 Apprentice          108            37          57     241
7 Apprentice          124            39          52     194
8 Apprentice          130            34          89     200
9 Apprentice          149            55          91     198
  SensoryMotorCoordination Perservation
1                       54          254
2                       40          300
3                       41          223
4                       80          209
5                       38          261
6                       59          245
7                       72          242
8                       85          242
9                       50          277
```

``` 
   Group Intelligence FormRelations Dynamometer Dotting
32 Pilot          135            41          83     216
33 Pilot          100            35          83     183
34 Pilot          149            37          94     227
35 Pilot          149            38          78     258
36 Pilot          153            27          89     283
37 Pilot          136            31          83     257
38 Pilot           97            36         100     252
39 Pilot          141            37         105     250
40 Pilot          164            32          76     187
   SensoryMotorCoordination Perservation
32                       39          306
33                       57          242
34                       30          240
35                       42          271
36                       66          291
37                       31          311
38                       30          225
39                       27          243
40                       30          264
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
