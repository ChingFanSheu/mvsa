**Exercises of Analysis of Longitudinal Data I and II**
================
C.-P. Cheng and C.-F. Sheu
05 October, 2019

# Exercises

## Problem 1

Data are drawn from test results on file in the Records Office of the
Laboratory School of the University of Chicago. They consist of scores,
obtained from a cohort of pupils at the eigth through eleventh gade
level on alternative forms of the vocabulary section of the Cooperative
Reading Tests. The Cooperative tests provide a basis for estimating a
vocabulary score on a scale with a common origin and unit. The resulting
scaled scores, with origin and unit fixed arbitrarily, are computed for
64 students in which 36 are males and 28 are females. Since these data
cover an age range in which physical growth is beginning to decellerate,
it is of interest to inquire whether a similar decelleration can be
observed in the acquisition of new vocabulary.

Use the vocabulary data (see below) and modify the R codes given in the
course materials to create suitable graphics and to analyse the data.

``` r
# file location
fLoc <- "http://www-stat.stanford.edu/~rag/stat222/BOCKlong.dat"
```

``` r
# input data
dta <- read.table(fLoc, header = T)
```

``` r
# examine data frame structure
str(dta)
```

    'data.frame':   256 obs. of  4 variables:
     $ vocab  : num  1.75 0.9 0.8 2.42 -1.31 -1.56 1.09 -1.92 -1.61 2.47 ...
     $ grade  : int  8 8 8 8 8 8 8 8 8 8 ...
     $ student: int  1 2 3 4 5 6 7 8 9 10 ...
     $ isMale : int  1 1 1 1 1 1 1 1 1 1 ...

``` r
# show top and bottom 9 lines of data frame
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
  vocab grade student isMale
1  1.75     8       1      1
2  0.90     8       2      1
3  0.80     8       3      1
4  2.42     8       4      1
5 -1.31     8       5      1
6 -1.56     8       6      1
7  1.09     8       7      1
8 -1.92     8       8      1
9 -1.61     8       9      1
```

``` 
    vocab grade student isMale
248  1.16    11      56      0
249  2.18    11      57      0
250  2.61    11      58      0
251  3.91    11      59      0
252  1.86    11      60      0
253  3.89    11      61      0
254  4.98    11      62      0
255  2.31    11      63      0
256  2.64    11      64      0
```

## Problem 2

A sample of 158 children with autisim spectrum disorder were recruited.
Social development was assessed using the Vineland Adaptive Behavior
Interview survey form, a parent-reported measure of socialization. It is
a combined score that included assessment of interpersonal
relationships, play/leisure time activities, and coping skills. Initial
language development was assessed using the Sequenced Inventory of
Communication Development (SICD) scale. These assessments were repeated
on these children when they were 3, 5, 9, 13 years of age.

Use the autism data set and modify the R codes given in the course
materials to create suitable graphics of the data and the analyze the
data with an appropriate model.

``` r
# install package with the data set
installed.packages("WWGbook", repos='http://cran.us.r-project.org')
```

``` 
     Package LibPath Version Priority Depends Imports LinkingTo Suggests
     Enhances License License_is_FOSS License_restricts_use OS_type Archs
     MD5sum NeedsCompilation Built
```

``` r
# load the package
library(WWGbook)

# load the data
data(autism)
```

``` r
# get help document on the data object
?autism
```

``` r
# make a copy of the data
dta <- autism
```

``` r
str(dta)
```

    'data.frame':   612 obs. of  4 variables:
     $ age    : int  2 3 5 9 13 2 3 5 9 13 ...
     $ vsae   : int  6 7 18 25 27 17 18 12 18 24 ...
     $ sicdegp: int  3 3 3 3 3 3 3 3 3 3 ...
     $ childid: int  1 1 1 1 1 3 3 3 3 3 ...

``` r
# the data are readily in the long form:
head(dta, n = 9); tail(dta, n = 9)
```

``` 
  age vsae sicdegp childid
1   2    6       3       1
2   3    7       3       1
3   5   18       3       1
4   9   25       3       1
5  13   27       3       1
6   2   17       3       3
7   3   18       3       3
8   5   12       3       3
9   9   18       3       3
```

``` 
    age vsae sicdegp childid
604   2   11       1     200
605   3    8       1     200
606   2    8       1     202
607   3    9       1     202
608   5    6       1     202
609  13   12       1     202
610   2    4       1     210
611   3   25       1     210
612   9  130       1     210
```

## References

Bock, R.D. (1975). *Multivariate Statistical Methods in Behavioral
Research*.

West, B.T., Welch, K.B., & Galecki, A.T. (2002). *Linear Mixed Models:
Practical Guide Using Statistical Software*.
