 *Multivariate Analysis for the Behavioral Sciences,*  
 **Examples of Chapter 8:**  
 **Analysis of Longitudinal Data I: Graphical Displays and Summary
Measure Approach**
================
Kimmo Vehkalahti, Brian S. Everitt; edited by C.-F. Sheu
07 September, 2019

## Example: Labor pain

The data come from a clinical trial comparing two treatments for
maternal pain relief during labor. In this study 83 women in labor were
randomized to receive an experimental pain medication (43 subjects in
group 1) or placebo (40 subjects in group 2). Treatment was initiated
when the cervical dilation was 8 cm. At 30-minute intervals, the amount
of pain was self-reported by placing a mark on a 100 mm line (0 = no
pain, 100 = very much pain).

``` r
# check to see if the tidyverse package is there
# if not install it and then load it
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
```

## Table 8.5: Pain Scores from 83 Women in Labor

``` r
# data input
fLoc <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/pain.txt"
PAIN <- read.table(fLoc, header = TRUE, sep = "\t")
```

``` r
PAIN <- within(PAIN, {
    group <- factor(group)
       id <- factor(id)
})
```

``` r
glimpse(PAIN)
```

    Observations: 83
    Variables: 9
    $ group <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ id    <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    $ m0    <dbl> 0.0, 0.0, 38.0, 6.0, 19.0, 7.0, 44.0, 1.0, 24.5, 1.0, 35.5…
    $ m30   <dbl> 0.0, 0.0, 5.0, 48.0, 5.0, 0.0, 42.0, 0.0, 35.0, 30.5, 44.5…
    $ m60   <dbl> 0.0, 0.0, 1.0, 85.0, NA, 0.0, 42.0, 0.0, 13.0, 81.5, 55.0,…
    $ m90   <dbl> 0.0, 0.0, 1.0, 0.0, NA, 0.0, 45.0, 0.0, NA, 67.5, 69.0, 0.…
    $ m120  <dbl> NA, 2.5, 0.0, 0.0, NA, NA, NA, 0.0, NA, 98.5, 72.5, 0.0, 2…
    $ m150  <dbl> NA, 2.3, 5.0, NA, NA, NA, NA, 6.0, NA, 97.0, 39.5, 0.0, 45…
    $ m180  <dbl> NA, 14.0, NA, NA, NA, NA, NA, 24.0, NA, NA, 26.0, 0.0, 91.…

## Table 8.6

``` r
# Convert data to long form:
PAINL <- gather(PAIN, key = mins, value = pain, -group, -id) %>%
  mutate(mins = as.integer(substr(mins, 2, 4)))
```

``` r
glimpse(PAINL)
```

    Observations: 581
    Variables: 4
    $ group <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ id    <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    $ mins  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ pain  <dbl> 0.0, 0.0, 38.0, 6.0, 19.0, 7.0, 44.0, 1.0, 24.5, 1.0, 35.5…

``` r
head(PAINL)
```

``` 
  group id mins pain
1     1  1    0    0
2     1  2    0    0
3     1  3    0   38
4     1  4    0    6
5     1  5    0   19
6     1  6    0    7
```

``` r
# (1) Make a summary data of group means,
# removing subjects with any missing values:
PAINLS1 <- PAINL %>%
  group_by(group, id) %>%
  summarise( mean = mean(pain) ) %>%
  ungroup()
```

``` r
glimpse(PAINLS1)
```

    Observations: 83
    Variables: 3
    $ group <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ id    <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    $ mean  <dbl> NA, 2.68571, NA, NA, NA, NA, NA, 4.42857, NA, NA, 48.85714…

``` r
head(PAINLS1)
```

``` 
# A tibble: 6 x 3
  group id     mean
  <fct> <fct> <dbl>
1 1     1     NA   
2 1     2      2.69
3 1     3     NA   
4 1     4     NA   
5 1     5     NA   
6 1     6     NA   
```

``` r
# (2) Make a summary data of group means,
# now using the mean of available responses for each subject:
PAINLS2 <- PAINL %>%
  group_by(group, id) %>%
  summarise( mean = mean(pain, na.rm = TRUE) ) %>%
  ungroup()
```

``` r
glimpse(PAINLS2)
```

    Observations: 83
    Variables: 3
    $ group <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ id    <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    $ mean  <dbl> 0.00000, 2.68571, 8.33333, 27.80000, 12.00000, 1.75000, 43…

``` r
head(PAINLS2)
```

    # A tibble: 6 x 3
      group id     mean
      <fct> <fct> <dbl>
    1 1     1      0   
    2 1     2      2.69
    3 1     3      8.33
    4 1     4     27.8 
    5 1     5     12   
    6 1     6      1.75

``` r
# Compare the t-test results:
t.test(mean ~ group, data = PAINLS1, var.equal = TRUE)
```

``` 

    Two Sample t-test

data:  mean by group
t = -3.85, df = 32, p-value = 0.00054
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -45.974 -14.140
sample estimates:
mean in group 1 mean in group 2 
         16.743          46.800 
```

``` r
t.test(mean ~ group, data = PAINLS2, var.equal = TRUE)
```

``` 

    Two Sample t-test

data:  mean by group
t = -4.15, df = 81, p-value = 8.3e-05
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -33.589 -11.803
sample estimates:
mean in group 1 mean in group 2 
         18.342          41.038 
```

## Session information
