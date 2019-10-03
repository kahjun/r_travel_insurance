Analyzing Travel Insurance
================
Kah Jun Lim
10/2/2019

## Introduction

The purpose of this project is to perform exploratory data analysis(EDA)
on travel insurance dataset and identify factors that might affect
whether a claim is made.

## Loading the Data and Dependencies

``` r
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)

travel_insurance_full <- read.csv("data/travel_insurance.csv")
head(travel_insurance_full)
```

    ##   Agency   Agency.Type Distribution.Channel
    ## 1    CBH Travel Agency              Offline
    ## 2    CBH Travel Agency              Offline
    ## 3    CWT Travel Agency               Online
    ## 4    CWT Travel Agency               Online
    ## 5    CWT Travel Agency               Online
    ## 6    JZI      Airlines               Online
    ##                      Product.Name Claim Duration   Destination Net.Sales
    ## 1              Comprehensive Plan    No      186      MALAYSIA     -29.0
    ## 2              Comprehensive Plan    No      186      MALAYSIA     -29.0
    ## 3 Rental Vehicle Excess Insurance    No       65     AUSTRALIA     -49.5
    ## 4 Rental Vehicle Excess Insurance    No       60     AUSTRALIA     -39.6
    ## 5 Rental Vehicle Excess Insurance    No       79         ITALY     -19.8
    ## 6                      Value Plan    No       66 UNITED STATES    -121.0
    ##   Commision..in.value. Gender Age
    ## 1                 9.57      F  81
    ## 2                 9.57      F  71
    ## 3                29.70         32
    ## 4                23.76         32
    ## 5                11.88         41
    ## 6                42.35      F  44

``` r
colnames(travel_insurance_full) <- c("agency", "agency.type", "distribution.channel", "product.name", "claim", 
                                     "duration", "destination", "net.sales", "commision", "gender", "age")
```

## Exploratory Data Analysis

``` r
summary(travel_insurance_full)
```

    ##      agency             agency.type    distribution.channel
    ##  EPX    :35119   Airlines     :17457   Offline: 1107       
    ##  CWT    : 8580   Travel Agency:45869   Online :62219       
    ##  C2B    : 8267                                             
    ##  JZI    : 6329                                             
    ##  SSI    : 1056                                             
    ##  JWT    :  749                                             
    ##  (Other): 3226                                             
    ##                           product.name   claim          duration      
    ##  Cancellation Plan              :18630   No :62399   Min.   :  -2.00  
    ##  2 way Comprehensive Plan       :13158   Yes:  927   1st Qu.:   9.00  
    ##  Rental Vehicle Excess Insurance: 8580               Median :  22.00  
    ##  Basic Plan                     : 5469               Mean   :  49.32  
    ##  Bronze Plan                    : 4049               3rd Qu.:  53.00  
    ##  1 way Comprehensive Plan       : 3331               Max.   :4881.00  
    ##  (Other)                        :10109                                
    ##     destination      net.sales         commision      gender   
    ##  SINGAPORE:13255   Min.   :-389.00   Min.   :  0.00    :45107  
    ##  MALAYSIA : 5930   1st Qu.:  18.00   1st Qu.:  0.00   F: 8872  
    ##  THAILAND : 5894   Median :  26.53   Median :  0.00   M: 9347  
    ##  CHINA    : 4796   Mean   :  40.70   Mean   :  9.81            
    ##  AUSTRALIA: 3694   3rd Qu.:  48.00   3rd Qu.: 11.55            
    ##  INDONESIA: 3452   Max.   : 810.00   Max.   :283.50            
    ##  (Other)  :26305                                               
    ##       age        
    ##  Min.   :  0.00  
    ##  1st Qu.: 35.00  
    ##  Median : 36.00  
    ##  Mean   : 39.97  
    ##  3rd Qu.: 43.00  
    ##  Max.   :118.00  
    ## 

There are 10 features in this data set, 4 of them are numeric and the
others are factors. The only column with missing data is Gender, where a
large portion of the values are missing. I notice that there are
non-positive values for duration, which does not make sense. There are
also durations that are excessively long. For age, the highest value is
118.

``` r
travel_insurance_full %>%
  filter(duration <= 0 | duration >= 1000) %>%
  nrow
```

    ## [1] 80

``` r
travel_insurance_full %>%
  filter(age > 100) %>%
  nrow
```

    ## [1] 984

Since there is only a relatively small proportion of the data contains
unreasonable duration and uncommon age, I remove these data points for
the purpose of this project.

``` r
travel_insurance <- travel_insurance_full %>%
  filter(duration > 0 & duration < 1000 & age <= 100) %>%
  mutate(claim = ifelse(claim == "Yes", 1, 0))
```

    ## Warning: The `printer` argument is deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
levels(travel_insurance$gender) <- c("Unknown", "F", "M")
```

We can now move on to look at distribution of some of the features.

### Factor Variables

``` r
# agency.type
ggplot(travel_insurance, aes(x = claim, fill = agency.type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format())
```

![](travel_insurance_files/figure-gfm/Factor%20variables-1.png)<!-- -->

``` r
# distribution.channel (not useful)
# ggplot(travel_insurance, aes(x = claim, fill = distribution.channel)) +
#   geom_bar()

# gender
ggplot(travel_insurance, aes(x = claim, fill = gender)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format())
```

![](travel_insurance_files/figure-gfm/Factor%20variables-2.png)<!-- -->

``` r
# Helper functions to plot top n for factors with many levels
plot_top_n <- function(df, var, n = 10, title = "") {
  colnames(df)[colnames(df) == var] <- "var"
  freq_by_var <- df %>%
    group_by(var, claim = as.factor(claim)) %>%
    summarise(freq = n()) %>%
    spread(key = claim, value = freq, fill = 0) %>%
    mutate(total = `0` + `1`) %>%
    ungroup %>%
    top_n(n = n, wt = total) %>%
    select(-total) %>%
    gather(key = "claim", value = "freq", `0`, `1`)
  p <- ggplot(freq_by_var, aes(x = var, y = freq, fill = claim))
  p1 <- p +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(var)
  p2 <- p +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(var)
  grid.arrange(p1, p2, ncol = 2, top = title)
}

plot_top_n(travel_insurance, "destination", title = "Top 10 Destinations")
```

![](travel_insurance_files/figure-gfm/Factor%20variables-3.png)<!-- -->

``` r
plot_top_n(travel_insurance, "agency", title = "Top 10 Agencies")
```

![](travel_insurance_files/figure-gfm/Factor%20variables-4.png)<!-- -->

``` r
plot_top_n(travel_insurance, "product.name", title = "Top 10 Products")
```

![](travel_insurance_files/figure-gfm/Factor%20variables-5.png)<!-- -->

1.  We see that given a claim has occured, it is more likely to be from
    airlines than from travel agency.

2.  For gender, although claims occur with equal proportion for Male,
    Female and Unknown.

3.  Only 5 agencies observed claims count more than 50.

4.  All the companies have extremely low percentage of claims observed.

### Numeric Variables

``` r
p1 <- ggplot(travel_insurance, aes(x = duration, y = claim)) +
  geom_jitter(alpha = 0.05)
p2 <- ggplot(travel_insurance, aes(x = net.sales, y = claim)) +
  geom_jitter(alpha = 0.05)
p3 <- ggplot(travel_insurance, aes(x = commision, y = claim)) +
  geom_jitter(alpha = 0.05)
p4 <- ggplot(travel_insurance, aes(x = age, y = claim)) +
  geom_jitter(alpha = 0.05)
grid.arrange(p1, p2, p3, p4, ncol = 2)
```

![](travel_insurance_files/figure-gfm/Numeric%20variables-1.png)<!-- -->

There doesn’t seem to be any obvious pattern in the four plots. Let us
add more dimension to the scatterplots and see if we can have any new
findings.

``` r
# duration
ggplot(travel_insurance, aes(x = duration, y = claim, color = gender)) +
  geom_jitter(alpha = 0.5, shape = 1)
```

![](travel_insurance_files/figure-gfm/Numeric%20Variables%20by%20Factors-1.png)<!-- -->

``` r
ggplot(travel_insurance, aes(x = duration, y = claim, color = agency.type)) +
  geom_jitter(alpha = 0.5, shape = 1)
```

![](travel_insurance_files/figure-gfm/Numeric%20Variables%20by%20Factors-2.png)<!-- -->

``` r
# age (not useful)
# ggplot(travel_insurance, aes(x = age, y = claim, color = gender)) +
#   geom_jitter(alpha = 0.5, shape = 1)
# ggplot(travel_insurance, aes(x = age, y = claim, color = agency.type)) +
#   geom_jitter(alpha = 0.5, shape = 1)

# net.sales
ggplot(travel_insurance, aes(x = net.sales, y = claim, color = gender)) +
  geom_jitter(alpha = 0.5, shape = 1)
```

![](travel_insurance_files/figure-gfm/Numeric%20Variables%20by%20Factors-3.png)<!-- -->

``` r
ggplot(travel_insurance, aes(x = net.sales, y = claim, color = agency.type)) +
  geom_jitter(alpha = 0.5, shape = 1)
```

![](travel_insurance_files/figure-gfm/Numeric%20Variables%20by%20Factors-4.png)<!-- -->

``` r
# commision (not useful)
# ggplot(travel_insurance, aes(x = commision, y = claim, color = gender)) +
#   geom_jitter(alpha = 0.5, shape = 1)
# ggplot(travel_insurance, aes(x = commision, y = claim, color = agency.type)) +
#   geom_jitter(alpha = 0.5, shape = 1)
```

TODO
