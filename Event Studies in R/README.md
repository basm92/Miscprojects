---
title: "Event Studies in R"
author: "Bas Machielsen"
date: "5/5/2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

This vignette briefly explains the basics of doing event studies (as described in MacKinlay, 1997, Event Studies in Economics and Finance) in R. It will make use of several packages, all of which in turn make use of publicly accessible data. Thus, no access or subscription to any data service provider is necessary, all you need is a working version of R and RStudio, and a couple of relevant packages. 

## Installing packages

We begin the manual by installing two categories of relevant packages. First, the more basic packages used to clean the data and model output:


```r
library(tidyverse)
library(readxl)
library(broom)
```

Secondly, we load the packages specifically relevant to extracting stock prices and calculating the relevant returns:


```r
library(tidyquant)
library(timetk)
```

You can install those packages with `install.packages` if you haven't done so already. Chances are you also have to install serves packages upon which these packages are dependent. 

## Other resources

A couple of relevant resources: 

- [The tidyquant Github manual](https://github.com/business-science/tidyquant)

The readme contains lots of different resources and event provides short tutorials to work with `tidyquant` efficiently. 

- [Coding Finance](https://www.codingfinance.com/post/2018-04-03-calc-returns/) 

Since we will be using only one particular approach here (calculating daily stock returns in a particular manner), it might be useful to realize, and see for yourself, what other approaches exist and how `tidyquant` can handle them easily. 

## Find the tickers

Our next step is to find the relevant tickers for an event study. In this example, I will take all stocks from the Dutch AEX, and see what happened to them on the 16th of March, 2020 (a random date). Consistuents of an index are usually easily found on Yahoo Finance (or any such site), or in the newspapers, etc. I extract them from Yahoo Finance using a bit of (sloppy) scraping, but you can also copy and paste them, and put them into a character vector. 


```r
tickers <- xml2::read_html("https://finance.yahoo.com/quote/%5EAEX/components/") %>%
  rvest::html_nodes(xpath = "/html/body/div[1]/div/div/div[1]/div/div[3]/div[1]/div/div[1]/div/div/section/section/div/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame() %>%
  pull(1)
```


## Download the stock prices in the estimation period

Obviously, we need to define an **estimation period**. I arbitrarily pick 1st of May to 1st of December, 2019. This will be the period over which we will estimate beta's (or other asset pricing models). `tidyquant` does the rest of the dirty work, and converts prices into returns easily:



```r
stocks <- tq_get(tickers,
                        from = "2019-05-01", #Estimation window
                        to = "2019-12-01",
                        get = "stock.prices")

#Making returns from prices
returns <- stocks %>%
  group_by(symbol) %>%          #Grouping the stocks by the stock symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')


head(returns)
```

```
## # A tibble: 6 x 3
## # Groups:   symbol [1]
##   symbol  date         returns
##   <chr>   <date>         <dbl>
## 1 RAND.AS 2019-05-02  0       
## 2 RAND.AS 2019-05-03 -0.00119 
## 3 RAND.AS 2019-05-06 -0.00119 
## 4 RAND.AS 2019-05-07 -0.0190  
## 5 RAND.AS 2019-05-08 -0.000202
## 6 RAND.AS 2019-05-09 -0.0174
```



## Download the index prices 

Now we also get AEX Index (and after the %>%, we make it into returns)


```r
aex <- tq_get("^AEX",
                get = "stock.prices",
                from = "2019-05-01",
                to = "2019-12-01") %>%
  tq_transmute(select = adjusted,
                mutate_fun = periodReturn,
                period = 'daily',
                col_rename = 'returns')

head(aex)
```

```
## # A tibble: 6 x 2
##   date        returns
##   <date>        <dbl>
## 1 2019-05-02  0      
## 2 2019-05-03  0.00390
## 3 2019-05-06 -0.00874
## 4 2019-05-07 -0.0122 
## 5 2019-05-08  0.00372
## 6 2019-05-09 -0.0167
```

# Merging the returns with the index

The next step is the merge the returns of stock i at time t with the corresponding return of the index, $m$ at time $t$. This is very easy to accomplish in R, using `left_join`. 


```r
reg <- left_join(returns, aex, by = "date") %>%
  rename(stock.return = returns.x, aex.return= returns.y)


head(reg)
```

```
## # A tibble: 6 x 4
## # Groups:   symbol [1]
##   symbol  date       stock.return aex.return
##   <chr>   <date>            <dbl>      <dbl>
## 1 RAND.AS 2019-05-02     0           0      
## 2 RAND.AS 2019-05-03    -0.00119     0.00390
## 3 RAND.AS 2019-05-06    -0.00119    -0.00874
## 4 RAND.AS 2019-05-07    -0.0190     -0.0122 
## 5 RAND.AS 2019-05-08    -0.000202    0.00372
## 6 RAND.AS 2019-05-09    -0.0174     -0.0167
```

## Calculating the Beta's

Estimating the stock beta's is also very easy. Here, we use the tools of the `dplyr` and `broom` packages to quickly and efficiently estimate all stock beta's:


```r
models <- reg %>%
  group_by(symbol) %>% #For each stock
  do(model = lm(stock.return ~ aex.return, data = .)) #This is the regression model

# Now, use broom to extract the coefficient (estmiate) from the aex.return variable:
betas <- models %>%
  tidy(model) %>%
  filter(term == "aex.return") %>%
  select(symbol, estimate)

head(betas)
```

```
## # A tibble: 6 x 2
## # Groups:   symbol [6]
##   symbol   estimate
##   <chr>       <dbl>
## 1 ABN.AS      0.787
## 2 AD.AS       0.709
## 3 ADYEN.AS    1.29 
## 4 AGN.AS      1.51 
## 5 AKZA.AS     0.729
## 6 ASM.AS      1.71
```


## The actual event study

Next on, we proceed to get stock prices and returns in the actual event window. Let's suppose we are interested in the event window ranging from -10 to +10 (you can change it to something else yoruself). 

I will do it a little bit faster than before, converting them right away to returns. Remember we are interested in the event around 16th of March, so let's define our event window to range from 04th of March to 27th of March (we could also look when exactly weekends and non-trading days are). 



```r
stocks2 <- tq_get(tickers,
                 from = "2020-03-04",
                 to = "2020-03-27",
                 get = "stock.prices") %>%
  group_by(symbol) %>%          #Grouping the stocks by the stock symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')
```

We repeat the same step for the AEX index:


```r
aex2 <- tq_get("^AEX",
                get = "stock.prices",
                from = "2020-03-04",
                to = "2020-03-27") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')
```


## Merging, compute ARs and other stats

Next, we will do two things:

1. Merge the stock returns with the AEX returns

2. Compute the abnormal returns


```r
evtstudy <- left_join(stocks2, aex2, by = "date") %>%
  merge(betas, by = "symbol") %>%
  rename(stock.return = returns.x, aex.return= returns.y) %>%
  mutate(abret = stock.return - (aex.return * estimate), 
         evttime = date - ymd("2020-03-16"))

evtstudy2 <- evtstudy %>%
  mutate(evttime = as.numeric(evttime)) %>%
  select(symbol, evttime, abret)

head(evtstudy)
```

```
##   symbol       date stock.return   aex.return  estimate        abret  evttime
## 1 ABN.AS 2020-03-04  0.000000000  0.000000000 0.7872458  0.000000000 -12 days
## 2 ABN.AS 2020-03-05 -0.039967091 -0.009305582 0.7872458 -0.032641310 -11 days
## 3 ABN.AS 2020-03-06 -0.038626570 -0.038458756 0.7872458 -0.008350074 -10 days
## 4 ABN.AS 2020-03-09 -0.149285645 -0.076474196 0.7872458 -0.089081652  -7 days
## 5 ABN.AS 2020-03-10 -0.003568509 -0.010536856 0.7872458  0.004726587  -6 days
## 6 ABN.AS 2020-03-11  0.001474711 -0.002739472 0.7872458  0.003631348  -5 days
```


## Calculate Cumulative Abnormal Returns and Variances

Finally, we compute cumulative abnormal returns and other stats required for an event study:


```r
results <- evtstudy2 %>%
  group_by(evttime) %>%
  summarise(avgar = mean(abret), var = var(abret)) %>%
  mutate(avgcar = cumsum(avgar),
         avgvar = cumsum(var))  #This comes from eq. 15 and 16 in MacKinley (1997))
```

## Example graph

Here is an example graph: unsurprisingly, no significant abnormal returns (because I took a random date when nothing unanticipated happened). 


```r
results %>%
  ggplot(aes(x = evttime, y = avgcar)) + geom_line() +
  geom_line(aes(x = evttime, y = avgcar + 1.96*sqrt(var)), lty = "dashed") +
  geom_line(aes(x = evttime, y = avgcar - 1.96*sqrt(var)), lty = "dashed")
```

![](Event_studies_in_R_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Example regression

The information in the data.frame `evtstudy` can also be used to analyse the results more into depth, for example, by regression the CAR's (-12, 10) on independent variables. The following can be used as input into such a regression: 


```r
evtstudy %>%
  mutate(evttime = as.numeric(evttime)) %>%
  group_by(symbol) %>%
  mutate(car = cumsum(abret)) %>%
  select(symbol, evttime, car) %>%
  filter(evttime == 10) %>%
  head()
```

```
## # A tibble: 6 x 3
## # Groups:   symbol [6]
##   symbol   evttime     car
##   <chr>      <dbl>   <dbl>
## 1 ABN.AS        10 -0.194 
## 2 AD.AS         10 -0.0242
## 3 ADYEN.AS      10  0.125 
## 4 AGN.AS        10  0.0550
## 5 AKZA.AS       10 -0.0821
## 6 ASM.AS        10  0.0626
```


Thank you for reading. If you have any questions, feel free to contact me on [Github](www.github.com/basm92) or [e-mail](a.h.machielsen@uu.nl). 