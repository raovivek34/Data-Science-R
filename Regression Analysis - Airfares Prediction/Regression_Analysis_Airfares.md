Regression\_Analysis\_Airfares\_Prediction
================

``` r
#Pacman tool used for Package-management
if(!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(caret, corrplot, glmnet, mlbench, tidyverse, ggplot2, goeveg, reshape, gridExtra, leaps, dplyr, tidyr, forecast, MASS) 
search()
```

    ##  [1] ".GlobalEnv"        "package:MASS"      "package:forecast" 
    ##  [4] "package:leaps"     "package:gridExtra" "package:reshape"  
    ##  [7] "package:goeveg"    "package:forcats"   "package:stringr"  
    ## [10] "package:dplyr"     "package:purrr"     "package:readr"    
    ## [13] "package:tidyr"     "package:tibble"    "package:tidyverse"
    ## [16] "package:mlbench"   "package:glmnet"    "package:foreach"  
    ## [19] "package:Matrix"    "package:corrplot"  "package:caret"    
    ## [22] "package:ggplot2"   "package:lattice"   "package:pacman"   
    ## [25] "package:stats"     "package:graphics"  "package:grDevices"
    ## [28] "package:utils"     "package:datasets"  "package:methods"  
    ## [31] "Autoloads"         "package:base"

# Question 1

## Create a correlation table and scatterplots between FARE and the predictors. What seems to be the best single predictor of FARE? Explain your answer

``` r
#Reading the file 'Airfares.csv'
airfares_raw.df <- read.csv("Airfares.csv") 
str(airfares_raw.df)
```

    ## 'data.frame':    638 obs. of  18 variables:
    ##  $ S_CODE  : Factor w/ 8 levels "*","DCA","EWR",..: 1 1 1 8 7 1 1 1 1 1 ...
    ##  $ S_CITY  : Factor w/ 51 levels "Albuquerque         NM",..: 14 3 7 9 9 11 14 18 23 25 ...
    ##  $ E_CODE  : Factor w/ 8 levels "*","DCA","EWR",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ E_CITY  : Factor w/ 68 levels "Amarillo            TX",..: 1 2 2 2 2 2 2 2 2 2 ...
    ##  $ COUPON  : num  1 1.06 1.06 1.06 1.06 1.01 1.28 1.15 1.33 1.6 ...
    ##  $ NEW     : int  3 3 3 3 3 3 3 3 3 2 ...
    ##  $ VACATION: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
    ##  $ SW      : Factor w/ 2 levels "No","Yes": 2 1 1 2 2 2 1 2 2 2 ...
    ##  $ HI      : num  5292 5419 9185 2657 2657 ...
    ##  $ S_INCOME: num  28637 26993 30124 29260 29260 ...
    ##  $ E_INCOME: num  21112 29838 29838 29838 29838 ...
    ##  $ S_POP   : int  3036732 3532657 5787293 7830332 7830332 2230955 3036732 1440377 3770125 1694803 ...
    ##  $ E_POP   : int  205711 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 ...
    ##  $ SLOT    : Factor w/ 2 levels "Controlled","Free": 2 2 2 1 2 2 2 2 2 2 ...
    ##  $ GATE    : Factor w/ 2 levels "Constrained",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DISTANCE: int  312 576 364 612 612 309 1220 921 1249 964 ...
    ##  $ PAX     : int  7864 8820 6452 25144 25144 13386 4625 5512 7811 4657 ...
    ##  $ FARE    : num  64.1 174.5 207.8 85.5 85.5 ...

``` r
head(airfares_raw.df)
```

    ##   S_CODE                 S_CITY E_CODE                 E_CITY COUPON NEW
    ## 1      * Dallas/Fort Worth   TX      * Amarillo            TX   1.00   3
    ## 2      * Atlanta             GA      * Baltimore/Wash Intl MD   1.06   3
    ## 3      * Boston              MA      * Baltimore/Wash Intl MD   1.06   3
    ## 4    ORD Chicago             IL      * Baltimore/Wash Intl MD   1.06   3
    ## 5    MDW Chicago             IL      * Baltimore/Wash Intl MD   1.06   3
    ## 6      * Cleveland           OH      * Baltimore/Wash Intl MD   1.01   3
    ##   VACATION  SW      HI S_INCOME E_INCOME   S_POP   E_POP       SLOT GATE
    ## 1       No Yes 5291.99    28637    21112 3036732  205711       Free Free
    ## 2       No  No 5419.16    26993    29838 3532657 7145897       Free Free
    ## 3       No  No 9185.28    30124    29838 5787293 7145897       Free Free
    ## 4       No Yes 2657.35    29260    29838 7830332 7145897 Controlled Free
    ## 5       No Yes 2657.35    29260    29838 7830332 7145897       Free Free
    ## 6       No Yes 3408.11    26046    29838 2230955 7145897       Free Free
    ##   DISTANCE   PAX   FARE
    ## 1      312  7864  64.11
    ## 2      576  8820 174.47
    ## 3      364  6452 207.76
    ## 4      612 25144  85.47
    ## 5      612 25144  85.47
    ## 6      309 13386  56.76

``` r
#remove the four predictors S_CODE,S_CITY,E_CODE,E_CITY
airfares.df <- airfares_raw.df[,!(names(airfares_raw.df) %in% c('S_CODE','S_CITY','E_CODE','E_CITY'))]
str(airfares.df)
```

    ## 'data.frame':    638 obs. of  14 variables:
    ##  $ COUPON  : num  1 1.06 1.06 1.06 1.06 1.01 1.28 1.15 1.33 1.6 ...
    ##  $ NEW     : int  3 3 3 3 3 3 3 3 3 2 ...
    ##  $ VACATION: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 1 ...
    ##  $ SW      : Factor w/ 2 levels "No","Yes": 2 1 1 2 2 2 1 2 2 2 ...
    ##  $ HI      : num  5292 5419 9185 2657 2657 ...
    ##  $ S_INCOME: num  28637 26993 30124 29260 29260 ...
    ##  $ E_INCOME: num  21112 29838 29838 29838 29838 ...
    ##  $ S_POP   : int  3036732 3532657 5787293 7830332 7830332 2230955 3036732 1440377 3770125 1694803 ...
    ##  $ E_POP   : int  205711 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 ...
    ##  $ SLOT    : Factor w/ 2 levels "Controlled","Free": 2 2 2 1 2 2 2 2 2 2 ...
    ##  $ GATE    : Factor w/ 2 levels "Constrained",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DISTANCE: int  312 576 364 612 612 309 1220 921 1249 964 ...
    ##  $ PAX     : int  7864 8820 6452 25144 25144 13386 4625 5512 7811 4657 ...
    ##  $ FARE    : num  64.1 174.5 207.8 85.5 85.5 ...

``` r
head(airfares.df)
```

    ##   COUPON NEW VACATION  SW      HI S_INCOME E_INCOME   S_POP   E_POP
    ## 1   1.00   3       No Yes 5291.99    28637    21112 3036732  205711
    ## 2   1.06   3       No  No 5419.16    26993    29838 3532657 7145897
    ## 3   1.06   3       No  No 9185.28    30124    29838 5787293 7145897
    ## 4   1.06   3       No Yes 2657.35    29260    29838 7830332 7145897
    ## 5   1.06   3       No Yes 2657.35    29260    29838 7830332 7145897
    ## 6   1.01   3       No Yes 3408.11    26046    29838 2230955 7145897
    ##         SLOT GATE DISTANCE   PAX   FARE
    ## 1       Free Free      312  7864  64.11
    ## 2       Free Free      576  8820 174.47
    ## 3       Free Free      364  6452 207.76
    ## 4 Controlled Free      612 25144  85.47
    ## 5       Free Free      612 25144  85.47
    ## 6       Free Free      309 13386  56.76

``` r
#remove non-numeric values
airfares1.df <- airfares.df[,-c(3,4,10,11)]
str(airfares1.df)
```

    ## 'data.frame':    638 obs. of  10 variables:
    ##  $ COUPON  : num  1 1.06 1.06 1.06 1.06 1.01 1.28 1.15 1.33 1.6 ...
    ##  $ NEW     : int  3 3 3 3 3 3 3 3 3 2 ...
    ##  $ HI      : num  5292 5419 9185 2657 2657 ...
    ##  $ S_INCOME: num  28637 26993 30124 29260 29260 ...
    ##  $ E_INCOME: num  21112 29838 29838 29838 29838 ...
    ##  $ S_POP   : int  3036732 3532657 5787293 7830332 7830332 2230955 3036732 1440377 3770125 1694803 ...
    ##  $ E_POP   : int  205711 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 7145897 ...
    ##  $ DISTANCE: int  312 576 364 612 612 309 1220 921 1249 964 ...
    ##  $ PAX     : int  7864 8820 6452 25144 25144 13386 4625 5512 7811 4657 ...
    ##  $ FARE    : num  64.1 174.5 207.8 85.5 85.5 ...

``` r
head(airfares1.df)
```

    ##   COUPON NEW      HI S_INCOME E_INCOME   S_POP   E_POP DISTANCE   PAX
    ## 1   1.00   3 5291.99    28637    21112 3036732  205711      312  7864
    ## 2   1.06   3 5419.16    26993    29838 3532657 7145897      576  8820
    ## 3   1.06   3 9185.28    30124    29838 5787293 7145897      364  6452
    ## 4   1.06   3 2657.35    29260    29838 7830332 7145897      612 25144
    ## 5   1.06   3 2657.35    29260    29838 7830332 7145897      612 25144
    ## 6   1.01   3 3408.11    26046    29838 2230955 7145897      309 13386
    ##     FARE
    ## 1  64.11
    ## 2 174.47
    ## 3 207.76
    ## 4  85.47
    ## 5  85.47
    ## 6  56.76

``` r
#Correlation Table between FARE and the predictors
airfarecor <- round(cor(airfares1.df,airfares1.df$FARE),2) 
colnames(airfarecor) <- 'FARE' 
airfarecor
```

    ##           FARE
    ## COUPON    0.50
    ## NEW       0.09
    ## HI        0.03
    ## S_INCOME  0.21
    ## E_INCOME  0.33
    ## S_POP     0.15
    ## E_POP     0.29
    ## DISTANCE  0.67
    ## PAX      -0.09
    ## FARE      1.00

``` r
#Scatterplot between FARE and the predictors
ggplot(data= airfares1.df, aes(COUPON, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#COUPON v FARE
```

![](README_figs/README-Question%201-1.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(NEW, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#NEW V FARE
```

![](README_figs/README-Question%201-2.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(HI, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#HI V FARE
```

![](README_figs/README-Question%201-3.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(S_INCOME, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#S_INCOME V FARE
```

![](README_figs/README-Question%201-4.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(E_INCOME, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#E_INCOME V FARE
```

![](README_figs/README-Question%201-5.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(S_POP, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#S_POP V FARE
```

![](README_figs/README-Question%201-6.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(E_POP, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#E_POP V FARE
```

![](README_figs/README-Question%201-7.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(DISTANCE, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#DISTANCE V FARE
```

![](README_figs/README-Question%201-8.png)<!-- -->

``` r
ggplot(data= airfares1.df, aes(PAX, FARE))+geom_point(color='blue')+geom_smooth(method="lm",se=FALSE)#PAX V FARE
```

![](README_figs/README-Question%201-9.png)<!-- -->

Explanation : \*DISTANCE seems to be the best single predictor of FARE
since the correlation between the two (0.67) is quite strong as compared
to others and also it is quite understandable from the scatterplot that
the two variables - FARE and DISTANCE have a strong
correlation.

# Question 2

## Explore the categorical predictors by computing the percentage of flights in each category. Create a pivot table with the average fare in each category. Which categorical predictor seems best for predicting FARE? Explain your answer.

``` r
#Percentage of flights in VACATION categories
vacation_category <- transform(as.data.frame(table(airfares.df$VACATION)),Percentage=Freq/nrow(airfares.df)*100)
names(vacation_category)[c(1,3)]=c('Vacation Route','Percentage of Flights')
vacation_category
```

    ##   Vacation Route Freq Percentage of Flights
    ## 1             No  468              73.35423
    ## 2            Yes  170              26.64577

``` r
#Percentage of flights in SW categories
sw_category <- transform(as.data.frame(table(airfares.df$SW)),Percentage=Freq/nrow(airfares.df)*100)
names(sw_category)[c(1,3)]=c('Southwest Airlines serving the route','Percentage of Flights')
sw_category
```

    ##   Southwest Airlines serving the route Freq Percentage of Flights
    ## 1                                   No  444              69.59248
    ## 2                                  Yes  194              30.40752

``` r
#Percentage of flights in SLOT categories
slot_category <- transform(as.data.frame(table(airfares.df$SLOT)),Percentage=Freq/nrow(airfares.df)*100)
names(slot_category)[c(1,3)]=c('End Airport SLOT','Percentage of Flights')
slot_category
```

    ##   End Airport SLOT Freq Percentage of Flights
    ## 1       Controlled  182              28.52665
    ## 2             Free  456              71.47335

``` r
#Percentage of flights in GATE categories
gate_category <- transform(as.data.frame(table(airfares.df$GATE)),Percentage=Freq/nrow(airfares.df)*100)
names(gate_category)[c(1,3)]=c('End Airport GATE','Percentage of Flights')
gate_category
```

    ##   End Airport GATE Freq Percentage of Flights
    ## 1      Constrained  124              19.43574
    ## 2             Free  514              80.56426

``` r
#Pivot Table with average fare in VACATION categories
pivot_vacation <- airfares.df %>%
  group_by(VACATION) %>% summarize(AVG_FARE=mean(FARE))
pivot_vacation
```

    ## # A tibble: 2 x 2
    ##   VACATION AVG_FARE
    ##   <fct>       <dbl>
    ## 1 No           174.
    ## 2 Yes          126.

``` r
#Pivot Table with average fare in SW categories
pivot_sw <- airfares.df %>%
  group_by(SW) %>% summarize(AVG_FARE=mean(FARE))
pivot_sw
```

    ## # A tibble: 2 x 2
    ##   SW    AVG_FARE
    ##   <fct>    <dbl>
    ## 1 No       188. 
    ## 2 Yes       98.4

``` r
#Pivot Table with average fare in SLOT categories
pivot_slot <- airfares.df %>%
  group_by(SLOT) %>% summarize(AVG_FARE=mean(FARE))
pivot_slot
```

    ## # A tibble: 2 x 2
    ##   SLOT       AVG_FARE
    ##   <fct>         <dbl>
    ## 1 Controlled     186.
    ## 2 Free           151.

``` r
#Pivot Table with average fare in GATE categories
pivot_gate <- airfares.df %>%
  group_by(GATE) %>% summarize(AVG_FARE=mean(FARE))
pivot_gate
```

    ## # A tibble: 2 x 2
    ##   GATE        AVG_FARE
    ##   <fct>          <dbl>
    ## 1 Constrained     193.
    ## 2 Free            153.

Explanation: \* The SW(Southwest Airlines) categorical predictor seems
best for predicting FARE. We observe that the average FARE of
SW(Southwest Airlines) is spread. Flights from Southwest has an average
of 98.38 and flights that are not from Southwest has an average of
188.18. Thus SW affects the price of FARE the
most.

# Question 3

## Create data partition by assigning 80% of the records to the training dataset. Use rounding if 80% of the index generates a fraction. Also, set the seed at 42.

``` r
#Create data partition by assigning 80% of the records to the training dataset
set.seed(42) 
train.index <- sample(c(nrow(airfares.df)),round(0.8*nrow(airfares.df))) 
length(train.index) 
```

    ## [1] 510

``` r
airfaretrain.df<-airfares.df[train.index,] 
airfarevalid.df<-airfares.df[-train.index,] 
head(airfaretrain.df) 
```

    ##     COUPON NEW VACATION SW      HI S_INCOME E_INCOME   S_POP   E_POP
    ## 561   1.61   3      Yes No 3873.00    24706    23654 9056076 2195215
    ## 321   1.08   3      Yes No 2386.85    32991    22360 8621121 1421287
    ## 153   1.42   3      Yes No 4956.58    24502    24575  125722 1197234
    ## 74    1.14   3       No No 6205.97    29260    28637 7830332 3036732
    ## 228   1.00   3       No No 2850.33    30124    32991 5787293 8621121
    ## 146   1.44   3      Yes No 4112.44    26993    24575 3532657 1197234
    ##           SLOT GATE DISTANCE   PAX   FARE
    ## 561       Free Free     2164  4065 224.21
    ## 321 Controlled Free      947 54990 123.97
    ## 153       Free Free     1769  7231 115.84
    ## 74        Free Free      805 20264 244.50
    ## 228 Controlled Free      183 66820 116.78
    ## 146       Free Free     1744  7881 143.62

``` r
head(airfarevalid.df) 
```

    ##    COUPON NEW VACATION  SW      HI S_INCOME E_INCOME   S_POP   E_POP
    ## 4    1.06   3       No Yes 2657.35    29260    29838 7830332 7145897
    ## 5    1.06   3       No Yes 2657.35    29260    29838 7830332 7145897
    ## 6    1.01   3       No Yes 3408.11    26046    29838 2230955 7145897
    ## 7    1.28   3       No  No 6754.48    28637    29838 3036732 7145897
    ## 9    1.33   3       No Yes 4662.44    27211    29838 3770125 7145897
    ## 13   1.12   3       No Yes 4471.62    25995    29838 1115048 7145897
    ##          SLOT GATE DISTANCE   PAX   FARE
    ## 4  Controlled Free      612 25144  85.47
    ## 5        Free Free      612 25144  85.47
    ## 6        Free Free      309 13386  56.76
    ## 7        Free Free     1220  4625 228.00
    ## 9        Free Free     1249  7811 172.63
    ## 13       Free Free      587  5654  79.17

``` r
nrow(airfarevalid.df)
```

    ## [1] 128

# Question 4

## Using leaps package, run stepwise regression to reduce the number of predictors. Discuss the results from this model.

``` r
#Create Linear Model  
airfare.lm <- lm(FARE ~ ., data = airfaretrain.df) 
options(scipen=999)
summary(airfare.lm)   
```

    ## 
    ## Call:
    ## lm(formula = FARE ~ ., data = airfaretrain.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -99.282 -23.384  -2.476  22.156 106.501 
    ## 
    ## Coefficients:
    ##                   Estimate     Std. Error t value             Pr(>|t|)    
    ## (Intercept)  13.8781441835  30.7076946550   0.452             0.651507    
    ## COUPON       11.6744988371  13.6949175687   0.852             0.394365    
    ## NEW          -2.2468005921   2.0827213457  -1.079             0.281210    
    ## VACATIONYes -37.8385127965   3.9788129464  -9.510 < 0.0000000000000002 ***
    ## SWYes       -38.9566477546   4.2526101838  -9.161 < 0.0000000000000002 ***
    ## HI            0.0085414832   0.0010936608   7.810   0.0000000000000343 ***
    ## S_INCOME      0.0006160967   0.0005709965   1.079             0.281119    
    ## E_INCOME      0.0015472928   0.0004141497   3.736             0.000209 ***
    ## S_POP         0.0000040087   0.0000007411   5.409   0.0000000987167149 ***
    ## E_POP         0.0000039572   0.0000008329   4.751   0.0000026562530825 ***
    ## SLOTFree    -16.4322948237   4.3647846605  -3.765             0.000187 ***
    ## GATEFree    -21.1634823059   4.4093579183  -4.800   0.0000021065804690 ***
    ## DISTANCE      0.0715673994   0.0039223121  18.246 < 0.0000000000000002 ***
    ## PAX          -0.0007340587   0.0001662490  -4.415   0.0000123830100844 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 35.41 on 496 degrees of freedom
    ## Multiple R-squared:  0.7817, Adjusted R-squared:  0.7759 
    ## F-statistic: 136.6 on 13 and 496 DF,  p-value: < 0.00000000000000022

``` r
#Stepwise Regression using Leaps package to reduce the number of predictors
airfare.lm.stepwise <- regsubsets(FARE~., data= airfaretrain.df, nbest = 1, nvmax=dim(airfaretrain.df)[2],method = "seqrep") 
sum1 <- summary(airfare.lm.stepwise)

sum1$which
```

    ##    (Intercept) COUPON   NEW VACATIONYes SWYes    HI S_INCOME E_INCOME
    ## 1         TRUE  FALSE FALSE       FALSE FALSE FALSE    FALSE    FALSE
    ## 2         TRUE  FALSE FALSE       FALSE  TRUE FALSE    FALSE    FALSE
    ## 3         TRUE  FALSE FALSE        TRUE  TRUE FALSE    FALSE    FALSE
    ## 4         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 5         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 6         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 7         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 8         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 9         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 10        TRUE   TRUE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ## 11        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 12        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ## 13        TRUE   TRUE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ##    S_POP E_POP SLOTFree GATEFree DISTANCE   PAX
    ## 1  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 2  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 3  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 4  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 5  FALSE FALSE     TRUE    FALSE     TRUE FALSE
    ## 6  FALSE FALSE     TRUE     TRUE     TRUE FALSE
    ## 7  FALSE FALSE     TRUE     TRUE     TRUE FALSE
    ## 8   TRUE  TRUE    FALSE    FALSE     TRUE  TRUE
    ## 9   TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 10  TRUE  TRUE     TRUE    FALSE    FALSE FALSE
    ## 11  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 12  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 13  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE

``` r
print("R Square")
```

    ## [1] "R Square"

``` r
sum1$rsq
```

    ##  [1] 0.4168069 0.5793894 0.6966218 0.7232479 0.7366555 0.7565835 0.7604199
    ##  [8] 0.7674947 0.7748171 0.6303171 0.7809073 0.7813501 0.7816700

``` r
print("Adjusted R Square")
```

    ## [1] "Adjusted R Square"

``` r
sum1$adjr2
```

    ##  [1] 0.4156589 0.5777302 0.6948231 0.7210558 0.7340429 0.7536799 0.7570792
    ##  [8] 0.7637820 0.7707638 0.6229086 0.7760679 0.7760708 0.7759476

``` r
print("Mallow's Cp")
```

    ## [1] "Mallow's Cp"

``` r
sum1$cp
```

    ##  [1] 818.89220 451.53899 187.21153 128.72255 100.26346  56.99127  50.27558
    ##  [8]  36.20326  21.56831 351.84190  11.73270  12.72670  14.00000

Explanation : \*When we look at adjusted R square, we see that model
with 11 predictors gives us the highest adjusted R square value. Thus
stepwise function has improved the model by removing the “New”,
S\_INCOME and “Coupon” predictors. The final list of variables are
VACATION + SW + HI + E\_INCOME + S\_POP + E\_POP + SLOT + GATE +
DISTANCE +
PAX.

# Question 5

## Repeat the process in (4) using exhaustive search instead of stepwise regression. Compare the resulting best model to the one you obtained in (4) in terms of the predictors included in the final model.

``` r
#Exhaustive Search 
airfare.exhaust <-regsubsets(FARE~., data= airfaretrain.df, nbest = 1, nvmax=dim(airfaretrain.df)[2],                                             method = "exhaustive") 
sum <- summary(airfare.exhaust)

#show models 
sum$which
```

    ##    (Intercept) COUPON   NEW VACATIONYes SWYes    HI S_INCOME E_INCOME
    ## 1         TRUE  FALSE FALSE       FALSE FALSE FALSE    FALSE    FALSE
    ## 2         TRUE  FALSE FALSE       FALSE  TRUE FALSE    FALSE    FALSE
    ## 3         TRUE  FALSE FALSE        TRUE  TRUE FALSE    FALSE    FALSE
    ## 4         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 5         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 6         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 7         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 8         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 9         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 10        TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 11        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 12        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ## 13        TRUE   TRUE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ##    S_POP E_POP SLOTFree GATEFree DISTANCE   PAX
    ## 1  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 2  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 3  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 4  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 5  FALSE FALSE     TRUE    FALSE     TRUE FALSE
    ## 6  FALSE FALSE     TRUE     TRUE     TRUE FALSE
    ## 7   TRUE  TRUE    FALSE    FALSE     TRUE  TRUE
    ## 8   TRUE  TRUE    FALSE    FALSE     TRUE  TRUE
    ## 9   TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 10  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 11  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 12  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 13  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE

``` r
#show metrics
sum$rsq 
```

    ##  [1] 0.4168069 0.5793894 0.6966218 0.7232479 0.7366555 0.7565835 0.7607777
    ##  [8] 0.7674947 0.7748171 0.7803115 0.7809073 0.7813501 0.7816700

``` r
sum$adjr2 
```

    ##  [1] 0.4156589 0.5777302 0.6948231 0.7210558 0.7340429 0.7536799 0.7574419
    ##  [8] 0.7637820 0.7707638 0.7759090 0.7760679 0.7760708 0.7759476

``` r
sum$cp       
```

    ##  [1] 818.89220 451.53899 187.21153 128.72255 100.26346  56.99127  49.46286
    ##  [8]  36.20326  21.56831  11.08605  11.73270  12.72670  14.00000

Explanation : \* We see that there is not much significant increase in
the adjusted R square after the 10th predictor. In the Mallow CP output
we can see no change after the 10th predictor and the value becomes
11.08605. This shows us that the model should have 10 predictors. Thus
we see that in stepwise the number of predictors shown are 11 and in
exhaustive search the number of predictors are
10.

# Question 6

## Compare the predictive accuracy of both models—stepwise regression and exhaustive search—using measures such as RMSE.

``` r
airfare.lm<-lm(formula = FARE ~ NEW+VACATION + SW + HI + E_INCOME + S_POP + E_POP +
SLOT + GATE + DISTANCE + PAX, data = airfaretrain.df )
airfare.lm.pred <- predict(airfare.lm,airfarevalid.df)
accuracy(airfare.lm.pred,airfarevalid.df$FARE)
```

    ##                ME     RMSE      MAE       MPE     MAPE
    ## Test set 3.166677 36.82363 27.57897 -5.812025 21.44043

``` r
airfare.exhaust<-lm(formula = FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP +
SLOT + GATE + DISTANCE + PAX, data = airfaretrain.df)
airfare.exhaust.pred <- predict(airfare.exhaust,airfarevalid.df)
accuracy(airfare.exhaust.pred,airfarevalid.df$FARE)
```

    ##               ME    RMSE      MAE       MPE     MAPE
    ## Test set 3.06081 36.8617 27.70568 -5.938062 21.62142

Explantion: \* The RMSE of Stepwise Regression is 36.82363 and RMSE for
the Exhaustive Regression is 36.8617. This shows that the Stepwise
regression is better as its Root Mean Squared Error is
lower.

# Question 7

## Using the exhaustive search model, predict the average fare on a route with the following characteristics: COUPON = 1.202, NEW = 3, VACATION = No, SW = No, HI = 4442.141, S\_INCOME = $28,760, E\_INCOME = $27,664, S\_POP = 4,557,004, E\_POP = 3,195,503, SLOT = Free, GATE = Free, PAX = 12,782, DISTANCE = 1976 miles.

``` r
valida.df <- data.frame('COUPON' = 1.202, 'NEW' = 3, 'VACATION' = 'No', 'SW' =
'No', 'HI' = 4442.141, 'S_INCOME' = 28760, 'E_INCOME' = 27664, 'S_POP' =
4557004, 'E_POP' = 3195503, 'SLOT' = 'Free', 'GATE' = 'Free', 'PAX' = 12782,
'DISTANCE' = 1976)


airfare.lm<-lm(formula = FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP + 
    SLOT + GATE + DISTANCE + PAX, data = airfaretrain.df )
airfare.lm.pred <- predict(airfare.lm,valida.df)
#pacman::p_load(data.table, forecast, leaps, tidyverse)
airfare.lm.pred
```

    ##       1 
    ## 247.684

Explanation: \* The average fare with the above test values is
247.684

# Question 8

## Predict the reduction in average fare on the route in question (7.), if Southwest decides to cover this route \[using the exhaustive search model above\].

``` r
valida1.df <- data.frame('COUPON' = 1.202, 'NEW' = 3, 'VACATION' = 'No', 'SW' =
'Yes', 'HI' = 4442.141, 'S_INCOME' = 28760, 'E_INCOME' = 27664, 'S_POP' =
4557004, 'E_POP' = 3195503, 'SLOT' = 'Free', 'GATE' = 'Free', 'PAX' = 12782,
'DISTANCE' = 1976)

airfare.lm<-lm(formula = FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP + 
    SLOT + GATE + DISTANCE + PAX, data = airfaretrain.df )
airfare.lm.pred <- predict(airfare.lm,valida1.df)
airfare.lm.pred
```

    ##        1 
    ## 207.1558

Explanation: \* If southwest decides to cover this route we get a
reduction of 40. SW being the best categorical factor it affects the
price and the fair drops from 247.684 to
207.1558.

# Question 9

## Using leaps package, run backward selection regression to reduce the number of predictors. Discuss the results from this model.

``` r
airfares.back <- regsubsets(FARE ~ ., data = airfaretrain.df, nbest = 1, nvmax = dim(airfares.df)[2],
method = "backward")
summ_back <- summary(airfares.back)

summ_back$which
```

    ##    (Intercept) COUPON   NEW VACATIONYes SWYes    HI S_INCOME E_INCOME
    ## 1         TRUE  FALSE FALSE       FALSE FALSE FALSE    FALSE    FALSE
    ## 2         TRUE  FALSE FALSE       FALSE  TRUE FALSE    FALSE    FALSE
    ## 3         TRUE  FALSE FALSE        TRUE  TRUE FALSE    FALSE    FALSE
    ## 4         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 5         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 6         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 7         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 8         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 9         TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE    FALSE
    ## 10        TRUE  FALSE FALSE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 11        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE    FALSE     TRUE
    ## 12        TRUE  FALSE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ## 13        TRUE   TRUE  TRUE        TRUE  TRUE  TRUE     TRUE     TRUE
    ##    S_POP E_POP SLOTFree GATEFree DISTANCE   PAX
    ## 1  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 2  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 3  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 4  FALSE FALSE    FALSE    FALSE     TRUE FALSE
    ## 5  FALSE  TRUE    FALSE    FALSE     TRUE FALSE
    ## 6   TRUE  TRUE    FALSE    FALSE     TRUE FALSE
    ## 7   TRUE  TRUE    FALSE    FALSE     TRUE  TRUE
    ## 8   TRUE  TRUE    FALSE     TRUE     TRUE  TRUE
    ## 9   TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 10  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 11  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 12  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE
    ## 13  TRUE  TRUE     TRUE     TRUE     TRUE  TRUE

``` r
print("R square")
```

    ## [1] "R square"

``` r
summ_back$rsq
```

    ##  [1] 0.4168069 0.5793894 0.6966218 0.7232479 0.7322282 0.7509946 0.7607777
    ##  [8] 0.7663728 0.7748171 0.7803115 0.7809073 0.7813501 0.7816700

``` r
print("Adjusted R square")
```

    ## [1] "Adjusted R square"

``` r
summ_back$adjr2
```

    ##  [1] 0.4156589 0.5777302 0.6948231 0.7210558 0.7295718 0.7480243 0.7574419
    ##  [8] 0.7626422 0.7707638 0.7759090 0.7760679 0.7760708 0.7759476

``` r
print("Mallow’s Cp")
```

    ## [1] "Mallow’s Cp"

``` r
summ_back$cp
```

    ##  [1] 818.89220 451.53899 187.21153 128.72255 110.32120  69.68802  49.46286
    ##  [8]  38.75199  21.56831  11.08605  11.73270  12.72670  14.00000

Explanation: \* We see that the backward regression has improved the
model by removing the “New” and “Coupon” predictors. The value of
adjusted R square does not change after the 10th predictor. The value Cp
increases very less after 11.08605. Thus a 10 predictor model is the
best.

# Question 10

## Now run a backward selection model using stepAIC() function. Discuss the results from this model, including the role of AIC in this model.

``` r
airfare.lm.bselect.aic <- stepAIC(airfare.lm, direction = "backward")
```

    ## Start:  AIC=3649.22
    ## FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP + SLOT + 
    ##     GATE + DISTANCE + PAX
    ## 
    ##            Df Sum of Sq     RSS    AIC
    ## <none>                   625690 3649.2
    ## - E_INCOME  1     15649  641339 3659.8
    ## - SLOT      1     19217  644907 3662.6
    ## - E_POP     1     28766  654456 3670.1
    ## - GATE      1     29165  654856 3670.5
    ## - PAX       1     32706  658396 3673.2
    ## - S_POP     1     42648  668338 3680.9
    ## - HI        1     78891  704581 3707.8
    ## - SW        1    126577  752267 3741.2
    ## - VACATION  1    127066  752756 3741.5
    ## - DISTANCE  1    825966 1451656 4076.4

``` r
summary(airfare.lm.bselect.aic)
```

    ## 
    ## Call:
    ## lm(formula = FARE ~ VACATION + SW + HI + E_INCOME + S_POP + E_POP + 
    ##     SLOT + GATE + DISTANCE + PAX, data = airfaretrain.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -99.148 -22.077  -2.028  21.491 107.744 
    ## 
    ## Coefficients:
    ##                   Estimate     Std. Error t value             Pr(>|t|)    
    ## (Intercept)  42.0764345686  14.7566725244   2.851             0.004534 ** 
    ## VACATIONYes -38.7574569132   3.8500841929 -10.067 < 0.0000000000000002 ***
    ## SWYes       -40.5282166043   4.0337560764 -10.047 < 0.0000000000000002 ***
    ## HI            0.0082681499   0.0010423739   7.932   0.0000000000000143 ***
    ## E_INCOME      0.0014446281   0.0004089281   3.533             0.000450 ***
    ## S_POP         0.0000041850   0.0000007176   5.832   0.0000000098509604 ***
    ## E_POP         0.0000037791   0.0000007890   4.790   0.0000022053722984 ***
    ## SLOTFree    -16.8515659965   4.3045728245  -3.915             0.000103 ***
    ## GATEFree    -21.2165142735   4.3991611435  -4.823   0.0000018824635124 ***
    ## DISTANCE      0.0736714582   0.0028704349  25.666 < 0.0000000000000002 ***
    ## PAX          -0.0007619280   0.0001491869  -5.107   0.0000004660838631 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 35.41 on 499 degrees of freedom
    ## Multiple R-squared:  0.7803, Adjusted R-squared:  0.7759 
    ## F-statistic: 177.2 on 10 and 499 DF,  p-value: < 0.00000000000000022

Explanation: \*After running backward selection model using stepAIC(),
we see that we get a much improved model or the best model by AIC by
removing the “Coupon”,“S\_INCOME” and “NEW” predictors which were least
significanT or less contributive towards FARE and this model maintained
a lower AIC of 3649.22 by removing these predictors.The adjusted
R-squared value of .7759 indicates that this is fairly good model. The
extremely small P-value for the model also indicates this. The role of
AIC in this model is to check that after dropping a predictor, how much
it would affect the AIC and we need to basically lower the AIC until it
can be lowered to get the best model.
