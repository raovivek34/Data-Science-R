Classification\_Analysis\_Spam
================

``` r
#Pacman tool used for Package-management
if(!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(fpp2,lattice, gains, e1071, pROC, gains, MASS, caret, tidyverse, dplyr) 
search()
```

    ##  [1] ".GlobalEnv"        "package:forcats"   "package:stringr"  
    ##  [4] "package:dplyr"     "package:purrr"     "package:readr"    
    ##  [7] "package:tidyr"     "package:tibble"    "package:tidyverse"
    ## [10] "package:caret"     "package:MASS"      "package:pROC"     
    ## [13] "package:e1071"     "package:gains"     "package:lattice"  
    ## [16] "package:fpp2"      "package:expsmooth" "package:fma"      
    ## [19] "package:forecast"  "package:ggplot2"   "package:pacman"   
    ## [22] "package:stats"     "package:graphics"  "package:grDevices"
    ## [25] "package:utils"     "package:datasets"  "package:methods"  
    ## [28] "Autoloads"         "package:base"

``` r
spamBaseFile<-file("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",open = "r", encoding = "UTF-8")
data.df<-read.table(spamBaseFile,sep = ",")
colnames(data.df) <- c ('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d', 'word_freq_our',
                        'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order', 
                        'word_freq_mail', 'word_freq_receive', 'word_freq_will', 'word_freq_people', 
                        'word_freq_report', 'word_freq_addresses', 'word_freq_free', 'word_freq_business',
                        'word_freq_email', 'word_freq_you', 'word_freq_credit', 'word_freq_your', 'word_freq_font',
                        'word_freq_000', 'word_freq_money', 'word_freq_hp', 'word_freq_hpl', 'word_freq_george', 
                        'word_freq_650', 'word_freq_lab', 'word_freq_labs', 'word_freq_telnet', 'word_freq_857', 
                        'word_freq_data', 'word_freq_415', 'word_freq_85', 'word_freq_technology','word_freq_1999',
                        'word_freq_parts', 'word_freq_pm', 'word_freq_direct', 'word_freq_cs', 'word_freq_meeting',
                        'word_freq_original', 'word_freq_project', 'word_freq_re', 'word_freq_edu',
                        'word_freq_table', 'word_freq_conference', 'char_freq_;', 'char_freq_(', 'char_freq_[', 
                        'char_freq_!', 'char_freq_$', 'char_freq_#', 'capital_run_length_average', 
                        'capital_run_length_longest', 'capital_run_length_total', 'spam')
```

# Question 1

## Examine how each predictor differs between the spam and non-spam e-mails by comparing the spam-class average and non-spam-class average. Identify 10 predictors for which the difference between the spam-class average and nonspam class average is highest.

``` r
# Converting the Spam variable to a categorical variable
data.df$spam <- as.factor(data.df$spam)

Aggregate <- aggregate(data.df[, 1:57],list(data.df$spam), FUN = mean)
Aggregate <- Aggregate[,-1]

#Examining all the predictors of Spam emails
spam.df <- data.df[data.df$spam == 1,]
#Examining all the predictors of Non-Spam emails
nonspam.df <- data.df[data.df$spam == 0,]

#Selecting  10 predictors for which the difference between the spam-class average and nonspam-class average is highest
Difference <- abs(Aggregate[2,] - Aggregate[1,])
Difference1 <- Difference[,-1]
Sorted<-head(t(sort(Difference1, decreasing = TRUE)),10)
Sorted
```

    ##                                      2
    ## capital_run_length_total   309.1484684
    ## capital_run_length_longest  86.1787801
    ## capital_run_length_average   7.1418640
    ## word_freq_george             1.2637155
    ## word_freq_you                0.9941987
    ## word_freq_your               0.9416680
    ## word_freq_hp                 0.8779941
    ## word_freq_free               0.4447750
    ## word_freq_hpl                0.4228216
    ## char_freq_!                  0.4037291

Answer : The predictors which have the highest difference between the
spam and non-spam averages i order of decreasing differences are:
capital\_run\_length\_total, capital\_run\_length\_longest,
capital\_run\_length\_average, word\_freq\_george, word\_freq\_you,
word\_freq\_your, word\_freq\_hp, word\_freq\_free, word\_freq\_hpl,
char\_freq\_\!,
word\_freq\_our.

# Question 2

## Perform a linear discriminant analysis using the training dataset. Include only 10 predictors identified in the question above in the model.

``` r
finalpredictors.df <- data.df[,c('capital_run_length_total','capital_run_length_longest','capital_run_length_average',
                       'word_freq_george','word_freq_you','word_freq_your','word_freq_hp','word_freq_free',
                       'word_freq_hpl',"char_freq_!",'spam')]
levels(finalpredictors.df$spam) <- c("non-spam","spam")

#Splitting the data into training set (80%) and validation set (20%)
set.seed(42)
training.index <- createDataPartition(finalpredictors.df$spam, p = 0.8, list = FALSE)
finalpredictors.train <- finalpredictors.df[training.index, ]
finalpredictors.valid <- finalpredictors.df[-training.index, ]


# Normalizing the data
# Estimate preprocessing parameters
normval <- preProcess(finalpredictors.train, method = c("center", "scale"))
# Transform the data using the estimated parameters
normtrain.df <- predict(normval, finalpredictors.train)
normvalid.df <- predict(normval, finalpredictors.valid)

#Linear Discriminant Analysis
LDA <- lda(spam ~ ., data = normtrain.df)
LDA
```

    ## Call:
    ## lda(spam ~ ., data = normtrain.df)
    ## 
    ## Prior probabilities of groups:
    ##  non-spam      spam 
    ## 0.6059207 0.3940793 
    ## 
    ## Group means:
    ##          capital_run_length_total capital_run_length_longest
    ## non-spam               -0.2004815                 -0.1686439
    ## spam                    0.3082524                  0.2593001
    ##          capital_run_length_average word_freq_george word_freq_you
    ## non-spam                -0.08525316        0.1459946    -0.2308880
    ## spam                     0.13108188       -0.2244754     0.3550043
    ##          word_freq_your word_freq_hp word_freq_free word_freq_hpl
    ## non-spam     -0.3183569    0.2060657     -0.2027840     0.1845683
    ## spam          0.4894929   -0.3168384      0.3117927    -0.2837848
    ##          `char_freq_!`
    ## non-spam    -0.2012532
    ## spam         0.3094390
    ## 
    ## Coefficients of linear discriminants:
    ##                                    LD1
    ## capital_run_length_total    0.37245497
    ## capital_run_length_longest  0.12966688
    ## capital_run_length_average  0.05268179
    ## word_freq_george           -0.21043836
    ## word_freq_you               0.24656232
    ## word_freq_your              0.57149367
    ## word_freq_hp               -0.23541887
    ## word_freq_free              0.38749847
    ## word_freq_hpl              -0.15061430
    ## `char_freq_!`               0.32675618

# Question 3

## What are the prior probabilities?

``` r
LDA$prior
```

    ##  non-spam      spam 
    ## 0.6059207 0.3940793

Answer 3: Prior probabilities of groups: non-spam: 0.6059207 and spam:
0.3940793. In other words, 60.5% of the training observations are
non-spam and 39.4% represent spam.

# Question 4

## What are the coefficients of linear discriminants? Explain.

``` r
coefficients(LDA)
```

    ##                                    LD1
    ## capital_run_length_total    0.37245497
    ## capital_run_length_longest  0.12966688
    ## capital_run_length_average  0.05268179
    ## word_freq_george           -0.21043836
    ## word_freq_you               0.24656232
    ## word_freq_your              0.57149367
    ## word_freq_hp               -0.23541887
    ## word_freq_free              0.38749847
    ## word_freq_hpl              -0.15061430
    ## `char_freq_!`               0.32675618

Coefficients of linear discriminants helps us to classify the data into
spam and non-spam. LDA computes “discriminant scores” for each
observation to classify what response variable class it is in (i.e. spam
or non-spam). The coefficients of linear discriminants output provides
the linear combination of spam and non-spam that are used to form the
LDA decision rule. In other words, these are the multipliers of the
elements in the LDA classifier
equation.

# Question 5

## Generate linear discriminants using your analysis. How are they used in classifying spams and non-spams?

``` r
pred.sample <- predict(LDA, normtrain.df[1:5,])
pred.sample$x
```

    ##          LD1
    ## 1 0.51887396
    ## 2 1.26103146
    ## 3 1.34711434
    ## 4 0.04469274
    ## 5 0.04387683

``` r
pred.sample$posterior
```

    ##    non-spam      spam
    ## 1 0.4725604 0.5274396
    ## 2 0.2273409 0.7726591
    ## 3 0.2054547 0.7945453
    ## 4 0.6460185 0.3539815
    ## 5 0.6462983 0.3537017

LDA classification is made based on the posterior probability, with
observations predicted to be in the class for which they have the
highest probability. The linear discriminant function is: y =
0.3725*(capital\_run\_length\_total)+0.1297*(capital\_run\_length\_longest)+0.0527*(Capital\_run\_length\_average)-0.2104*(word\_freq\_george)+0.2466*(word\_freq\_you)+0.5715*(word\_freq\_your)-0.2354*(word\_freq\_hp)+0.3875*(word\_freq\_free)-0.1506*(word\_freq\_hpl)+0.3268*(char\_freq\_\!)
Using this linear discriminant function, a linear discriminant is
generated for the observation in question based on the factors for that
observation. Using this function (above), one can set the cut-off limit
to dictate when an observation falls within a class (spam vs non-spam in
this case)

# Question 6

## How many linear discriminants are in the model? Why?

THere are 1 linear discriminant in the model. Because LDA is a dimension
reduction model, and since there are 2 possible outcomes (spam/not spam)
the model reduces it to a single dimension Number of linear
discriminants = n - 1; n = number of classes of target
variable.

# Question 7

## Generate LDA plot using the training and validation data. What information is presented in these plots? How are they different?

``` r
train_lda = lda(spam ~ ., data = normtrain.df)
plot(train_lda)
```

![](README_figs/README-Question%207-1.png)<!-- -->

``` r
test_lda = lda(spam ~ ., data = normvalid.df)
plot(test_lda)
```

![](README_figs/README-Question%207-2.png)<!-- --> Answer 7: In the
train LDA, the overlapping between spam and non spam is less thus
classification of classes is better as compared to that in the test LDA.
Also if the LDA score is greater than 0 then records are classified as
spam. Thus we can see that most of the records are classified as
spam.

# Question 8

## Generate the relevant confusion matrix. What are the sensitivity and specificity?

``` r
# Predict propensities
lda.pred <- predict(LDA, normvalid.df[, -11], type = "response")

confusionMatrix(table(lda.pred$class, normvalid.df$spam))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           
    ##            non-spam spam
    ##   non-spam      502  118
    ##   spam           55  244
    ##                                           
    ##                Accuracy : 0.8118          
    ##                  95% CI : (0.7849, 0.8365)
    ##     No Information Rate : 0.6061          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.5934          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.432e-06       
    ##                                           
    ##             Sensitivity : 0.9013          
    ##             Specificity : 0.6740          
    ##          Pos Pred Value : 0.8097          
    ##          Neg Pred Value : 0.8161          
    ##              Prevalence : 0.6061          
    ##          Detection Rate : 0.5462          
    ##    Detection Prevalence : 0.6746          
    ##       Balanced Accuracy : 0.7876          
    ##                                           
    ##        'Positive' Class : non-spam        
    ## 

Answer 8: Sensitivity : 0.901  
Specificity :
0.674.

# Question 9

## Generate lift and decile charts for the validation dataset and evaluate the effectiveness of the model in identifying spams.

``` r
### generating a cumulative lift chart
gain <- gains(as.numeric(normvalid.df$spam), lda.pred$x[,1], groups = 10)
str(lda.pred$posterior)
```

    ##  num [1:919, 1:2] 0.87418 0.00767 0.35947 0.81967 0.65966 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:919] "8" "15" "20" "21" ...
    ##   ..$ : chr [1:2] "non-spam" "spam"

``` r
options(scipen=999)

### Computing gains relative to price
spam<- as.numeric(normvalid.df$spam)
plot(c(0,gain$cume.pct.of.total*sum(spam))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="Lift Chart",
     col = "red", type="l")
### inserting a baseline to the lift chart
lines(c(0,sum(spam))~c(0, dim(finalpredictors.valid)[1]), lty = 5)
```

![](README_figs/README-Question%209-1.png)<!-- -->

``` r
### Decile-wise chart lift chart
heights <- gain$mean.resp/mean(spam)
midpoints <- barplot(heights, names.arg = gain$depth,  ylim = c(0,1.5), col = "pink",  
                     xlab = "Percentile", ylab = "Mean Response",
                     main = "Decile-wise lift chart")
```

![](README_figs/README-Question%209-2.png)<!-- --> Answer 9: As Lift is
a measure of the effectiveness of a predictive model. In the above lift
chart the further away the curve from the diagonal base line the better
this model is doing in seperating records with high value outcomes from
those with low value outcomes. Thus it can be concluded that the model
predictive performance in terms of lift is better than the base model,
since its lift curve is higher than the base model. We can see that
former decile is higher than the later thus the decile wise lift chart
is rightly skewed.  
Thus,this model is effective in detecting the
spams.

# Question 10

## Does accuracy of model changes if you use a probability threshold of 0.2. Explain your answer

``` r
probsTest <- predict(LDA, normvalid.df)

pred<- factor( ifelse(probsTest$posterior[,2] >= 0.2, "spam", "non-spam") )

tab<-table(pred, normvalid.df$spam)
confusionMatrix(tab)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           
    ## pred       non-spam spam
    ##   non-spam      356   34
    ##   spam          201  328
    ##                                                
    ##                Accuracy : 0.7443               
    ##                  95% CI : (0.7148, 0.7722)     
    ##     No Information Rate : 0.6061               
    ##     P-Value [Acc > NIR] : < 0.00000000000000022
    ##                                                
    ##                   Kappa : 0.5045               
    ##                                                
    ##  Mcnemar's Test P-Value : < 0.00000000000000022
    ##                                                
    ##             Sensitivity : 0.6391               
    ##             Specificity : 0.9061               
    ##          Pos Pred Value : 0.9128               
    ##          Neg Pred Value : 0.6200               
    ##              Prevalence : 0.6061               
    ##          Detection Rate : 0.3874               
    ##    Detection Prevalence : 0.4244               
    ##       Balanced Accuracy : 0.7726               
    ##                                                
    ##        'Positive' Class : non-spam             
    ## 

Answer 10: We can see that as we change the threshold to 0.2 from the
default threshold of 0.5, the accuracy drops down to 0.744. Change in
accuracy by changing the threshold depends on the dataset.
