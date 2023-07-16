Yahtzee
================
Valerie Morrill
1/16/2022

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

Rules of the Game
-----------------

How many dice are there? How many sides of the dice are there? How many games will we simulate?

``` r
dice<-5
sides<-6
simulations<-10000
```

Make the score card for the simulations
---------------------------------------

``` r
score <- rep(NA,simulations)
```

Run the simulations
-------------------

``` r
for (i in 1:simulations){
  
        ##First roll all the dice
        roll <- sample(1:sides, dice, replace = T)
        keep <- names(sort(table(roll), decreasing = TRUE))[1] #Which dice value to hold in our hand
        number <- sort(table(roll), decreasing = TRUE)[1] #How many dice to hold in our hand 

        score[i] <-1 #record the number of rolls in the score card
        
                while(number!=dice) { #keep re-rolling until you have all dice of one kind
        
                        for (j in 1:dice){ #only reroll the dice we do not want to hold in our hand
                
                                if (!roll[j] %in% keep){
                                roll[j] <- sample(sides, size=1)
                        }
        
                        }
        
                #each time you reroll, re-evaluate what dice value to keep, and how many to hold in our hand
                keep <- names(sort(table(roll), decreasing = TRUE))[1]
                number <- sort(table(roll), decreasing = TRUE)[1]

                score[i] <- score[i]+1 #record the number of rolls in the score card
                
        
                }
}
```

Edit score card for graphing
----------------------------

``` r
score_recode <- as.data.frame(score) %>% rename("rolls"=score) %>% add_rownames(var = "round")
```

    ## Warning: Deprecated, use tibble::rownames_to_column() instead.

``` r
roll_counts <- as.data.frame(table(score_recode$rolls)) %>% rename("rolls"="Var1") %>% mutate(probability_roll=Freq/10000) 

roll_counts$probability_roll_or_less <- NA

roll_counts$probability_roll_or_less[1] <- 0.0010

for (i in 2:51){
        
     roll_counts$probability_roll_or_less[i] <- (roll_counts$probability_roll[i] + roll_counts$probability_roll_or_less[i-1]) 
     
}
```

Graph the number count of the number of rolls to get a yahtzee
--------------------------------------------------------------

``` r
ggplot(score_recode, aes(x=rolls)) + 
  geom_histogram(color="black", fill="white", bins=31) +
       # xlim(c(0,30)) +
        ylab("Number of games") + 
        xlab("Number of rolls to get a Yahtzee") +
        ggtitle("Simulation of 10,000 Yahtzee-only Yahtzee games")
```

![](Yahtzee_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(roll_counts, aes(x=as.numeric(rolls), y=(probability_roll_or_less*100))) + 
  geom_smooth(color="black", se=FALSE) +
        xlim(c(0,30)) +
        ylab("Probability of getting a Yahtzee within a # of rolls") + 
        xlab("Number of rolls") +
        ggtitle("Probability of getting a Yahtzee within a # of rolls")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 21 rows containing non-finite values (stat_smooth).

![](Yahtzee_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(score_recode, aes(x=rolls)) + 
  geom_histogram(color="black", fill="white", bins=31) +
        xlim(c(0,30)) +
        ylab("Number of games") + 
        xlab("Number of rolls to get a Yahtzee") +
        ggtitle("Simulation of 10,000 balls to the wall Yahtzee games")
```

    ## Warning: Removed 138 rows containing non-finite values (stat_bin).

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Yahtzee_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(roll_counts, aes(x=as.numeric(rolls), y=(probability_roll*100))) + 
  geom_smooth(color="black", se=FALSE) +
        xlim(c(0,30)) +
        ylab("Probability of getting a Yahtzee") + 
        xlab("Number of rolls to get a Yahtzee") +
        ggtitle("Probability of getting a Yahtzee within a # of rolls")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 21 rows containing non-finite values (stat_smooth).

![](Yahtzee_files/figure-markdown_github/unnamed-chunk-8-2.png)
