Homework 5
================
Bingkun Luo
11/2/2019

\#Problem 1 \#\# a

``` r
str(iris_with_missing)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    150 obs. of  5 variables:
    ##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 NA 5 4.4 4.9 ...
    ##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 NA ...
    ##  $ Petal.Width : num  0.2 0.2 0.2 NA 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##  $ Species     : chr  "setosa" "setosa" "setosa" "setosa" ...

``` r
replace = function(x){
        if (class(x) == "numeric") {replace_na(x, mean(x, na.rm = TRUE))}       
        else if (class(x) == "character") {replace_na(x, "virginica")} 
                }

final_iris = replace(iris_with_missing)
```

    ## Warning in if (class(x) == "numeric") {: the condition has length > 1 and
    ## only the first element will be used

    ## Warning in if (class(x) == "character") {: the condition has length > 1 and
    ## only the first element will be used

``` r
head(final_iris)
```

    ## NULL

``` r
 #is.na(iris_with_missing[,])
a = function(x){for(i in 1:length(x)){
  is.na(x[,i]) = mean(x[,i], na.rm = TRUE)}
   }


#replace_na("virginica")
 #if(is.numeric(x[,i]))
```
