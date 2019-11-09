Homework 5
================
Bingkun Luo
11/2/2019

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

\#\#\#Problem 1

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
kable(head(iris_with_missing),format = "markdown")
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
| -----------: | ----------: | -----------: | ----------: | :------ |
|          5.1 |         3.5 |          1.4 |         0.2 | setosa  |
|          4.9 |         3.0 |          1.4 |         0.2 | setosa  |
|          4.7 |         3.2 |          1.3 |         0.2 | setosa  |
|          4.6 |         3.1 |          1.5 |          NA | setosa  |
|          5.0 |         3.6 |          1.4 |         0.2 | setosa  |
|          5.4 |         3.9 |          1.7 |         0.4 | setosa  |

``` r
replace = function(x){
        if (class(x) == "numeric") {replace_na(x, mean(x, na.rm = TRUE))}       
        else if (class(x) == "character") {replace_na(x, "virginica")} 
                }

##apply using map

final_iris = map_df(iris_with_missing, replace)

kable(head(final_iris),format = "markdown")
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
| -----------: | ----------: | -----------: | ----------: | :------ |
|          5.1 |         3.5 |          1.4 |    0.200000 | setosa  |
|          4.9 |         3.0 |          1.4 |    0.200000 | setosa  |
|          4.7 |         3.2 |          1.3 |    0.200000 | setosa  |
|          4.6 |         3.1 |          1.5 |    1.192308 | setosa  |
|          5.0 |         3.6 |          1.4 |    0.200000 | setosa  |
|          5.4 |         3.9 |          1.7 |    0.400000 | setosa  |

\#\#\#Problem 2

``` r
input_files = list.files(path = './data/data', full.names = TRUE)

#input_data = purrr::map(input_files,.)
```
