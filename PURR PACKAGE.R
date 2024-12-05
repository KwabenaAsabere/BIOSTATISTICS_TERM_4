library(tidyverse)
map(1:5,sqrt)
map_dbl(1:5,sqrt)


silly_list <- list("foo",
                   1:5,
                   faithful,
                   TRUE)
class(silly_list)
map_chr(silly_list,class)

faithful_character <- faithful %>% 
  map_df(as.character)
glimpse(faithful_character)
str(faithful_character)

min_to_sec <- function(x){
  x*60
}
faithful_sec <- faithful %>% 
  map_df(min_to_sec)
glimpse(faithful_sec)

## equivalent
faithful_sec <- faithful %>% 
  map_df(function(x)x*60)

## shortcut for function is the slash \
faithful_sec <- faithful %>% 
  map_df(\(x)x*60)

## aadditional arguments
means <- c(1,10,20,50)


map(means,rnorm,n=5)

map(means,\(m)rnorm(5,m))

## multiple iterations
sds <- c(0.1,1,3,5)
#map2() iterates over two different vectors

map2(means,
     sds,
\(m,s)rnorm(5,m,s))

## map iterated over the factors in a pairwise fashion
## if u want all combinations,use expand_grid

expand_grid(means,sds)

## iterate over 3 or more vectors
## use pmap() with input given as a list


nums <- 1:4

pmap(list(nums,means,sds),rnorm)








