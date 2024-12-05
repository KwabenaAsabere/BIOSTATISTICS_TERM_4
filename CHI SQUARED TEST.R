library(patchwork)
library(tidyverse)

iris <- iris
 
flowers <- iris |> 
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small","Medium","Large"))) |> 
  select(Species,size)

## breaks = 3 divides into 3 equal intervals

table(flowers)


# chi goodness of fit test ------------------------------------------------

## are these 3 proportions equal?
## is there a statistically significant difference in the proportion of flowers that are
## small,medium and large(alpha = 0.05)
## H0 : the proportion of flowers that are small,medium and large are equal
## Ha : the proportion of flowers that are small,medium and large are not equal


table(flowers$size)

flowers |> 
  select(size) |> 
  table() |> 
  chisq.test()

## p-value <<< 0.05 therefore we reject the null hypothesis that the proportions are equal



# chi squared test of independence ----------------------------------------

## Ho: the variables,species and size are independent
## i.e knowing the value of one variable doesn't help to predict the value of the other


flowers |> 
  table() |> 
  chisq.test()

### p_value <<<< 0.05 ,hence we reject the null hypothesis that the variables are independent


# Fisher's exact test -----------------------------------------------------
## fisher's exact test if >20% of expected values are <5 
## or all are if any values of <5 in a 2x2 table

my_test <- flowers |> 
  table() |> 
  chisq.test()

attributes(my_test)

my_test$expected ## is equivalent to the code below


flowers |> 
  table() |> 
  chisq.test() |> 
  _$expected

## the native pipe uses the underscore( _ ) to hold place for the name of the dataset

## all the values are above 5 so we don't have to use Fisher's exact test



