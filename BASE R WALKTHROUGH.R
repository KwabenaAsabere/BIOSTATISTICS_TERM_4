# Create a list
my_list <- list(a = c(1, 2, 3), b = c(4, 5, 6))

# Use sapply to calculate the mean of each element in the list
sapply(my_list, mean)


# Create a list
my_list <- list(a = c(1, 2, 3), b = c(4, 5, 6))

# Use sapply to calculate the square of each element in the list
sapply(my_list, function(x) x^2)


# Create a dataframe
df <- data.frame(values = c(1, 2, 3, 4), groups = c("A", "A", "B", "B"))

# Use tapply to calculate the mean of values by group
tapply(df$values, df$groups, mean)


x <- c("one","two","three","four","five")
x[c(3,2,5)]

x[c(-3,-2,-5)]

x <- c(10,3,NA,5,8,1,NA)
x[!is.na(x)]

x[x %% 2 ==0]

df <- tibble(
  x = 1:3,
  y = c("a","e","f"),
  z = runif(3)
)
df
df[1,2]
df[,c("x","y")]



df[df$x>1,]
df[which(df$x >1),]

tb <- tibble(
  x = 1:4,
  y = c(10,4,1,21)
)
tb
tb[[1]]
tb[["x"]]
tb$x
tb$z <- tb$x + tb$y
tb

l <- list(
  a = 1:3,
  b =" a string",
  c = pi,
  d = list(-1,-5)
)
l
l[1:2]
str(l[1:2])

l[[1]]
str(l[[1]])


library(dplyr)

# Example dataframe
df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9)
)

# Using summarize() with across() and .id
summary_df <- df %>%
  summarize(
    across(everything(), mean, .names = "mean_{.col}"),
    .id = "variable"
  )

print(summary_df)
summary_df

library(dplyr)

# Example dataframe
df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9)
)

# Using summarize() with across() and .id
summary_df <- df %>%
  summarize(
    across(everything(), mean, .names = "mean_{.col}")
    
  ) %>% t()



()

print(summary_df)





























