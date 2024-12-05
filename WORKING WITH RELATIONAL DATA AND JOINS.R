library(tidyverse)
library(Lahman)
People <- as_tibble(Lahman::People)

Teams <- as_tibble(Lahman::Teams)
fielding <- as_tibble(Lahman::Fielding)
pitching <- as_tibble(Lahman::Pitching)
batting <- as_tibble(Lahman::Batting)
people <- People

## Primary key is respective to each table and uniquely 
# identifies observation in each table

head(people)

people %>% 
  count(playerID)  %>% 
  filter(n>1)

## People$PlayerID is the Foreign key from from the perspective of batting

head(batting)
## in batting PrimaryID is not a primary key

batting %>% 
  count(playerID) %>% 
  filter(n>1)

head(Teams)

# inner_join


x <- tribble(
  ~key,~value_x,
  1,"x1",
  2,'x2',
  3,"x3"
  )

y <- tribble(
  ~key,~val_y,
  1,"y1",
  2,"y2",
  4,"y4"
)

x
y

##inner join

## start with one data frame

x %>% 
  inner_join(y, by = "key")

## inner join only keeps observations that appear in both datatframes

##selecting all batting stats for players who were born in the 1980s
nrow(batting)


People %>% 
  filter(between(birthYear,1980,1989)) %>% 
  inner_join(batting, by = "playerID")

##left join
x %>% 
  left_join(y, by = "key")


## right join

x %>% 
  right_join(y, by = "key")


## full join
x %>% 
  full_join(y, by = "key")


Teams %>% 
  select(teamID,name,yearID) %>% 
  distinct() %>% 
  right_join(Batting, by = c("teamID","yearID"))

Teams %>% 
  select(teamID,name) %>% 
  distinct() %>% 
  add_count(teamID) %>% 
  arrange(-n)



Teams %>% 
  select(teamID,name) %>% 
  distinct() %>% 
  count(teamID) %>% 
  arrange(-n)


## list the first name,last name  and team name for every player who played in 2018

Teams
people
batting

Batting %>% 
  select(yearID,playerID,teamID) %>% 
  filter(yearID == 2018) %>% 
  left_join(Teams, by = c("yearID","teamID")) %>% 
  left_join(people, by = "playerID") %>% 
  select(nameFirst,nameLast,name) %>% 
  distinct()

Batting %>% 
  select(yearID,playerID,teamID) %>% 
  filter(yearID == 2018) %>% 
  inner_join(Teams, by = c("yearID","teamID")) %>% 
  inner_join(people, by = "playerID") %>% 
  select(nameFirst,nameLast,name) %>% 
  distinct()

## semi_join

## called filtering joins
##you are using the joins to filter your rows.U are actually not adding anything new
## it will look at the rows in the other table and filter the other table
## based on the rows in the first table

#semi_join
x %>% 
  semi_join(y , by = "key")

## anti_join() = what in x but not in y
x %>% 
  anti_join(y, by = "key")



##Find the 10 players with the highest number of strikeouts(SO) from the Batting table
## and assign to a new data frame
## Join the appropriate data frames to select all players from Peeople for
## those 10 players

ten_worst <- Batting %>% 
  group_by(playerID) %>% 
  summarise(count_strikeout = sum(SO,na.rm = TRUE)) %>% 
  slice_max(count_strikeout, n = 10)

People %>% 
  semi_join(ten_worst, by = "playerID")


ten_worst_vec <- Batting %>% 
  group_by(playerID) %>% 
  summarise(count_strikeout = sum(SO,na.rm = TRUE)) %>% 
  slice_max(count_strikeout, n = 10) %>% 
  pull(playerID)


people  %>% 
filter(playerID %in% ten_worst_vec)

## when your key names don't match................

library(nycflights13)
flights %>% 
  left_join(airports, by = c("origin" = "faa"))


### set operations
x <- tribble(
  ~key,~val,
  1,"a",
  1,"b",
  2,"a"
)

y <- tribble(
  ~key,~val,
  1,"a",
  2,"b"
)


## Union
## All unique rows from x and y

union(x,y)

## intersect
## common rows in both x & y, keeping just unique rows
intersect(x, y)

## setdiff

## All rows from x which are not also rows in y , keeping just unique rows

setdiff(x, y)

setdiff(y, x)


## appending data frames
 
## bind_rows































































