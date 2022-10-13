Myname <- "Yaquan Yang"
library(tidyverse)

table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)
# Compute cases per year
table1 %>%
  count(year, wt = cases)

# Visualize changes over time
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "darkolivegreen4") +
  geom_point(aes(color = country)) # only two years of data

##12.2.1 exercises
## problem2 table2
(table2_cases <- table2 %>%
    filter(type == "cases"))
(table2_population <- table2 %>%
    filter(type == "population"))

(table2_mod <- tibble(
  country = table2_cases$country,
  year = table2_cases$year,
  cases = table2_cases$count,
  population = table2_population$count
))
table1 == table2_mod
(table2_mod <- table2_mod %>%
    mutate(rate = (cases / population) * 10000))
## table4a-4b
table4a
table4b
(table4_mod = tibble(
  country = c(table4a$country, table4b$country),
  year = c(rep(colnames(table4a)[2],length(table4a$`1999`)),
           rep(colnames(table4a)[3],length(table4a$`2000`))),
  cases = c(table4a$`1999`, table4a$`2000`),     #4a has cases
  population = c(table4b$`1999`, table4b$`2000`) #4b has population
))
(table4_mod <- table4_mod %>%
    mutate(rate = (cases / population) * 10000) %>%
    arrange(country))

## problem3
table1
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "darkolivegreen4") +
  geom_point(aes(color = country)) # only two years of data

ggplot(table2_mod, aes(year, cases)) +
  geom_line(aes(group = country), color = "darkolivegreen4") +
  geom_point(aes(color = country)) # only two years of data

table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), color = "darkolivegreen4") +
  geom_point(aes(color = country)) + # only two years of data
  labs(y = "cases")

table4a
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b
table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

table2
spread(table2, key = type, value = count)

# Exercises 12.3.3
## problem 1
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

# The functions pivot_longer() and pivot_wider() are not perfectly symmetrical because column type information is lost. 
# When we use pivot_wider() on a data frame, it discards the original column types. 
# It has to coerce all the variables into a single vector with a single type.
# Later, if we pivot_longer() that data frame, the pivot_longer() function does not know the original data types of the variables.

## problem 2
table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
#The code fails because the column names 1999 and 2000 are not non-syntactic variable names. 



## problem 3
people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
pivot_wider(people, names_from = names, values_from = values)
  # pivot_wider() this data frame fails because the name and key columns do not uniquely identify rows. 
  # In particular, there are two rows with values for the age of “Phillip Woods”.
  # We could solve the problem by adding a row with a distinct observation count for each combination of name and key.
people2 <- people %>%
group_by(name, names) %>%
  mutate(obs = row_number())
people2
  # We can pivot_wider() people2 because the combination of name and obs will uniquely identify the spread rows.
pivot_wider(people2, names_from = names, values_from = values)

## problem 4
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg
# step1:gather(male, female, key = "sex", value = "count")
preg_tidy <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")
preg_tidy

# step2:remove NA
preg_tidy2 <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count", values_drop_na=TRUE)
preg_tidy2