# Exercise 1
3 + 2 #5
5 * 3 #15
15 - 6 #9
9 / 3 #3

# Objects
a <- 1
#1
a + 2
#3
1:2
#1 2
1:4
die <- 1:6

# Functions
round(3.1415)
#3
factorial(3)
#6
mean(1:6)
#3.5
mean(die)
#3.5
round(3.1415, digits = 2)
#3.14
die <- 1:6
dice <- sample(die, size = 2, replace = TRUE)
sum(dice)
my_function <- function() {}
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
roll()

