# Basil chattha
# Chapter 8

play()

play()

one_play <- play()

one_play

num <- 1000000000
print(num)

class(num) <- c("POSIXct", "POSIXt")
print(num)

attributes(DECK)

row.names(DECK)

row.names(DECK) <- 101:152

levels(DECK) <- c("level 1", "level 2", "level 3")

attributes(DECK)

one_play <- play()
one_play

attributes(one_play)

attr(one_play, "symbols") <- c("B", "0", "B")

attributes(one_play)

attr(one_play, "symbols")

one_play

one_play + 1

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

play()

two_play <- play()

two_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
three_play <- play()
three_play

slot_display <- function(prize){
  # extract symbols
  symbols <- attr(prize, "symbols")
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  # display regular expression in console without quotes
  cat(string)
}

slot_display(one_play)

symbols <- attr(one_play, "symbols")

symbols

symbols <- paste(symbols, collapse = " ")

symbols

prize <- one_play
string <- paste(symbols, prize, sep = "\n$")

string

cat(string)

slot_display(play())

slot_display(play())

print(pi)

pi

print(head(deck))

head(deck)

print(play())

play()

num <- 1000000000
print(num)

class(num) <- c("POSIXct", "POSIXt")
print(num)

print

print.POSIXct

methods(print)

class(one_play) <- "slots"

args(print)

print.slots <- function(x, ...) {
  cat("I'm using the print.slots method")
}

print(one_play)

one_play

now <- Sys.time()
attributes(now)

print.slots <- function(x, ...) {
  slot_display(x)
}

one_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

play()

play()

methods(class = "factor")

play1 <- play()
play1

play2 <- play()
play2

c(play1, play2)

play1[1]

# Chapter 9

die <- c(1, 2, 3, 4, 5, 6)

rolls <- expand.grid(die, die)

rolls

rolls$value <- rolls$Var1 + rolls$Var2
head(rolls, 3)

prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)

prob

rolls$Var1

prob[rolls$Var1]

rolls$prob1 <- prob[rolls$Var1]
head(rolls, 3)

rolls$prob2 <- prob[rolls$Var2]

head(rolls, 3)

rolls$prob <- rolls$prob1 * rolls$prob2

head(rolls, 3)

sum(rolls$value * rolls$prob)

combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

combos

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
}

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

head(combos, 3)

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

head(combos, 3)

sum(combos$prob)

symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])

score(symbols)

for (value in that) {
  this
}

for (value in c("My", "first", "for", "loop")) {
  print("one run")
}

for (value in c("My", "second", "for", "loop")) {
  print(value)
}

value

for (word in c("My", "second", "for", "loop")) {
  print(word)
}
for (string in c("My", "second", "for", "loop")) {
  print(string)
}
for (i in c("My", "second", "for", "loop")) {
  print(i)
}

for (value in c("My", "third", "for", "loop")) {
  value
}

chars <- vector(length = 4)
The next loop will fill it with strings:
  words <- c("My", "fourth", "for", "loop")
for (i in 1:4) {
  chars[i] <- words[i]
}
chars

combos$prize <- NA

head(combos, 3)

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

head(combos, 3)

sum(combos$prize * combos$prob)

score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  # identify case
  # since diamonds are wild, only nondiamonds
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  # double for each diamond
  prize * 2^diamonds
}

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)

while (condition) {
  code
}

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}
plays_till_broke(100)

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}
plays_till_broke(100)