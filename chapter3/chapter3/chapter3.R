#Chapter 3
#Basil Chattha

die <- c(1, 2, 3, 4, 5, 6)
die

is.vector(die)

five <- 5
five

is.vector(five)

length(five)

length(die)

int <- 1L
text <- "ace"

int <- c(1L, 5L)
text <- c("ace", "hearts")

sum(int)

sum(text)

die <- c(1, 2, 3, 4, 5, 6)
die

typeof(die)

int <- c(-1L, 2L, 4L)
int

typeof(int)

sqrt(2)^2 - 2

text <- c("Hello", "World")
text

typeof(text)

typeof("Hello")

3 > 4

logic <- c(TRUE, FALSE, TRUE)
logic

typeof(logic)

typeof(F)

comp <- c(1 + 1i, 1 + 2i, 1 + 3i)
comp

typeof(comp)

raw(3)

typeof(raw(3))

hand <- c("ace", "king", "queen", "jack", "ten")
hand

typeof(hand)

attributes(die)

names(die)

names(die)<-c("one", "two", "three", "four", "five", "six")

names(die)

attributes(die)

die

die + 1

names(die) <- c("uno", "dos", "tres", "quatro", "cinco", "seis")
die

names(die) <- NULL
die

dim(die) <- c(2,3)
die

dim(die) <- c(3,2)
die

dim(die) <- c(1, 2, 3)
die

m <- matrix(die, nrow = 2)
m

m <- matrix(die, nrow = 2, byrow = TRUE)
m

ar <- array(c(11:14, 21:24, 31:34), dim = c(2,2,3))
ar

hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades","spades","spades","spades")
matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5,2)

hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack",
           "spades", "ten", "spades")
matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)

dim(die) <- c(2,3)
typeof(die)

class(die)

attributes(die)

class("Hello")

class(5)

now <- Sys.time()
now

typeof(now)

class(now)

unclass(now)

mil <- 1000000
mil

class(mil) <- c("POSIXct", "POSIXt")
mil

gender <- factor(c("male", "female", "female", "male"))

typeof(gender)

attributes(gender)

unclass(gender)

gender

as.character(gender)

card <- c("ace", "hearts", 1)
card

sum(c(TRUE, TRUE, FALSE, FALSE))

sum(c(1,1,0,0))

as.character(1)

as.logical(1)

as.numeric(FALSE)

list1 <- list(100:130, "R", list(TRUE, FALSE))
list1

card <- list("ace", "hearts", 1)
card

df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"),
                 value = c(1, 2, 3))
df

typeof(df)

class(df)

str(df)

df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"),
                 value = c(1, 2, 3),
                 stringsAsFactors = FALSE)


head(deck)

write.csv(deck, file = "cards.csv", row.names = FALSE)

getwd()
