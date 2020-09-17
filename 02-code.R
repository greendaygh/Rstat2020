library(UsingR)
exec.pay
?exec.pay
whale <- c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)
hip_cost <- c(10500, 45000, 74100, NA, 83500)

head(precip)
class(precip)
length(precip)
names(precip)
o <- order(names(precip))
names(precip)[o]

test_scores <- c(100, 90, 80)
names(test_scores) <- c("Alice", "Bob", "Shirley")


head(precip)
precip[1]
precip[2:10]
precip[c(1,3,5)]
precip[-1]
precip["Seattle Tacoma"]
precip[c("Seattle Tacoma", "Portland")]
precip[2] <- 10


1:5
seq(1,5, by=1)
seq(0, 100, by=10)
seq(0, 100, length.out=10)
?seq
rep(5, times=10)
rep(1:3, times=4)
?rep
rep(1:3, each=4)

paste("X", 1:10)
paste("X", 1:10, sep="")
paste("The", "quick", "brown", "fox")
paste(c("The", "quick", "brown", "fox"))
paste(c("The", "quick", "brown", "fox"), collapse=" ")
x <- 1:10
paste(x)
paste(x, collapse=":")

x <- c("Red", "Blue", "Yellow", "Green", "Blue", "Green")
y <- factor(x)
y
levels(y)
y[1] <- "Gold"
y
levels(y) <- c(levels(y), "Gold")


is.na(1)
is.numeric(1)
is.logical(TRUE)

pi < 3
precip < 30
i <- which(precip < 30)
precip[i]
any(precip < 30)
all(precip < 30)
any(39 == precip)
which(39 == precip)
sum(precip < 30)
sum(c(TRUE, TRUE))

x <- 1:100
x < 10
x > 90
x < 10 | x >90
which(x < 10 | x >90)
i <- which(x < 10 | x >90)
x[i]
x[x < 10 | x >90]

library(lubridate)
current_time <- now() # record since 1970
as.numeric(current_time)
as.numeric(now())
month(current_time)


get_dist <- function(x){
  z <- abs(x - mean(x))
  return(z)
}

mean(rivers)
get_dist(rivers)

abs(rivers - mean(rivers))


LETTERS
x <- sample(LETTERS, 10)
x_paste <- paste(x, collapse="")

random_string_generator <- function(n){
  x <- sample(LETTERS, n)
  x_paste <- paste(x, collapse="")
  return(x_paste)
}

random_string_generator(20)
random_strings <- replicate(10, random_string_generator(5))

grep("D", random_strings)
i <- grep("^X", random_strings)
random_strings[i]
i <- grep("J$", random_strings)
random_strings[i]

x <- c(10,20,30,40)
names(x) <- replicate(4, random_string_generator(5))
x[grep("^S", names(x))]


x <- 0:5
length(x)
quantile(x, 0.25)
median(x)
quantile(x, seq(0, 1, by=0.2))
quantile(x)

wts

mean(wts)
median(wts)
plot(wts, ylim=c(0,60))
abline(h=mean(wts), col="red")
abline(h=median(wts), col="blue")
wts2 <- wts[wts<120]
abline(h=mean(wts2), col="red", lty=2)
abline(h=median(wts2), col="blue", lty=2)


x <- 0:5
quantile(x)
boxplot(x)
text(x=1.3, y=quantile(x, 0.25), labels = "1사분위수")
text(x=1.3, y=quantile(x, 0.5), labels = "2사분위수")
text(x=1.3, y=quantile(x, 0.75), labels = "3사분위수")

boxplot(wts)

hist(wts, breaks = 50)
hist(wts, 50)
abline(v=mean(wts), col="red")
sd(wts)

z <- (wts - mean(wts))/sd(wts)
hist(z, 50)



