boxplot(kid.weights)
boxplot(kid.weights$weight)

#install.packages("vioplot")
library(vioplot)
vioplot(kid.weights)
vioplot(kid.weights, col=c("#3CAEA3", "#F6D55C", "#ED553B"), rectCol=c("gray"), main="Kids")
?vioplot

#console
par(mfrow=c(1,2))
plot(density(kid.weights$weight))
vioplot(kid.weights$weight)


x <- rep(Macdonell$finger, Macdonell$frequency)
qqnorm(x)
hist(x, breaks=50)

hist(Galton$child, breaks=50)
x <- jitter(Galton$child, factor=5)
hist(x, breaks=50)
qqnorm(x)

y <- rnorm(10000, mean(x), sd(x))
hist(y, br=50)
qqnorm(y)

## overlay
x_lim <- range(c(density(x)$x, density(y+10)$x))
plot(density(x), xlim=x_lim)
lines(density(y+10), col="red")

x <- babies$smoke
x <- factor(x, labels=c("never", "now", "until current", "once, quit", "unknown"))
table(x)
out <- table(x)
prop <- 100*(out/sum(out))
round(prop, digits=2)
barplot(out)
barplot(prop)
dotplot(out)
dotplot(prop)
pie(out)


library(UsingR)
head(michelson)
?michelson
str(michelson)
speed <- michelson$Speed 
expt <- michelson$Expt
fourth <- speed[expt == 4]
fifth <- speed[expt == 5]
d4 <- density(fourth)
d5 <- density(fifth)
xrange <- range(c(d4$x, d5$x))
yrange <- range(c(d4$y, d5$y))
plot(d4, xlim=xrange, ylim=yrange, main="")
lines(d5, lty=2)
legend(650, 0.008, legend=c("Fourth", "Fifth"), lty=c(1,2))


plot(fourth, fifth)



qqplot(fourth, fifth)
ps <- seq(0.05, 0.95, by=0.05)
x <- quantile(fourth, ps)
y <- quantile(fifth, ps)
plot(x, y)

o <- order(fourth)
fourth[o]
fourth



b <- list(beets = beets, "no beets"=no_beets)
b$beets
b[1]
b[[1]]
class(b[1])
class(b[[1]])
boxplot(b)


id <- 1:10
name <- paste("Name", id, sep="")
grade <- LETTERS[sample(1:5, size=length(id), replace=T)]
student <- data.frame(id, name, grade)
student
str(student)

student$id
student[,1]
class(student$name)

class(student)
class(student[,1])
class(student$id)
student <- data.frame(id, name, grade, stringsAsFactors = F)
str(student)
?data.frame


beets
no_beets
runtime <- c(beets, no_beets)
nitrate <- c(rep("beets", length(beets)), rep("nobeets", length(no_beets)))
food.sports <- data.frame(runtime, nitrate)
boxplot(runtime~nitrate, data=food.sports)

head(michelson)
boxplot(michelson$Speed ~ michelson$Expt)
boxplot(Speed ~ Expt, data=michelson)

plot(Speed ~ Expt, data=michelson)
out <- summary(Speed ~ Expt, data=michelson)
plot(out)
plot(michelson$Speed)
plot(michelson$Speed, main="Speed", ylab="Speed", bty="l", pch="*", cex=2, col="red")
?pch


b <- list("beets" = beets, "no beets" = no_beets)
stacked <- stack(b)
plot(values ~ ind, data=stacked)

?split
speeds <- split(michelson$Speed, michelson$Expt)
names(speeds) <- paste("Expt", 1:5, sep="")
speeds



library(UsingR)
class(fat)
str(fat)
head(fat)
names(fat)

neck_pair <- fat$neck
wrist_pair <- fat$wrist
mean(neck_pair/wrist_pair)
mean(neck_pair)/mean(wrist_pair)
plot(neck_pair, wrist_pair)

neck_nopair <-sample(fat$neck)
wrist_nopair <- sample(fat$wrist)
mean(neck_nopair)/mean(wrist_nopair)
mean(neck_nopair/wrist_nopair)
plot(neck_nopair, wrist_nopair)


x <- fat$wrist
y <- fat$neck
plot(x, y)
abline(v = mean(x), lty=2)
abline(h = mean(y), lty=2)
points(mean(x), mean(y), pch=16, cex=4, col="#00000055")
abline(lm(y~x))

x <- Animals$body
y <- Animals$brain
cor(x, y)
plot(x, y, xlim=c(0,5000))
plot(log(x), log(y))
Animals
cor(rank(x), rank(y))
cor(x, y, method="spearman") 

ToothGrowth

plot(ToothGrowth$dose, ToothGrowth$len)
cor(ToothGrowth$dose, ToothGrowth$len)
l <- split(ToothGrowth$len, ToothGrowth$dose)
group_means <- c(mean(l[[1]]), mean(l[[2]]), mean(l[[3]]))

points(c(0.5, 1, 2), group_means, col="red", pch=17, cex=2)
cor(c(0.5, 1, 2), group_means)


cor(SAT$salary, SAT$total)
plot(salary~total, data=SAT, cex=2)
points(salary~total, SAT, subset = perc < 10,  col="red", pch=15, cex=2)
points(salary~total, SAT, subset = perc > 40, col="blue", pch=16, cex=2)
abline(lm(SAT$salary~SAT$total))
abline(lm(salary~total, SAT, subset = perc < 10),  col="red")
abline(lm(salary~total, SAT, subset = perc > 40),  col="blue")


buyback <- c(ACT=1500, NSW=2500, WA=2700, Qld=3700, Vic=4250, SA=4200, NT=5000, Tas=7500)
change <- -c(0.2, 1, 0.9, 2.5, 1.0, 1.6, 2.5, 3.4)
m <- data.frame(buyback, change)
cor(buyback, change)

ToothGrowth
str(ToothGrowth)
l <- split(ToothGrowth$len, ToothGrowth$dose)
group_means <- lapply(l, mean)
plot(len ~ dose, data=ToothGrowth, pch=16, col=rgb(0, 0, 0, 0.25), cex=1.2)
points(c(0.5, 1, 2), group_means, cex=2, pch=18, col="red")
lines(c(0.5, 1, 2), group_means, col="red")



library(UsingR)
?lm
out <- lm(maxrate ~ age, data=heartrate)
out

## visualize fitted line
names(out)
summary(out)
plot(maxrate ~ age, data=heartrate)
abline(out)

## residuals
sum(resid(out))
sum(out$residuals)
res <- heartrate$maxrate - out$fitted.values
sum(res)

## fitted values
age <- c(30, 40)
out$coefficients[1] + out$coefficients[2]*age
predict(out, data.frame(age))
head(heartrate)
predict(out, data.frame(age=30))

f <- child ~ parent
plot(f, data=Galton, col=rgb(0,0,0, alpha=0.25))
res.lm <- lm(f, data=Galton)
abline(res.lm, lty=1, lwd=2)

## loess
res.loess <- loess(f, data=Galton, degree=1)
x <- seq(64, 73, length=20)
newdata <- data.frame(parent=x)
predicted <- predict(res.loess, newdata)
lines(predicted~x, lty=2, lwd=2)



f <- child ~ parent
plot(f, data=Galton, col=rgb(0,0,0, alpha=0.25))
res.lm <- lm(f, data=Galton)
abline(res.lm, lty=1, lwd=2)

## loess
res.loess <- loess(f, data=Galton, degree=1)
x <- seq(64, 73, length=20)
newdata <- data.frame(parent=x)
predicted <- predict(res.loess, newdata)
lines(predicted~x, lty=2, lwd=2)


parent2 <- jitter(Galton$parent, factor = 2)
child2 <- jitter(Galton$child, factor=2)+(parent2-mean(parent2))^2
dat <- data.frame(parent2, child2)
f <- child2 ~ parent2
plot(f, data=dat, col=rgb(0,0,0, alpha=0.25))
res.lm <- lm(f, data=dat)
abline(res.lm, lty=1, lwd=2)

## loess
res.loess <- loess(f, data=dat, degree=1)
x <- seq(64, 73, length=20)
newdata <- data.frame(parent2=x)
predicted <- predict(res.loess, newdata)
lines(predicted~x, lty=2, lwd=2, col="red")

head(grades)
str(grades)
mytbl <- table(grades$prev, grades$grade)




mytbl
rowSums(mytbl)
margin.table(mytbl, margin=1)
hist(x, br=10)

colSums(mytbl)
margin.table(mytbl, margin=2)
hist(x, br=10)


prop.table(seatbelts)
seatbelts/sum(seatbelts)

prop.table(seatbelts, margin=1)
prop.table(seatbelts, margin=2)


head(Fingerprints)
tail(Fingerprints)

## without xtabs
idx <- !is.na(Fingerprints$count)
Fingerprints[idx,]
whorls <- rep(Fingerprints$Whorls[idx], Fingerprints$count[idx])
loops <- rep(Fingerprints$Loops[idx], Fingerprints$count[idx])
table(whorls, loops)

## with xtabs
xtabs(count ~ Whorls + Loops, Fingerprints)
xtabs(count ~ ., Fingerprints)


str(Cars93)
xtabs( ~ Origin + Type, Cars93)

barplot(seatbelts)
barplot(seatbelts, beside=T)
barplot(seatbelts, beside=T, legend=rownames(seatbelts))
barplot(seatbelts, beside=T, legend=rownames(seatbelts), col=c("red", "blue"))

titanic <- as.data.frame(Titanic)
head(titanic)
tbl <- xtabs(Freq ~ Survived + Class, data=titanic, subset=Sex=="Female")
mosaicplot(tbl)
tbl <- xtabs(Freq ~ Survived + Class, data=titanic)
mosaicplot(tbl)

xtabs(Freq ~ Sex, titanic)
tbl <- xtabs(Freq ~ Sex + Survived, titanic)
mosaicplot(tbl)
tbl <- xtabs(Freq ~ Sex + Survived + Class, titanic)
mosaicplot(tbl)


seatbelts <- matrix(c(56, 2, 8, 16), nrow=2)
rownames(seatbelts) <- c("pa_buckled","pa_unbuckled")
colnames(seatbelts) <- c("ch_buckled","ch_unbuckled")
seatbelts

fo <- seatbelts
fo
## marginal probability
margin_rows <- rowSums(fo)/sum(fo)
margin_cols <- colSums(fo)/sum(fo)

fe <- matrix(0, 2, 2,) 
rownames(fe) <- rownames(fo)
colnames(fe) <- rownames(fo)
## expected numbers
fe[1,1] <- sum(fo)*margin_rows[1]*margin_cols[1]
fe[1,2] <- sum(fo)*margin_rows[1]*margin_cols[2]
fe[2,1] <- sum(fo)*margin_rows[2]*margin_cols[1]
fe[2,2] <- sum(fo)*margin_rows[2]*margin_cols[2]

sum((fo-fe)^2 / fe)

## use chisq.test function
chisq.test(fo)







