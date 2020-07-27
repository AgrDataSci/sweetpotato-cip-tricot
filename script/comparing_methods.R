
m <- c(1, 494, 490, 512, 525,
       2, 395, 397, 430, 415,
       3, 516, 512, 520, 508,
       4, 434, 401, 428, 444,
       5, 476, 470, 500, 500,
       6, 557, 611, 600, 625,
       7, 413, 415, 364, 460,
       8, 442, 431, 380, 390,
       9, 650, 638, 658, 642,
       10, 433, 429, 445, 432,
       11, 417, 420, 432, 420,
       12, 656, 633, 626, 605,
       13, 267, 275, 260, 227,
       14, 478, 492, 477, 467,
       15, 178, 165, 259, 268,
       16, 423, 372, 350, 370,
       17, 427, 421, 451, 443)

m <- matrix(m, nrow = 17, ncol = 5, byrow = TRUE)

m

# dt <- data.frame(base = c(m[,2], m[,3]),
#                  new  = c(m[,4], m[,5]))

dt <- data.frame(base = (m[,2] + m[,3]) / 2,
                 new =  (m[,4] + m[,5]) / 2)

r <- lm(base ~ new, data = dt)

coef <- coefficients(r)

plot(dt, xlim = c(0,800), ylim = c(0,800))
abline(r, col = "red")

cor(dt$base, dt$new)

diff <- dt$base - dt$new
aver <- rowMeans(dt[,c("base", "new")])

plot(aver, diff, xlim = c(0, 800), ylim = c(-100, 100))
abline(h = mean(diff))
abline(h = mean(diff) - 2 * sd(diff), lty = 3)
abline(h = mean(diff) + 2 * sd(diff), lty = 3)

# the sample size 
n <- dim(dt)[[1]]

# the mean of the difference
d <- mean(diff)

# the standard deviation of the difference
s <- sd(diff)

# limits of agreement
llim <- d - (2 * s)
ulim <- d + (2 * s)

# value t from a t table 
tval <- 2.12

# the standard error of d
se <- sqrt(s ^ 2 / n)

# the 95% confidence interval for the bias
d - (tval * se)
d + (tval * se)

# the standard error for the limits
sel <- sqrt((3 * (s ^ 2)) / n)

# confidence interval for the lower limit
llim - (tval * sel)
llim + (tval * sel)

# the confidence interval for the upper limit
ulim - (tval * sel)
llim + (tval * sel)
