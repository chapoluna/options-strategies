B <- 10000
n <- 80
operation <- 2000
loss_per_operation <- -.4*operation
p_loss <- 8/21

# Smaller samples should use T-distribution
se <- sqrt(p_loss*(1-p_loss)/21)

p_losses <- c(p_loss - qnorm(.975)*se, p_loss + qnorm(.975)*se) 
losses <- replicate(B, { 
  loss <- sample(c(0,1), n, prob=c(1-p_loss,p_loss), replace = TRUE)
  sum(loss*loss_per_operation)
})
#data.frame(losses_in_thousands = losses/10^3) %>% ggplot(aes(losses_in_thousands)) + geom_histogram(binwidth = 1, col = "black")

avg <- n*(p_loss*loss_per_operation + (1-p_loss)*0)
se <- sqrt(n)*abs(loss_per_operation)*sqrt(p_loss*(1-p_loss))

# To cover the losses:  loss_per_operation*p_loss + x*(1 - p_loss) = 0
- loss_per_operation*p_loss/(1-p_loss)
# Let's define the probability of losing money (i.e sum of losses) be less than 10%
z <- qnorm(0.01)
# P(Z <= z) = 0.01
# -{lp + x(1-p)}n / (x-1)sqrt(np(1-p)) = z
l <- loss_per_operation
x <- -l*(n*p_loss - z*sqrt(n*p_loss*(1-p_loss)))/(n*(1-p_loss) + z*sqrt(n*p_loss*(1-p_loss)))
#Expected profit
n*(loss_per_operation*p_loss + x*(1-p_loss))

#Let's chek the results, if we really make money
profit <- replicate(B, {
  new_p_loss <- sample(seq(p_losses[1], p_losses[2], length = 100), 1)
  x <- sample(seq(.3,.6,length=10),1)*operation
  draws <- sample(c(x,loss_per_operation), n, prob=c(1-new_p_loss,new_p_loss), replace = TRUE)
  sum(draws)
})
data.frame(profit) %>% ggplot(aes(profit)) + geom_histogram(col = "black")
mean(profit)
mean(profit<0)
min(profit)
max(profit)

# Let's test for normality. If p > 0.05, then we accept the normality of the data
p = seq(0.05, 0.95, 0.05)
sample_quantiles = quantile(profit, p)
theoretical_quantiles = qnorm(p, mean = mean(profit), sd = sd(profit))
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

shapiro.test(sample_quantiles)
fisher.test(sample_quantiles)