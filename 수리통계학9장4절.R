

# 9.4.2
y1 <- c(52,47,44,51,42)
y2 <- c(60,55,49,52,43)
y3 <- c(56,48,45,44,38)

a <- 3   # A요인개수
b <- length(y1)   # B요인개수

y <- c(y1, y2, y3)
ni <- c(length(y1), length(y2), length(y3))
a_bar <- c(mean(y1), mean(y2), mean(y3))
b_bar <- c((y1+y2+y3)/a)
ybar <- sum(a_bar) / a


ssto <- sum((ybar - y)**2)
ssa <- b * sum((a_bar - ybar)**2)
ssb <- a * sum((b_bar - ybar)**2)
sse <- ssto - ssa - ssb

df_a <- a-1
df_b <- b-1
df_e <- (a-1)*(b-1)
df_sum <- a*b - 1

msa <- ssa / df_a
msb <- ssb / df_b
mse <- sse / df_e

F_a <- msa / mse
F_b <- msb / mse


alpha <- 0.05
f_alpha_a <- qf(1-alpha, df_a, df_e)
f_alpha_b <- qf(1-alpha, df_b, df_e)
pvalue_a <- 1-pf(F_a, df_a, df_e)
pvalue_b <- 1-pf(F_b, df_b, df_e)

print(c(F_a, f_alpha_a, pvalue_a))
print(c(F_b, f_alpha_b, pvalue_b))

print(c(ssa,ssb,sse,ssto))
print(c(msa, msb, mse))



