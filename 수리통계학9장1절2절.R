# 9.1.1
1 - pchisq(6.25, 3)

# 9.1.2
qchisq(0.95, 9)

# 9.1.3
qchisq(0.95, 3)

# 9.2.1
o_A <- c(95,36,71,21,45,32)
o_B <- c(53,26,43,18,32,28)
nA <- sum(o_A)
nB <- sum(o_B)

nr <- 2 #행 개수
nc <- length(o_A)

phat <- (o_A + o_B) / (nA + nB)
e_A <- nA*phat
e_B <- nB*phat

d_A <- (o_A-e_A)^2/e_A
d_B <- (o_B-e_B)^2/e_B
chisq0 <- sum(d_A) + sum(d_B)
alpha <- 0.05
df <- (nr-1)*(nc-1)
chisq_alpha <- qchisq(1-alpha, df)
print(c(chisq0, chisq_alpha))


# 9.2.3
o_A <- c(262, 231, 10)
o_B <- c(302, 205, 5)
o_sum <- c(564, 436, 15)
nA <- sum(o_A)
nB <- sum(o_B)
nC <- sum(o_sum)

nr <- 2 #행 개수
nc <- length(o_A)

phat <- (o_A + o_B) / (nA + nB)
e_A <- nC*(nA/nC)*(o_sum/nC)
e_B <- nC*(nB/nC)*(o_sum/nC)

d_A <- (o_A-e_A)^2/e_A
d_B <- (o_B-e_B)^2/e_B
chisq0 <- sum(d_A) + sum(d_B)
alpha <- 0.05
df <- (nr-1)*(nc-1)
chisq_alpha <- qchisq(1-alpha, df)
pvalue <- 1-pchisq(chisq0, df)
print(c(chisq0, chisq_alpha, pvalue))



# 9.2.4
o_A <- c(4, 11, 15, 6, 9)
o_B <- c(7, 18, 6, 6, 18)
o_sum <- c(11, 29, 21, 12, 27)
nA <- sum(o_A)
nB <- sum(o_B)
nC <- sum(o_sum)

nr <- 2 #행 개수
nc <- length(o_A)

phat <- (o_A + o_B) / (nA + nB)
e_A <- nC*(nA/nC)*(o_sum/nC)
e_B <- nC*(nB/nC)*(o_sum/nC)

d_A <- (o_A-e_A)^2/e_A
d_B <- (o_B-e_B)^2/e_B
chisq0 <- sum(d_A) + sum(d_B)
alpha <- 0.05
df <- (nr-1)*(nc-1)
chisq_alpha <- qchisq(1-alpha, df)
pvalue <- 1-pchisq(chisq0, df)
print(c(chisq0, chisq_alpha, pvalue))


# 9.2.5
o_A <- c(12, 8)
o_B <- c(13, 27)
nA <- sum(o_A)
nB <- sum(o_B)

nr <- 2 #행 개수
nc <- length(o_A)

phat <- (o_A + o_B) / (nA + nB)
e_A <- nA*phat
e_B <- nB*phat

d_A <- (o_A-e_A)^2/e_A
d_B <- (o_B-e_B)^2/e_B
chisq0 <- sum(d_A) + sum(d_B)
alpha <- 0.05
df <- (nr-1)*(nc-1)
chisq_alpha <- qchisq(1-alpha, df)
pvalue <- 1-pchisq(chisq0, df)
print(c(chisq0, chisq_alpha, pvalue))



# 9.2.7
o_A <- c(30, 68, 10)
o_B <- c(61, 79, 20)
o_C <- c(98, 43, 21)

nA <- sum(o_A)
nB <- sum(o_B)
nC <- sum(o_C)

nr <- 3 #행 개수
nc <- length(o_A)

phat <- (o_A + o_B + o_C) / (nA + nB + nC)
e_A <- nA*phat
e_B <- nB*phat
e_C <- nC*phat

d_A <- (o_A-e_A)^2/e_A
d_B <- (o_B-e_B)^2/e_B
d_C <- (o_C-e_C)^2/e_C

chisq0 <- sum(d_A) + sum(d_B) + sum(d_C)
alpha <- 0.05
df <- (nr-1)*(nc-1)
chisq_alpha <- qchisq(1-alpha, df)

namelist <- list('  연령' = c('35세 미만', '35-53세', '54세 이상'), '가장 신뢰하는 대중매체' = c('신문', 'TV', '라디오'))
o <- matrix(c(o_A, o_B, o_C), nrow = 3, ncol = 3, byrow = TRUE , dimnames = namelist ) #관측도수

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)

e <- (rowsum %*% t(colsum))/sum(o) #기대도수
dimnames(e) <- namelist

d <- (o-e)^2/e #이거 다 더하면 카이제곱

print(o)
print(d)
print(e)

chisq0 <- sum(d)
df <- (nrow(o)-1)*(ncol(o)-1)
alpha <- 0.05
chisqp_alpha <- qchisq(1-alpha, df)
print(c(chisq0, chisq_alpha))

pvalue <- 1-pchisq(chisq0, df)
print(pvalue)

