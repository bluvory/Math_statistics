# 9.3.1
y1 <- c(5,9,6,8)
y2 <- c(11,13,10,12)
y3 <- c(10,6,9,9)

k <- 3 #범주개수

ni <- c(length(y1), length(y2), length(y3))
yibar <- c(mean(y1), mean(y2), mean(y3))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3)))
sst <- sse + sstr

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))


# 9.3.2
y1 <- c(194.11, 182.80, 187.43)
y2 <- c(216.06, 203.50, 216.88)
y3 <- c(178.10, 189.20, 181.33)
y4 <- c(197.11, 202.68, 209.18)

k <- 4 #범주개수

ni <- c(3, 3, 3, 3) #c(length(y1), length(y2), length(y3), length(y4))
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3), var(y4)))
sst <- sse + sstr

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))



# 9.3.3
y1 <- c(22.0, 23.9, 20.9, 23.8, 25.0, 24.0, 21.7, 23.8, 22.8, 23.1, 23.1, 23.5, 23.0, 23.0)
y2 <- c(21.8, 23.0, 23.3, 22.4, 23.0, 23.0, 23.0, 22.4, 23.9, 22.3, 22.0, 22.6, 22.0, 22.1, 21.1, 23.0)
y3 <- c(19.8, 22.1, 21.5, 20.9, 22.0, 21.0, 22.3, 21.0, 20.3, 20.9, 22.0, 20.0, 20.8, 21.2, 21.0)

k <- 3 #범주개수

ni <- c(length(y1), length(y2), length(y3))
yibar <- c(mean(y1), mean(y2), mean(y3))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3)))
sst <- sse + sstr

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))



# 9.3.4
y1 <- c(7,7,15,11,9)
y2 <- c(12,17,12,18,18)
y3 <- c(14,18,18,19,19)
y4 <- c(19,25,22,19,23)
y5 <- c(7,10,11,15,11)

k <- 5 #범주개수

ni <- c(length(y1), length(y2), length(y3), length(y4), length(y5))
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4), mean(y5))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3), var(y4), var(y5)))
sst <- sse + sstr

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))



# 9.3.5
y1 <- c(240, 221, 265)
y2 <- c(286, 256, 272)
y3 <- c(259, 245, 232)
y4 <- c(239, 215, 223)

k <- 4 #범주개수

ni <- c(length(y1), length(y2), length(y3), length(y4))
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3), var(y4)))
sst <- sse + sstr

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))

beta <- 0.025
f_beta <- qf(1-beta, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_beta, pvalue))



# 9.3.6
y1 <- c(38.7, 39.2, 40.1, 38.9)
y2 <- c(41.9, 42.3, 41.3)
y3 <- c(40.8, 41.2, 39.5, 38.9, 40.3)

k <- 3 #범주개수

ni <- c(length(y1), length(y2), length(y3))
yibar <- c(mean(y1), mean(y2), mean(y3))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3)))
sst <- sse + sst

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))



# 9.3.7
y1 <- c(43.06, 43.32, 42.63, 42.86, 43.05, 42.87, 42.94, 42.80, 42.36)
y2 <- c(42.33, 42.81, 42.13, 42.41, 42.39, 42.10, 42.42, 41.42, 42.52)
y3 <- c(42.83, 42.57, 42.96, 43.16, 42.25, 42.24, 42.20, 41.97, 42.61)


k <- 3 #범주개수

ni <- c(length(y1), length(y2), length(y3))
yibar <- c(mean(y1), mean(y2), mean(y3))
n <- sum(ni)
ybar <- sum(ni*yibar)/n #전체평균

sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum ((ni-1)*c(var(y1), var(y2), var(y3)))
sst <- sse + sst

df <- c(k-1, n-k, n-1)
ss <- c(sstr, sse, sst)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovaTb1 <- data.frame('제곱합'=ss, '자유도'= df, '평균제곱MS' = c(ms[1], ms[2], ''), 'f값' = c(f0, '', ''))
rownames(anovaTb1) <- c('처리간SST', '처리내SSE', '합계SSTO')
print(anovaTb1)

alpha <- 0.01
f_alpha <- qf(1-alpha, k-1, n-k)
pvalue <- 1-pf(f0, k-1, n-k)
print(c(f0,f_alpha, pvalue))

