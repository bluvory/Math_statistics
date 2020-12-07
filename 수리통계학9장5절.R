#13주차 강의자료 15페이지---------------
x <- c(40, 50, 30, 60, 70, 60, 30, 60, 20, 80)
y <- c(87, 108, 69, 135, 148, 132, 73, 128, 50, 170)

xbar <- mean(x)
ybar <- mean(y)

n <- length(x)
d_x <- x-xbar #x값과 xbar의 차이들
d_y <- y-ybar #y값과 ybar의 차이들

d_x_2 <- (x-xbar)^2 #x값과 xbar의 차이들의 제곱
d_y_2 <- (y-ybar)^2 #y값과 ybar의 차이들의 제곱

print(x)
print(y)
print(d_x)
print(d_y)
print(d_x_2)
print(d_y_2)



### 2. 상관계수 ###
s_xx <- sum(x^2)-n*xbar^2    #x값과 xbar의 차이들의 제곱의 합
s_xy <- sum(x*y)-n*xbar*ybar #y값과 ybar의 차이들의 제곱
s_yy <- sum(y^2)-n*ybar^2    #x값과 xbar의 차이들*y값과 ybar의 차이들


r <- s_xy/(sqrt(s_xx)*sqrt(s_yy))
print(r)



### 3. 회귀계수 추정 ###
b1 <- s_xy/s_xx
b0 <- ybar-b1*xbar



### 4. 추정식 ###


### 5. 분산분석 ###
k = 1   # 단순회귀모형일때 1 (독립변수의 개수)

sst <- s_yy
ssr <- b0^2 * s_xx
sse <- s_yy-((s_xy)^2/s_xx)

msr <- ssr / k
mse <- sse / (n-k-1)

print(c(sst, ssr, sse, msr, mse))


### 6. 회귀계수의 구간추정 ###
alpha <- 0.05

t_alpha_half <- qt(1-alpha/2,n-2)
d1 <- t_alpha_half * sqrt(mse/s_xx)            #beta1
d0 <- t_alpha_half * sqrt(mse * (1/n + xbar^2/s_xx)) #beta0

print(c('beta1의 신뢰구간', b1-d1, b1+d1))
print(c('beta0의 신뢰구간', b0-d0, b0+d0))



### 7. 회귀계수의 가설설정 ###
t1 <- (b1-0) / sqrt(mse/s_xx)
t2 <- (b0-0) / sqrt(mse * (1/n+xbar^2/s_xx))
print(c('beta1의 검정통계량', t1))
print(c('beta0의 검정통계량', t2))




