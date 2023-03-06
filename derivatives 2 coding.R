################################################################################
############################### Derivatives 2 ##################################
################################################################################
rm(list =ls())
# library ####
require(MASS); require(tidyverse); require(ggplot2)
require(Matrix)


A = matrix(rnorm(9),3,3)
A =A %>% triu() %>% as.matrix()
matrixStats::rowSums2(A)
Matrix::rowSums(A)[1]

## CIR process
kappa = 2; theta = 0.02; sigma = 0.09; dt = 1/12 # monthly
Time_step = 200 #do it 100 times from 0 to 99
r_0 = 0.08; r = c(r_0, rep(NA,Time_step-1))
for (i in c(2:(Time_step))){
  d_r = kappa*(theta -r[i-1])*dt+ sigma*sqrt(r[i-1])*sqrt(dt)*rnorm(1,0,1)
  r[i] = r[i-1]+d_r }
plot(r, type = "l", col= "purple")

## ABM process
mu = 0.2
sigma = 0.09
dt = 1/12
Time_step = 2000
r_0 = 0.08
r = c(r_0, rep(NA,Time_step-1))
for (i in c(2:(Time_step))){
  d_r = mu*dt+ sigma*sqrt(dt)*rnorm(1,0,1)
  r[i] = r[i-1]+d_r }
plot(r, type = "l", col= "purple", main = "AB Motion", xlab = "time", ylab = "rate")


##GBM process using mu and sigma as athrimetic
d_r = c(0, rep(NA, Time_step-1))
r = c(r_0, rep(NA,Time_step-1))
for (i in c(2:(Time_step))){
  d_r[i] = mu*r[i-1]*dt+ sigma*r[i-1]*sqrt(dt)*rnorm(1,0,1)
  r[i] = r[i-1]+d_r[i] }
plot(r, type = "l", col= "purple")
hist(d_r/r, breaks = 150, main = "Distribution of Dx/X", col= "purple");lines(density(d_r/r))

## MATLAB tutorial

#clear all matrices;
rm(list = ls())
#clear command window
#ctr + 2

#define matrix X
#rows are separated by comma
A = matrix(1:4,2,2, byrow = TRUE)

#transpose
Transpose_A = A %>% t

#define column vector y
b = c(5,6)

# multiplication 
c = A%*%b

#inverse
Ainv = solve(A);

#rank 
rk = rankMatrix(A)
  
#special Matlab syntax for solution of system of equations A*x = b
x = solve(A)%*%b

#loop (here to compute factorial of 10)
prod = 1;
for (i in c(2:10)){
prod = prod*i}; prod

#factorial as function
test=factorial(10);
A%*%x

# binomial coefficient (choose k out of n)
bc = combinations(5,3) %>% nrow()

# special vectors and matrices
# 3-row column vector of 1s
mat_of_ones = matrix(1,3,3);

# 3-dimenisonal identity matrix
ident = diag(1,3);

# operations on vectors
#elementwise division
s = c(1:3)/c(4,6,7)
#taking a number to the power of a vector
t = 5^c(1:3)
require(magrittr)
#estimating parameter from interestate
rate = read.csv("E:/Goethe Uni MMF/IV. SS 19-20/Credit Risks/GS1M.csv")
rate_total = rate %>% set_colnames(c("Month", "Rate")) %>% 
         mutate(Rate = Rate/(100*12)) %>% mutate(D_R = Rate -lag(Rate,1)) %>% 
         mutate(stochastic = rnorm(215,0,1))


#black scholes models that pull out call (put) prices, delta, gamma and vega as output + including dividend yield
# step 1 develop a function like professor required 
Black_Scholes = function (S, K, r, sigma, dividend_yield, maturity, price_time) {
  #unit block for use later
  delta_T    = maturity - price_time
  d_1        = (log(S/K) + (r- dividend_yield + sigma^2/2)*delta_T)*(sigma*sqrt(delta_T))^-1
  d_2        = d_1 - sigma*sqrt(delta_T)
  div_cum    = exp(-dividend_yield*delta_T)
  
  #calculating prices
  Call_t_T   = S*div_cum*pnorm(d_1) - K*exp(-r*(delta_T))*pnorm(d_2)
  Put_t_T    = Call_t_T - S*div_cum + K*exp(-r*(delta_T))
  #calculating Delta
  Delta_Call = div_cum*pnorm(d_1)
  Delta_put  = -div_cum*pnorm(-d_1)
  #Calculating Gamma
  Gamma = div_cum*dnorm(d_1)/(S*sigma*sqrt(delta_T)) # same for both call-put
  #Calculating Vega
  Vega  = div_cum*dnorm(d_1)*S*sqrt(delta_T)
  
  #summary
  output = cbind(Call_t_T, Put_t_T, Delta_Call, Delta_put, Gamma, Vega) %>% as.matrix %>% 
    #magrittr::set_rownames("Summary figures") %>% 
    magrittr::set_colnames(.,c("Call price", "Put price", "Delta Call", "Delta Put", "Gamma", "Vega"))
  return(output)
}
S = 100:120
Black_Scholes(S, K = 100, r = 0.05, sigma= 0.15, dividend_yield = 0.02, maturity = 5, price_time = 0)

## step 2, make it more usefull if we can map the BS function into a panel 

S = rep(100,100); 
K = rep(110, length(S)); 
r = 0.05; sigma = seq(0.01,0.2, length.out = length(S)); dividend_yield= 0.02; maturity = 1; price_time = 0

pmap(list(S,K,r,sigma, dividend_yield, maturity,price_time), Black_Scholes()[1]) # not really useful

data_table = data.frame(S,K,r,sigma, dividend_yield,maturity,price_time) %>% 
  rowwise %>% 
  mutate(Call = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[1],
         Put  = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[2],
         Delta_C = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[3],
         Delta_P = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[4],
         Gamma = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[5],
         Vega = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[6]
  )
require(ggplot2)
ggplot(data_table)+ aes(x = sigma, y = Vega)+ geom_line() +
  geom_line(aes(x = sigma, y = Call))


#Stock price:  40, 40.5, 41 | K = 40 | r = 0.01 | sigma = 0.2 | zero dividend yield
maturity = seq(1/52, 1, 1/52) %>% rep(.,3)
S = c(40, 40.5, 41) %>% rep(., each = length(maturity))
K = 40; r = 0.01; sigma = 0.2; dividend_yield = 0;
price_time = 0

data_table = data.frame(S,K,r,sigma, dividend_yield,maturity,price_time) %>%  
  rowwise() %>% 
  mutate(Call = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[1],
         Put  = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[2],
         Delta_C = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[3],
         Delta_P = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[4],
         Gamma = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[5],
         Vega = Black_Scholes(S,K,r,sigma, dividend_yield, maturity, price_time)[6]
  )

p1 = ggplot(data_table %>% filter(S==40)) + geom_line(aes(x = maturity, y = Delta_C)) + ylab("40")
p2 = ggplot(data_table %>% filter(S==40.5)) + geom_line(aes(x = maturity, y = Delta_C)) + ylab("40.5")
p3 = ggplot(data_table %>% filter(S==41)) + geom_line(aes(x = maturity, y = Delta_C)) + ylab("41")
par(mfrow=c(2,2))
gridExtra::grid.arrange(p1, p2,p3,ncol = 1)


ggplot()+  geom_line(aes(x = maturity, y = Delta_C, colour = factor(S)), data = data_table)+ 
  ggtitle("Delta Call across Maturity")+ xlab("Maturity (Weeks)") + ylab("Delta C")
  
## Integrating with mid-point method

test_fun = function(x){2*x^3 -12*x^2+4}
Reg_Int = function (f, a, b, step, option) {
sum = 0
  series = seq(a,b, length.out = step); 
  L = length(series)-1
  for(i in c(1:(L))){
    midpoint = 0.5*(series[i+1]+series[i])
    area = if(option == 0) {((series[i+1]-series[i])*f(midpoint))} else {abs(((series[i+1]-series[i])*f(midpoint)))}
    sum = sum + area
  }
  return(sum)
}
curve(test_fun, color = "Blue", xlim = c(-4,4), from =-4, n = 1001, ylab = "y")


##Beyond Black Scholes: Hesian, Merton
## Heston model
runif(10000,0,1) %>% hist(., breaks = 100)



set.seed(1234)
x = rnorm(10)
x
   

