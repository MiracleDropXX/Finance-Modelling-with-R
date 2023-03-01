# ASSIGNMENT NO:.....
# BY:
# TOPIC:

# step 0, loading necessary package, script and data
library(readxl)
library(zoo)
library(TTR)
library(readxl)
library(dplyr)
library(PerformanceAnalytics)
library(xts)
library(ggplot2)
require(TTR)
library(tidyquant)
library(tidyverse)
library(quantmod)
library(treasuryTR)
library(reshape2)
library(ggplot2)
library(lubridate)
# Mardoff data:
Madoff_Fairfield_return <- readxl::read_xlsx("Madoff-Fairfield-return.xlsx", 
                                      col_types = c("text", "text", "date", 
                                                    "numeric")) # file name wrong, must be identical


## support function
# support function  ============================================================
BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}

# data inspection: basic sanity check on the data, all seems good, no crazy outliers
summary(Madoff_Fairfield_return)
# View(Madoff_Fairfield_return)
# type of Madoff_Fairfield_retrurn?: list 
typeof(Madoff_Fairfield_return) # convert to data frame for better operation:

Madoff_Fairfield_return <- as.data.frame(Madoff_Fairfield_return) # command: doing calculation (right hand side) on an object and directly assign the result to the same object


# calculate Fairfield montly return and st.deviation
# take out the return (4th column from a dataframe to a separate vector, not needed but perform nonetheless)
# alternatively (matrix operation syntax, to attract element from a matrix needing row index and column index)
# e.g., extracting an element from a matrix: matrix_name[row_index, column_index]
# extract entire column: matrix_name[,column_index] / extract entire row: matrix_name[row_index,]
Madoff_return_monthly  <-   Madoff_Fairfield_return[,4] 
# for row: for example: 
# NOT RUN: Madoff_Fairfield_return[1, ]



# Part 1: calculating basic satistic of Mardoff Fund: monthly mean, sd, 
Madoff_average_monthly <- mean(Madoff_return_monthly)
# Mean/monthly average
# print result:
print(Madoff_average_monthly)
#test: not run: sum(all return)/length( all return)
sum(Madoff_Fairfield_return$Return)/length(Madoff_Fairfield_return$Return)

#monhly standard deviation:
Madoff_sd_monthly      <- sd(Madoff_return_monthly)
print(Madoff_sd_monthly)
# test: using formula: square root of: squared sum of residuals divided by N-1
sqrt(sum((Madoff_return_monthly - Madoff_average_monthly)^2)/(length(Madoff_return_monthly)-1))
#st.deviation
# is matching?:
Madoff_sd_monthly - sqrt(sum((Madoff_return_monthly - Madoff_average_monthly)^2)/(length(Madoff_return_monthly)-1)) # very close to zero... just rounding issue
print(Madoff_sd_monthly)


# annualized return and standard deviation
# formula: Prod(1+ret_i) = (1+ ret_annualized)^(length of time expressed in year)
return_annualized <- prod(1+ Madoff_return_monthly)^(12/length(Madoff_return_monthly))-1   #total return -> annualized
stdev_annualized  <- Madoff_sd_monthly*sqrt(12)

# create a result table:
basicStatistic <- c(Madoff_average_monthly, Madoff_sd_monthly, return_annualized, stdev_annualized);
# naming the element in the vector result:
names(basicStatistic) <- c("monthly average return", "monthly stdev", "annualized return", "annualized stdev")

#View object:
print(basicStatistic) # alternatively: View(basicStatistic)

Return_by_year <- Madoff_Fairfield_return %>% 
  group_by(Year) %>% 
  summarise(ReturnperYear = (prod(1+Return)-1))

stdev_year <- sd(Return_by_year$ReturnperYear) #very small but recall: annual vs annualized is a very different things
mean_year  <- mean(Return_by_year$ReturnperYear)


# ploting data:
# adding the first element for starting period ( no return, 100 USD investment)

date <- c(lubridate::as_date("1990-11-30"), Madoff_Fairfield_return$Date)
data_for_plot <- data.frame(
  date = date,
  return = c(0,Madoff_Fairfield_return$Return ) ) %>%
  mutate(`Mardoff Performance` = 100*(cumprod(1+return)))

# PART 2: replica of Mardoff performance: ==================
getSymbols(c("^GSPC","^VIX"),from = as.Date("1990-11-30"), to = as.Date("2008-12-31"))
Tbill_1M <- get_yields(
  series = "DGS1MO",
  
  na_locf = TRUE,
  percent_adjust = TRUE,
  format_out = "tibble"
)
Tbill_3M <- get_yields(
  series = "DGS3MO",
  
  na_locf = TRUE,
  percent_adjust = TRUE,
  format_out = "tibble"
)

require(lubridate)
month <- seq(as.Date("1990-12-01"),as.Date("2009-01-01"),by="months")-1
day   <- seq(as.Date("1990-12-01"),as.Date("2009-01-01"),by="days")-1

Tbill_all <- left_join(Tbill_3M,Tbill_1M, by = c("date" = "date")) |> 
  as.data.frame() %>% 
  rowwise() %>%  
  mutate(riskFree = ifelse(is.na(DGS1MO), DGS3MO, DGS1MO))|> 
  select(c("date","riskFree"))

SnP_andVIX <- merge(GSPC, VIX) %>%  as.data.frame  %>%  rownames_to_column()
colnames(SnP_andVIX)[1] <- "date" 
SnP_andVIX$date <- as.Date(SnP_andVIX$date)

# translate VIX to percentage



SnP_andVIX <- SnP_andVIX %>% 
  mutate(SnPReturn = ifelse(is.na(GSPC.Adjusted/lag(GSPC.Adjusted,1)),0,GSPC.Adjusted/lag(GSPC.Adjusted,1)-1)) %>% 
  mutate(SnPaccumulate = cumprod(SnPReturn+1))


SnP_VIX_shortList <- data.frame(date = SnP_andVIX$date,
                                SnP  = SnP_andVIX$GSPC.Close,
                                VIX  = SnP_andVIX$VIX.Close/100)



data_aggregated <- data.frame(date = day)

# fill in data column
data_aggregated <- data_aggregated %>% 
  left_join(.,Tbill_all, by = c("date"= "date")) %>% 
  left_join(.,SnP_VIX_shortList, by = c("date" ="date")) %>% 
  na.locf() 
data_aggregated_month <- data_aggregated %>% 
  filter(date %in% month)

parameterSSCdistance<- 0.05 # 5 percent below/upper of the index
parameterSSCMaturity <- 1/12 #y
S0 <- data_aggregated_month$SnP[1]

data_aggregated_month <- data_aggregated_month %>%
  rowwise() %>% 
  mutate(Kcall = SnP*(1+parameterSSCdistance),
         Kput = SnP*(1-parameterSSCdistance),
         PriceCall  = BlackScholes(SnP, Kcall, T= riskFree,parameterSSCMaturity , VIX, type = "C"),
         PricePut = BlackScholes(SnP, Kput, T= riskFree,parameterSSCMaturity ,VIX,type = "P"))


# t0 long Index, long put sell call
data_aggregated_month$Nindex <- NA; data_aggregated_month$Nindex[1] <- 100/data_aggregated_month$SnP[1]
data_aggregated_month$indexPortfolio <- NA; 
data_aggregated_month$indexPortfolio[1] <- data_aggregated_month$Nindex[1]*data_aggregated_month$SnP[1]
data_aggregated_month$cashValue <- NA;
data_aggregated_month$cashValue[1] <- data_aggregated_month$Nindex[1]*(data_aggregated_month$PriceCall[1]- 
                                                                         data_aggregated_month$PricePut[1])


data_aggregated_month$settlement <- NA
data_aggregated_month$V_h <- data_aggregated_month$cashValue[1] + data_aggregated_month$indexPortfolio[1] # for lazy initiation
data_aggregated_month$traditionalLong <- 100

for(i in 2:nrow(data_aggregated_month)){
  data_aggregated_month$settlement[i] <- (max(0, data_aggregated_month$Kput[i-1] -data_aggregated_month$SnP[i]) +
                                            - max(0, data_aggregated_month$SnP[i]-data_aggregated_month$Kcall[i-1]))*data_aggregated_month$Nindex[i-1]
  
  endingBalance <- data_aggregated_month$SnP[i]*data_aggregated_month$Nindex[i-1] +  data_aggregated_month$settlement[i]  +  data_aggregated_month$cashValue[i-1]*(1+ data_aggregated_month$riskFree[i-1]/12)
  
  # reinvest endingBalance into index:
  data_aggregated_month$Nindex[i] <- endingBalance/data_aggregated_month$SnP[i] 
  # portfolio (directly equal to endingBalance)
  data_aggregated_month$indexPortfolio[i] <- endingBalance
  data_aggregated_month$cashValue[i]      <- data_aggregated_month$Nindex[i]*(+data_aggregated_month$PriceCall[i]- 
                                                                                data_aggregated_month$PricePut[i])
  data_aggregated_month$V_h[i] <- data_aggregated_month$cashValue[i]  + data_aggregated_month$indexPortfolio[i] 
  data_aggregated_month$traditionalLong[i] <-   data_aggregated_month$traditionalLong[i-1]*( data_aggregated_month$SnP[i]/ data_aggregated_month$SnP[i-1])
}


colnames(data_aggregated_month)[c(13,14)] <- c("SSC strategy", "S&P 500")


data_aggregated_month <- data_aggregated_month[1:(nrow(data_aggregated_month)-2),] %>%
  left_join(data_for_plot)



plot_data <- data_aggregated_month[,c(1,13,14,16)] %>% melt(., id.var = "date", value.name  = "Value",
                                                            variable.name = "Strategy")

library(reshape2); library(reshape2);require(gridExtra)

p1 <- ggplot(plot_data) +
  aes(x = date, y = Value, col = Strategy) + geom_line() +
  ggtitle("Performance Comparision (1990-2008)") + 
  xlab("Date") +
  ylab("Value of initial USD 100 investment")


tiff(paste0("Mardoff",".png"), units = "cm", width = 42, height = 18, res = 150)

grid.arrange(
  p1
)

dev.off()