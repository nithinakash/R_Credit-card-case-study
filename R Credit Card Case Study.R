setwd('C:/Users/Nithin/Downloads/R - Credit card case study/R case study 2 (Credit card)')

#**************************************************************************************************************#

#########################
#-->Required Packages<--#
#########################
require(dplyr)
require(lubridate)
require(ggplot2)
require(scales)

#**************************************************************************************************************#

cust <- read.csv('Customer Acqusition.csv')
repay <- read.csv('Repayment.csv')
spend <- read.csv('spend.csv')

#Data Prep
cust_final <- merge(x = cust,y = repay,by ='Customer',all = F)
cust_final <- merge(x = cust_final,y = spend,by ='Customer',all = F)

colnames(cust_final)
cust_final <-  dplyr::mutate(cust_final,No = NULL,SL.No. = NULL,Sl.No. = NULL,X = NULL)

cust_final <-  dplyr::rename(cust_final,
              'Repay_mnth' = 'Month.x','repay_amt' = 'Amount.x','spend_mnth' = 'Month.y','spend_amt' ='Amount.y')

cust_final$Repay_mnth <- lubridate::dmy(cust_final$Repay_mnth)
cust_final$spend_mnth <- lubridate::dmy(cust_final$spend_mnth)

#**************************************************************************************************************#

#1.In the above dataset,
#  a.Incase age is less than 18, replace it with mean of age values.
#  b.Incase spend amount is more than the limit, replace it with 50% of that customer's limit. 
#    (customer's limit provided in acquisition table is the per transaction limit on his card)
#  c.Incase the repayment amount is more than the limit, replace the repayment with the limit.

#a.
cust_final$Age[cust_final$Age<18] <- mean(cust_final$Age)

#b.
cust_final <- dplyr::mutate(cust_final,spend_amt = if_else(cust_final$spend_amt > cust_final$Limit,cust_final$Limit * 0.5,cust_final$spend_amt))

#c.
cust_final <- dplyr::mutate(cust_final,repay_amt = ifelse(cust_final$repay_amt > cust_final$Limit,cust_final$Limit,cust_final$repay_amt))

#**************************************************************************************************************#

#2.From the above dataset create the following summaries:
#  a.How many distinct customers exist?
#  b.How many distinct categories exist?
#  c.What is the average monthly spend by customers?
#  d.What is the average monthly repayment by customers?
#  e.If the monthly rate of interest is 2.9%, what is the profit for the bank for each month? 
#    (Profit is defined as interest earned on Monthly Profit. 
#    Monthly Profit = Monthly repayment - Monthly spend. Interest is earned only on positive profits and not on negative amounts)
#  f.What are the top 5 product types?
#  g.Which city is having maximum spend?
#  h.Which age group is spending more money?
#  i.Who are the top 10 customers in terms of repayment?
  
#a.
nrow(cust_final[!duplicated(cust_final$Customer),])

#b.
nrow(cust_final[!duplicated(cust_final$Product),])

#c.
avg_mnt_spnd <- cust_final %>% dplyr::group_by(Customer,spnd_mnth = lubridate::month(spend_mnth)) %>% summarise(Avg_spnd = mean(spend_amt,na.rm = T)) %>% arrange(Customer,spnd_mnth)

#d.
avg_mnt_rep <- cust_final %>% dplyr::group_by(Customer,Rep_mnth = lubridate::month(spend_mnth)) %>% summarise(Avg_spnd = mean(Repay_mnth,na.rm = T)) %>% arrange(Customer,Rep_mnth)   

#e.
cust_final <-  dplyr::mutate(cust_final,'Profit'= cust_final$repay_amt - cust_final$spend_amt)
cust_final$Profit[cust_final$Profit < 0] <- 0
cust_final %>% dplyr::group_by(lubridate::month(Repay_mnth)) %>% summarise(tot_prof = sum(Profit)*0.029)

#f.
head(cust_final %>% dplyr::group_by(Type) %>% summarise(Prof = sum(spend_amt)) %>% arrange(desc(Prof)),5)


#g.
head(cust_final %>% dplyr::group_by(City) %>% summarise(spnd = sum(spend_amt)) %>% arrange(desc(spnd)),1)

#h.
head(cust_final %>% dplyr::group_by(Age) %>% summarise(spnd = sum(spend_amt)) %>% arrange(desc(spnd)),3)

#i.
head(cust_final %>% dplyr::group_by(Customer) %>% summarise(repa = sum(repay_amt)) %>% arrange(desc(repa)),10)

#**************************************************************************************************************#

#3.Calculate the city wise spend on each product on yearly basis. Also include a graphical representation for the same.

df <- cust_final %>% dplyr::group_by(City,Product,lubridate::year(spend_mnth)) %>% summarise(tot_spnd = sum(spend_amt))

ggplot2::ggplot(data = df) + aes (x = City, y = tot_spnd, fill = Product ) + geom_col() + scale_y_continuous(labels = scales::comma)

#**************************************************************************************************************#

#4.Create graphs for
#  a.Monthly comparison of total spends, city wise
#  b.Comparison of yearly spend on air tickets
#  c.Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)

#a.
df1 <- cust_final %>% dplyr::group_by(City,mnt = lubridate::month(spend_mnth)) %>% summarise(spnd = sum(spend_amt))

ggplot2::ggplot(df1) + aes(x = mnt,y = spnd, fill = City) + geom_col() + scale_y_continuous(labels = scales::comma)

#b.
yr_air <- cust_final[cust_final$Type == 'AIR TICKET',] %>% dplyr::group_by(yr = lubridate::year(spend_mnth)) %>% summarise(spnd = sum(spend_amt))

ggplot2::ggplot(yr_air) + aes(x = yr,y = spnd) + geom_col() + scale_y_continuous(labels = scales::comma)

#c.
prod_mnt <- cust_final %>% dplyr::group_by(Product,mnt = lubridate::month(spend_mnth)) %>% summarise(spnd = sum(spend_amt))

ggplot2::ggplot(prod_mnt) + aes(x = mnt,y = spnd,fill = Product) + geom_col()

#Spend in Maximun on January Month

#**************************************************************************************************************#

#5.Write user defined R function to perform the following analysis:
#  You need to find top 10 customers for each city in terms of their repayment amount by different products and 
#  by different time periods i.e. year or month. The user should be able to specify the product (Gold/Silver/Platinum) and 
#  time period (yearly or monthly) and the function should automatically take these inputs while identifying the top 10 customers.

prod <- readline("Please Enter Product Category in Gold/Silver/Platinum: ")
#Enter value in console
mnt <- readline("Please Enter Time Period in yearly/monthly: ")
#Enter value in console


Top10_Report <- function(prod,mnt) {
  if(mnt == 'yearly'){
    Top10 <- head(cust_final[cust_final$Product == prod,] %>% dplyr::group_by(Product,City,Customer,yr = lubridate::year(Repay_mnth)) %>% 
           summarise(rep = sum(repay_amt)) %>% arrange(desc(rep)),10)
    return(Top10 )
    }else if (mnt == 'monthly'){
    Top10 <- head(cust_final[cust_final$Product == prod,] %>% dplyr::group_by(Product,City,Customer,mnt = lubridate::month(Repay_mnth)) %>% 
                    summarise(rep = sum(repay_amt)) %>% arrange(desc(rep)),10)
    return(Top10)
    }else {
    print('Wrong Input')
    }
}

Top10_Report(prod,mnt)

#**************************************************************************************************************#