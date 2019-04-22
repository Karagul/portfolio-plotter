# Portfolio Plotter Version 1.2, 21.03.2019
library(quantmod) # download stock data from the internet
library(magrittr) # the pipe 
library(tidyverse) # used for data wrangling, written by Hadley Wickham
library(zoo) # used for dealing with xts objects

EXS2.DE<-(getSymbols("EXS2.DE", 
                             from = "2018-01-01", 
                             to = "2019-2-28", 
                             src="yahoo",
                             auto.assign=FALSE,
                             na.omit = TRUE))
chartSeries(EXS2.DE,theme = chartTheme("white"))
# An example of the stock data


transactions <- read.table("H:/Data_Science/Financial Modeling/buydata.txt", 
                           dec=".", 
                           header = T)
# Define start and end date for stock data
startdate <- "2017-10-03"
enddate <- "2019-04-28"
# enddate <- Sys.Date() # Let's use today as the end date. Careful, if stock data on Yahoo finance is incomplete, as is the case for some foreign stocks such as EXS2.DE, then the end date should be later than Sys.Date(). Otherwise, the stock value on Sys.Date() will not be plotted.

aN<-length(levels(transactions[,4]))
aN 


# Loop 1 begin (main loop)

for(a in 1:aN){
  loop_stocksym <- levels(transactions[,4])[a] # select stock symbol
  tmpstock_data <- getSymbols(loop_stocksym, #pull stock data
                              from = startdate,
                              to = enddate,
                              src="yahoo",
                              auto.assign=FALSE) %>% .[,4]
  stocknam <- filter(transactions, 
                     #select stock name from transactions (5th col of transactions)
                     stocksymbol == loop_stocksym) %>%
    select(stockname) %>% 
    .[1,] %>% 
    as.character() 
  sub_transactions<-transactions[transactions[,6]=="buy" & transactions[,5]==stocknam,]
  # subset transactions to stockname and buy
  tot_cost <- rep(0,nrow(tmpstock_data)); tot_cost
  tot_buyvol <- rep(0,nrow(tmpstock_data)); tot_buyvol
  # create empty vectors to be filled during the loop
  # vectors have the same length as the data that was pulled from Yahoo Finance
  
  
  # Loop 1.1 begin (1st loop within main loop)
  
  for(i in 1:nrow(sub_transactions)){ # run the loop for every buy transaction (nrow)
    tmp_cost <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[i,1]),
                       0, 
                       sub_transactions[i,3])
    # if date is before purchase, then price = 0. If date is after purchase, 
    # then price = transaction cost price (3rd col of sub_transactions)
    
    tmp_vol <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[i,1]),
                      0,
                      sub_transactions[i,2])  
    # if date is before purchase, then volume = 0. If date is after purchase, 
    # then volume = transaction volume (2nd col of sub_transactions)
    
    tot_cost <- tot_cost + tmp_cost * tmp_vol; tot_cost
    # add price * vol to the empty vector that was defined before this loop. 
    # Value increases with every iteration of this loop (i.e., with every buy of same stock)
    
    tot_buyvol <- tot_buyvol + tmp_vol; tot_buyvol
    # add vol to the empty vector that was defined before this loop. 
    # Value increases with every iteration of this loop (i.e., with every buy of same stock)
    
    tmpstock_data<-merge(tmpstock_data, tmp_cost, tmp_vol)
    # merge those dataframes to the pulled stock data
    
    colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_cost"] <- paste("buyprice", 
                                                                          i, sep = "") 
    # rename those colnames of tmp_cost by "buyprice" and the corresponding number
    
    colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_vol"] <- paste("buy_volume", 
                                                                         i, sep = "") 
    # same as above, rename by "buy_volume" and the corresponding number
  } # Loop 1.1 end
  
  tmpstock_data <- merge(tmpstock_data, tot_cost, tot_buyvol)
  # merge the created data frames with the stock data from the main loop
  
  # Now we do exactly the same, but with sell data in the next loop.
  tot_sellval <- rep(0,nrow(tmpstock_data)); tot_sellval
  tot_sellvol <- rep(0,nrow(tmpstock_data)); tot_sellvol
  
  sub_transactions<-transactions[transactions[,6]=="sell" & transactions[,5]==stocknam,]
  # subset transactions to "buyorsell" (6th col) = sell"
  
  # Loop 1.2 begin (2nd loop in main loop)
  
  for(k in 1:nrow(sub_transactions)){ 
    tmp_cost <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[k,1]), 
                       0, sub_transactions[k,3]) 
    
    tmp_vol <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[k,1]),
                      0, sub_transactions[k,2])
    
    tot_sellval <- tot_sellval + tmp_cost * tmp_vol; tot_sellval
    tot_sellvol <- tot_sellvol + tmp_vol; tot_sellvol
    tmpstock_data<-merge(tmpstock_data, tmp_cost, tmp_vol) 
    
    colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_cost"] <- paste("sellprice", 
                                                                          k, sep = "") 
    
    colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_vol"] <- paste("sell_volume", 
                                                                         k, sep = "") 
    
  } # Loop 1.2 end 
  
  tmpstock_data <- merge(tmpstock_data, tot_sellval, tot_sellvol)
  # merge the created data frames with the stock data from the main loop
  
  tmpstock_data$vol_owned <- tmpstock_data$tot_buyvol - tmpstock_data$tot_sellvol 
  # volume of stock shares currently owned = bought shares - sold shares
  tmpstock_data$val_owned <- tmpstock_data$vol_owned * tmpstock_data[,1]
  # value owned = volume * price
  tmpstock_data$sum_eq <- tmpstock_data$val_owned + tmpstock_data$tot_sellval
  # the sum of our equity is owned stock + value of sales
  tmpstock_data$gain <- tmpstock_data$val_owned + tmpstock_data$tot_sellval - 
    tmpstock_data$tot_cost
  # our gain is value owned shares + value sold shares - total cost of shares
  
  tmpstock_data$gain_perc <- (tmpstock_data$val_owned + tmpstock_data$tot_sellval -
                                tmpstock_data$tot_cost)/tmpstock_data$tot_cost*100
  # our gain % is (value owned shares + value sold shares -
  # total cost of shares) / total cost of shares
  
  assign(paste(stocknam, sep = ""), tmpstock_data)
  # save the data as an object with the name of the stock (stocknam)
} # Loop 1 end



merg_df <- get(levels(transactions[,5])[1])$gain
names(merg_df)[1] <- "to_be_deleted" # this way we get a vector of the correct length

bN <- length(levels(transactions[,5]))
for(c in 1:bN){
  tmp_merg_df <- merge(get(levels(transactions[,5])[c])$gain,
                       get(levels(transactions[,5])[c])$tot_cost)
  # merge gain and tot_cost into temp dataframe
  tmp_merg_df$gain_perc <- tmp_merg_df$gain / tmp_merg_df$tot_cost *100
  # calculate percent gain in temp dataframe
  names(tmp_merg_df)[1] <- paste(as.character(levels(transactions[,5])[c]),
                                 "gain", sep = "_")
  names(tmp_merg_df)[2] <- paste(as.character(levels(transactions[,5])[c]),
                                 "tot_cost", sep = "_")
  names(tmp_merg_df)[3] <- paste(as.character(levels(transactions[,5])[c]),
                                 "gain_perc", sep = "_")
  merg_df <- merge(tmp_merg_df,merg_df)
}

merg_df<-merg_df[,1:ncol(merg_df)-1]
# now delete the last column (I know this is ugly, but for now it has to suffice)

ncol_merg_df <- ncol(merg_df) # how many cols are in merg_df?

merg_df$grand_total_gain <- rowSums(merg_df[,seq(1,ncol_merg_df,3)],na.rm = TRUE) 
merg_df$grand_tot_cost <- rowSums(merg_df[,seq(2,ncol_merg_df,3)]) 
merg_df$grand_total_gain_perc <- rowSums(merg_df[,seq(1,ncol_merg_df,3)],na.rm = TRUE)/
  rowSums(merg_df[,seq(2,ncol_merg_df,3)],na.rm = TRUE)*100 
# calculate grand total gain = gain of all stocks.
# same principle for grand total cost and gain percentage.

merg_df_noxts <- data.frame(date=index(merg_df), coredata(merg_df))
ncol_merg_df_noxts <- ncol(merg_df_noxts)
# we need to turn xts object into non-xts object, so ggplot can read it

gain_df <-  select(merg_df_noxts,
                   names(merg_df_noxts[,c(1,seq(2,ncol_merg_df_noxts,3))]))

gain_perc_df <-  select(merg_df_noxts,
                        names(merg_df_noxts[,c(1,seq(4,ncol_merg_df_noxts,3))]))



g1 <- gather(gain_df, key = "stock", value = "gain",-date, na.rm = FALSE) 
#I have to exclude the date column, else it will be gathered too.

ggplot() +
  geom_point(data = g1[g1$stock %in% "grand_total_gain",],mapping = aes(x = date, y = gain), size = 2) +
  geom_line(data = g1, mapping = aes(x = date, y = gain, color = stock), size = 1) +
  theme(legend.position="top") +
  ylab("Absolute gain ($)") +
  scale_y_continuous(breaks = seq(-30000,50000,500), position = "right") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  scale_x_date(date_breaks = "2 months",  date_minor_breaks = "1 months")

# this is the dataset before we shortened it for plotting

g2 <- gather(gain_perc_df, key = "stock", value = "gain_percent",-date) 
#I have to exclude the date column, else it will be gathered too.

ggplot() +
  geom_point(data = g2[g2$stock %in% "grand_total_gain_perc",], mapping = aes(x = date, y = gain_percent)) +
  geom_line(data = g2, mapping = aes(x = date, y = gain_percent, color = stock), size = 1) +
  theme(legend.position="top") +  
  ylab("Gain %") +
  scale_y_continuous(breaks = seq(-100,500,5),position = "right") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  scale_x_date(date_breaks = "2 months",  date_minor_breaks = "1 months")

merg_df_noxts[merg_df_noxts[,1]=="2018-10-19",] 
merg_df_noxts[merg_df_noxts[,1]=="2018-11-21",] 
merg_df_noxts[merg_df_noxts[,1]=="2018-12-21",] 
merg_df_noxts[merg_df_noxts[,1]=="2019-01-21",] 
merg_df_noxts[merg_df_noxts[,1]=="2019-02-21",] 
merg_df_noxts[merg_df_noxts[,1]=="2019-03-21",] 

