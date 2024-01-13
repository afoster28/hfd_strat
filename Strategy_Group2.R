# setting the working directory if needed
setwd("C:\\Users\\afost\\Documents\\UW\\Y2\\S1\\Quantitative Strategies High Frequency Data\\hfd_strat")

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs

# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

# mySR function
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} # end of definition



# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')

# do it simply in a loop on quarters

for (selected_quarter in c("2021_Q1", "2021_Q3", "2021_Q4", 
                           "2022_Q2", "2022_Q4", 
                           "2023_Q1", "2023_Q2")) {
  
  selected_quarter <- "2021_Q1"
  
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  filename_ <- paste0("data/data2_", selected_quarter, ".RData")
  
  load(filename_)
  
  data.group2 <- get(paste0("data2_", selected_quarter))
  
  times_ <- substr(index(data.group2), 12, 19)
  
  # Keep gold and silver
  data.group2 <- data.group2[, !colnames(data.group2) %in% c("AUD","CAD")]
  names(data.group2)[1:2] <- c("XAG.close","XAU.close")
  
  data.group2.return <- 10000*diff.xts(log(data.group2))
  names(data.group2.return)[1:2] <- c("XAG.return","XAU.return")
  
  data.group2 <- merge(data.group2[, c("XAG.close", "XAU.close")],
                       data.group2.return[, c("XAG.return", "XAU.return")])
  
  myTheme <- chart_theme()
  myTheme$col$line.col <- "darkblue"
  
  layout(matrix(1:4, 2, 2))
  chart_Series(data.group2$XAG.close, theme = myTheme)
  chart_Series(data.group2$XAU.close, theme = myTheme)
  chart_Series(data.group2$XAG.return, theme = myTheme)
  chart_Series(data.group2$XAU.return, theme = myTheme)
  layout(matrix(1))
  
  # the following common assumptions were defined:
  # 1.	do not use in calculations the data from the first and last 10 minutes of the session (18:01--18:10 and 16:51--17:00) – put missing values there,
  
  # lets put missing values for these periods
  data.group2["T18:01/T18:10",] <- NA 
  data.group2["T16:51/T17:00",] <- NA
  
  layout(matrix(1:4, 2, 2))
  chart_Series(data.group2$XAG.close, theme = myTheme)
  chart_Series(data.group2$XAU.close, theme = myTheme)
  chart_Series(data.group2$XAG.return, theme = myTheme)
  chart_Series(data.group2$XAU.return, theme = myTheme)
  layout(matrix(1))
  
  
  
  
  
  
  
  
  
  
  
  
  
  # lets calculate EMA10 and EMA60 for all series
  data.group2$XAG_EMA10 <- EMA(na.locf(data.group2$XAG), 10)
  data.group2$XAG_EMA60 <- EMA(na.locf(data.group2$XAG), 60)
  data.group2$XAU_EMA10 <- EMA(na.locf(data.group2$XAU), 10)
  data.group2$XAU_EMA60 <- EMA(na.locf(data.group2$XAU), 60)
  
  # put missing value whenever the original price is missing
  data.group2$XAG_EMA10[is.na(data.group2$XAG)] <- NA
  data.group2$XAG_EMA60[is.na(data.group2$XAG)] <- NA
  data.group2$XAU_EMA10[is.na(data.group2$XAU)] <- NA
  data.group2$XAU_EMA60[is.na(data.group2$XAU)] <- NA
  
  # lets calculate the position for the MOMENTUM strategy
  # for each asset separately
  # if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
  # if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
  #  caution! this strategy is always in the market !
  
  data.group2$position.XAG.mom <- ifelse(lag.xts(data.group2$XAG_EMA10) >
                                           lag.xts(data.group2$XAG_EMA60),
                                         1, -1)
  
  data.group2$position.XAU.mom <- ifelse(lag.xts(data.group2$XAU_EMA10) >
                                           lag.xts(data.group2$XAU_EMA60),
                                         1, -1)
  
  
  # lets apply the remaining assumptions
  # - exit all positions 15 minutes before the session end, i.e. at 16:45
  # - do not trade within the first 15 minutes after the break (until 18:15)
  
  data.group2$position.XAG.mom[times(times_) > times("16:45:00") &
                                 times(times_) <= times("18:15:00")] <- 0
  
  data.group2$position.XAU.mom[times(times_) > times("16:45:00") &
                                 times(times_) <= times("18:15:00")] <- 0
  
  
  # lets also fill every missing position with the previous one
  data.group2$position.XAG.mom <- na.locf(data.group2$position.XAG.mom, na.rm = FALSE)
  data.group2$position.XAU.mom <- na.locf(data.group2$position.XAU.mom, na.rm = FALSE)
  
  
  # calculating gross pnl - remember to multiply by the point value !!!!
  data.group2$pnl_gross.XAU.mom <- data.group2$position.XAU.mom * diff.xts(data.group2$XAU) * 100
  data.group2$pnl_gross.XAG.mom <- data.group2$position.XAG.mom * diff.xts(data.group2$XAG) * 5000
  
  # number of transactions
  
  data.group2$ntrans.XAG.mom <- abs(diff.xts(data.group2$position.XAG.mom))
  data.group2$ntrans.XAG.mom[1] <- 0
  
  data.group2$ntrans.XAU.mom <- abs(diff.xts(data.group2$position.XAU.mom))
  data.group2$ntrans.XAU.mom[1] <- 0
  
  # net pnl
  data.group2$pnl_net.XAG.mom <- data.group2$pnl_gross.XAG.mom  -
    data.group2$ntrans.XAG.mom * 7 # 7$ per transaction
  
  data.group2$pnl_net.XAU.mom <- data.group2$pnl_gross.XAU.mom  -
    data.group2$ntrans.XAU.mom * 12 # 12$ per transaction
  
  
  # aggregate pnls and number of transactions to daily
  my.endpoints <- endpoints(data.group2, "days")
  
  data.group2.daily <- period.apply(data.group2[,c(grep("pnl", names(data.group2)),
                                                   grep("ntrans", names(data.group2)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # lets SUM gross and net pnls
  
  data.group2.daily$pnl_gross.mom <- 
    data.group2.daily$pnl_gross.XAU.mom +
    data.group2.daily$pnl_gross.XAG.mom
  
  data.group2.daily$pnl_net.mom <- 
    data.group2.daily$pnl_net.XAU.mom +
    data.group2.daily$pnl_net.XAG.mom
  
  # lets SUM number of transactions (with the same weights)
  
  data.group2.daily$ntrans.mom <- 
    data.group2.daily$ntrans.XAG.mom +
    data.group2.daily$ntrans.XAU.mom
  
  
  # summarize the strategy for this quarter
  
  # SR
  grossSR = mySR(x = data.group2.daily$pnl_gross.mom, scale = 252)
  netSR = mySR(x = data.group2.daily$pnl_net.mom, scale = 252)
  # CR
  grossCR = myCalmarRatio(x = data.group2.daily$pnl_gross.mom, scale = 252)
  netCR = myCalmarRatio(x = data.group2.daily$pnl_net.mom, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group2.daily$ntrans.mom, 
                          na.rm = TRUE)
  # PnL
  grossPnL = sum(data.group2.daily$pnl_gross.mom)
  netPnL = sum(data.group2.daily$pnl_net.mom)
  
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 2,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE
  )
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(data.group2.daily$pnl_gross.mom),
               cumsum(data.group2.daily$pnl_net.mom)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  dev.off()
  
  # remove all unneeded objects for group 2
  rm(data.group2, my.endpoints, grossSR, netSR, av.daily.ntrades,
     grossPnL, netPnL, stat, quarter_stats, data.group2.daily)
  
  gc()
  
  
} # end of the loop

write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = FALSE)

