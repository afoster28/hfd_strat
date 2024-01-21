# Set the working directory and quarters for run

setwd("C:\\Users\\afost\\Documents\\UW\\Y2\\S1\\Quantitative Strategies High Frequency Data\\hfd_strat")

selected_quarters <- c("2021_Q1", "2021_Q3", "2021_Q4", 
  "2022_Q2", "2022_Q4", 
  "2023_Q1", "2023_Q2")
# Make sure to store all quarterly data in a subfolder called 'data'

# Load libraries, functions set system settings

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr)
library(kableExtra)
library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)

options(scipen=999)
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ = 'America/New_York')

mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
}

plotHeatmap <- function(data_plot, # dataset (data.frame) with calculations
                        col_vlabels, # column name with the labels for a vertical axis (string)
                        col_hlabels, # column name with the labels for a horizontal axis (string)
                        col_variable, # column name with the variable to show (string)
                        main,      # title
                        label_size = 6, # size of labels
                        save_graph = FALSE, # whether to save the graph
                        width = 12,
                        height = 8,
                        file_name = NULL) { # filename for saving
  
  require(ggplot2)
  require(dplyr)
  
  
  data_plot$labels_ <- round(data_plot[, col_variable], 2)
  data_plot[, col_hlabels] <- as.factor(data_plot[, col_hlabels])
  data_plot[, col_vlabels] <- as.factor(data_plot[, col_vlabels])
  
  
  p1 <- ggplot(data_plot, 
               aes_string(x = col_hlabels, 
                          y = col_vlabels)) +
    geom_raster(aes_string(fill = col_variable)) +
    theme_bw() +
    xlab(col_hlabels) +
    ylab(col_vlabels) +
    ggtitle(main) +
    scale_fill_gradient2(low = "red",
                         high = "darkgreen",
                         mid = "white",
                         midpoint = 0) +
    geom_label(aes_string(label = "labels_"),
               size = label_size) +
    theme(legend.position = "bottom", 
          legend.key.width = unit(2, "cm"))
  
  if(save_graph) { 
    if(is.null(file_name)) stop("Please provide the file_name= argument") else
      ggsave(filename = file_name, 
             plot = p1, 
             units = "in",
             width = width, 
             height = height)
  }
  
  return(p1)
}

#-------------------------------------------------------------
# Group 1

heatmap_list_single <- list()
heatmap_list_cross <- list()
heatmap_list_single_mr <- list()
heatmap_list_cross_mr <- list()
sensitivities_single <- list()
sensitivities_cross <- list()
sensitivities_single_mr <- list()
sensitivities_cross_mr <- list()

for (selected_quarter in selected_quarters) {
  
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  filename_ <- paste0("data/data1_", selected_quarter, ".RData")
  
  load(filename_)
  
  # create index of times for this quarter
  
  data.group1 <- get(paste0("data1_", selected_quarter))
  
  times_ <- substr(index(data.group1), 12, 19)
  
  # Keep S&P500
  data.group1 <- data.group1[, !colnames(data.group1) %in% c("NQ")]
  
  # the following common assumptions were defined:
  # 1.	do not use in calculations the data from the first 
  # and last 10 minutes of the session (9:31--9:40 and 15:51--16:00)
  # – put missing values there,
  
  # lets put missing values for these periods
  data.group1["T09:31/T09:40",] <- NA 
  data.group1["T15:51/T16:00",] <-NA
  
  myTheme <- chart_theme()
  myTheme$col$line.col <- "darkblue"
  layout(matrix(1:1, 1, 1))
  chart_Series(data.group1$SP, theme = myTheme)
  layout(matrix(1))
  
  
  # Momentum
  
  # Single EMA
  
  EMA_pairs = list(c(1, 10), c(1, 20), c(1, 30), c(1, 40), c(1, 50), c(1, 60), c(1, 70), c(1, 80))
  data.group1a <- data.group1
  
  for (pair in EMA_pairs) {
    # lets calculate EMAfast and EMAslow for SP
    data.group1a$SP_EMA <- EMA(na.locf(data.group1a$SP), pair[2])
    
    # put missing value whenever the original price is missing
    data.group1a$SP_EMA[is.na(data.group1a$SP)] <- NA
    
    # lets calculate the position for the MOMENTUM strategy
    # if price(t-1) > MA(t-1) => pos(t) = 1 [long]
    # if price(t-1) <= MA(t-1) => pos(t) = -1 [short]
    #  caution! this strategy is always in the market !
    data.group1a$positionSP.mom <- ifelse(lag.xts(data.group1a$SP) >
                                            lag.xts(data.group1a$SP_EMA),
                                          1, -1)
    
    # lets apply the remaining assumptions
    # - exit all positions 20 minutes before the session end, i.e. at 15:40
    # - do not trade within the first 25 minutes of stocks quotations (until 9:55)
    data.group1a$positionSP.mom[times(times_) <= times("09:55:00") | 
                                  times(times_) > times("15:40:00")] <- 0
    
    
    # lets also fill every missing position with the previous one
    
    data.group1a$positionSP.mom <- na.locf(data.group1a$positionSP.mom, na.rm = FALSE)
    
    # calculating gross pnl
    
    data.group1a$pnl_grossSP.mom <- data.group1a$positionSP.mom * diff.xts(data.group1a$SP) * 50
    
    
    # number of transactions
    data.group1a$ntransSP.mom <- abs(diff.xts(data.group1a$positionSP.mom))
    
    data.group1a$ntransSP.mom[1] <- 0
    
    # net pnl
    data.group1a$pnl_netSP.mom <- data.group1a$pnl_grossSP.mom  -
      data.group1a$ntransSP.mom * 10 # 10$ per transaction
    
    # total for strategy
    
    data.group1a$pnl_gross.mom <- data.group1a$pnl_grossSP.mom
    data.group1a$pnl_net.mom <- data.group1a$pnl_netSP.mom
    
    
    # aggregate pnls and number of transactions to daily
    my.endpoints <- endpoints(data.group1a, "days")
    
    data.group1a.daily <- period.apply(data.group1a[,c(grep("pnl", names(data.group1a)),
                                                       grep("ntrans", names(data.group1a)))],
                                       INDEX = my.endpoints, 
                                       FUN = function(x) colSums(x, na.rm = TRUE))
    
    # summarize the strategy for this quarter
    
    # SR
    grossSR = mySR(x = data.group1a.daily$pnl_gross.mom, scale = 252)
    netSR = mySR(x = data.group1a.daily$pnl_net.mom, scale = 252)
    # CR
    grossCR = myCalmarRatio(x = data.group1a.daily$pnl_gross.mom, scale = 252)
    netCR = myCalmarRatio(x = data.group1a.daily$pnl_net.mom, scale = 252)
    
    # average number of transactions
    av.daily.ntrades = mean(data.group1a.daily$ntransSP.mom, na.rm = TRUE)
    # PnL
    grossPnL = sum(data.group1a.daily$pnl_gross.mom)
    netPnL = sum(data.group1a.daily$pnl_net.mom)
    # stat
    stat = netCR * max(0, log(abs(netPnL/1000)))
    
    # summary of a particular strategy
    summary_ <- data.frame(Close = 1,
                           EMA = pair[2],
                           period = selected_quarter, # "2016-08-16 - 2016-11",
                           gross.SR = grossSR,
                           net.SR = netSR,
                           gross.PnL = grossPnL,
                           net.PnL = netPnL,
                           av.daily.ntrans = av.daily.ntrades,
                           stringsAsFactors = FALSE)
    
    # putting all summaries together
    
    if(!exists("summary.pair.trading")) summary.pair.trading <- summary_ else
      summary.pair.trading <- rbind(summary.pair.trading, summary_)
    
    # deleting working files not needed any more
    rm(grossSR, netSR, netCR,
       grossPnL, netPnL,
       av.daily.ntrades, stat,
       summary_)
  }
  
  # net.SR - spread av_ratio
  heatmap_sr_single <- plotHeatmap(data_plot = summary.pair.trading, # dataset (data.frame) with calculations
                                   col_vlabels = "Close", # column name with the labels for a vertical axis (string)
                                   col_hlabels = "EMA", # column name with the labels for a horizontal axis (string)
                                   col_variable = "net.SR", # column name with the variable to show (string)
                                   main = paste(selected_quarter, "Sensitivity analysis for momentum stategy based on single EMA", sep = ": "),
                                   label_size = 3)
  
  sensitivities_single[[selected_quarter]] <- summary.pair.trading
  rm(summary.pair.trading)
  heatmap_list_single[[selected_quarter]] <- heatmap_sr_single
  
  
  # EMA Crossover
  
  EMA_pairs = list(c(10, 20), c(10, 30), c(10, 40), c(10, 50), c(10, 60), c(10, 70), c(10, 80),
                   c(20, 30), c(20, 40), c(20, 50), c(20, 60), c(20, 70), c(20, 80),
                   c(30, 40), c(30, 50), c(30, 60), c(30, 70), c(30, 80),
                   c(40, 50), c(40, 60), c(40, 70), c(40, 80),
                   c(50, 60), c(50, 70), c(50, 80),
                   c(60, 70), c(60, 80),
                   c(70, 80))
  
  data.group1b <- data.group1
  
  # pair <- c(60, 70)
  for (pair in EMA_pairs) {
    # lets calculate EMAfast and EMAslow for SP
    data.group1b$SP_EMAfast <- EMA(na.locf(data.group1b$SP), pair[1])
    data.group1b$SP_EMAslow <- EMA(na.locf(data.group1b$SP), pair[2])
    
    # put missing value whenever the original price is missing
    data.group1b$SP_EMAfast[is.na(data.group1b$SP)] <- NA
    data.group1b$SP_EMAslow[is.na(data.group1b$SP)] <- NA
    
    # lets calculate the position for the MOMENTUM strategy
    # if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
    # if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
    #  caution! this strategy is always in the market !
    data.group1b$positionSP.mom <- ifelse(lag.xts(data.group1b$SP_EMAfast) >
                                            lag.xts(data.group1b$SP_EMAslow),
                                          1, -1)
    
    # lets apply the remaining assumptions
    # - exit all positions 20 minutes before the session end, i.e. at 15:40
    # - do not trade within the first 25 minutes of stocks quotations (until 9:55)
    data.group1b$positionSP.mom[times(times_) <= times("09:55:00") | 
                                  times(times_) > times("15:40:00")] <- 0
    
    
    # lets also fill every missing position with the previous one
    
    data.group1b$positionSP.mom <- na.locf(data.group1b$positionSP.mom, na.rm = FALSE)
    
    # calculating gross pnl
    
    data.group1b$pnl_grossSP.mom <- data.group1b$positionSP.mom * diff.xts(data.group1b$SP) * 50
    
    
    # number of transactions
    data.group1b$ntransSP.mom <- abs(diff.xts(data.group1b$positionSP.mom))
    
    data.group1b$ntransSP.mom[1] <- 0
    
    # net pnl
    data.group1b$pnl_netSP.mom <- data.group1b$pnl_grossSP.mom  -
      data.group1b$ntransSP.mom * 10 # 10$ per transaction
    
    # total for strategy
    
    data.group1b$pnl_gross.mom <- data.group1b$pnl_grossSP.mom
    data.group1b$pnl_net.mom <- data.group1b$pnl_netSP.mom
    
    
    # aggregate pnls and number of transactions to daily
    my.endpoints <- endpoints(data.group1b, "days")
    
    data.group1b.daily <- period.apply(data.group1b[,c(grep("pnl", names(data.group1b)),
                                                       grep("ntrans", names(data.group1b)))],
                                       INDEX = my.endpoints, 
                                       FUN = function(x) colSums(x, na.rm = TRUE))
    
    # summarize the strategy for this quarter
    
    # SR
    grossSR = mySR(x = data.group1b.daily$pnl_gross.mom, scale = 252)
    netSR = mySR(x = data.group1b.daily$pnl_net.mom, scale = 252)
    # CR
    grossCR = myCalmarRatio(x = data.group1b.daily$pnl_gross.mom, scale = 252)
    netCR = myCalmarRatio(x = data.group1b.daily$pnl_net.mom, scale = 252)
    
    # average number of transactions
    av.daily.ntrades = mean(data.group1b.daily$ntransSP.mom, na.rm = TRUE)
    # PnL
    grossPnL = sum(data.group1b.daily$pnl_gross.mom)
    netPnL = sum(data.group1b.daily$pnl_net.mom)
    # stat
    stat = netCR * max(0, log(abs(netPnL/1000)))
    
    #selected_quarter = "2021_Q1"
    #pair <- c(60, 70)
    # collecting all statistics for a particular quarter
    if(pair[1] == 60 & pair[2] == 70) {
      quarter_stats <- data.frame(quarter = selected_quarter,
                                  assets.group = 1,
                                  gross.SR = grossSR,
                                  net.SR = netSR,
                                  gross.CR = grossCR,
                                  net.CR = netCR,
                                  gross.PnL = grossPnL,
                                  net.PnL = netPnL,
                                  av.daily.ntrans = av.daily.ntrades,
                                  stat,
                                  stringsAsFactors = FALSE
      )
      
      # collect summaries for all quarters
      if(!exists("quarter_stats.all.group1")) quarter_stats.all.group1 <- quarter_stats else
        quarter_stats.all.group1 <- rbind(quarter_stats.all.group1, quarter_stats)
      
      # create a plot of gros and net pnl and save it to png file
      png(filename = paste0("pnl_group1_", selected_quarter, ".png"),
          width = 1000, height = 600)
      
      print( # when plotting in a loop you have to use print()
        plot(cbind(cumsum(data.group1b.daily$pnl_gross.mom),
                   cumsum(data.group1b.daily$pnl_net.mom)),
             multi.panel = FALSE,
             main = paste0("Gross and net PnL for asset group 1 \n quarter ", selected_quarter), 
             col = c("#377EB8", "#E41A1C"),
             major.ticks = "weeks", 
             grid.ticks.on = "weeks",
             grid.ticks.lty = 3,
             legend.loc = "topleft",
             cex = 1)
      )
      # closing the png device (and file)
      dev.off()
      
      # remove all unneeded objects for group 1
      rm(pnl.gross.d, pnl.net.d, quarter_stats)
      
      gc()
      
      # # create a plot of gros and net pnl and save it to png file
      # pnl.gross.d <- data.group1b.daily$pnl_gross.mom
      # pnl.net.d <- data.group1b.daily$pnl_net.mom
      # 
      # y_range <- range(c(cumsum(pnl.gross.d), cumsum(pnl.net.d)))
      # 
      # png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
      #     width = 1000, height = 600)
      # print( # when plotting in a loop you have to use print()
      #   plot(cumsum(pnl.gross.d),
      #        type = "l",
      #        main = paste0("Gross and net PnL for asset group 1 \n quarter ", selected_quarter),
      #        col = "#377EB8",
      #        xlab = "Time",
      #        ylab = "Cumulative PnL",
      #        ylim = y_range
      #   )
      # )
      # lines(cumsum(pnl.net.d), col = "#E41A1C")
      # legend("topleft", legend = c("Gross PnL", "Net PnL"), col = c("#377EB8", "#E41A1C"), lty = 1, cex = 1)
      # dev.off()
      # 
      # # remove all unneeded objects for group 1
      # rm(pnl.gross.d, pnl.net.d, quarter_stats)
    }
    
    # summary of a particular strategy
    summary_ <- data.frame(EMA.fast = pair[1],
                           EMA.slow = pair[2],
                           period = selected_quarter, # "2016-08-16 - 2016-11",
                           gross.SR = grossSR,
                           net.SR = netSR,
                           gross.PnL = grossPnL,
                           net.PnL = netPnL,
                           av.daily.ntrans = av.daily.ntrades,
                           stringsAsFactors = FALSE)
    
    # putting all summaries together
    
    if(!exists("summary.pair.trading")) summary.pair.trading <- summary_ else
      summary.pair.trading <- rbind(summary.pair.trading, summary_)
    
    # deleting working files not needed any more
    rm(grossSR, netSR, netCR,
       grossPnL, netPnL,
       av.daily.ntrades, stat,
       summary_)
  }
  
  # net.SR - spread av_ratio
  heatmap_sr_cross <- plotHeatmap(data_plot = summary.pair.trading, # dataset (data.frame) with calculations
                                  col_vlabels = "EMA.fast", # column name with the labels for a vertical axis (string)
                                  col_hlabels = "EMA.slow", # column name with the labels for a horizontal axis (string)
                                  col_variable = "net.SR", # column name with the variable to show (string)
                                  main = paste(selected_quarter, "Sensitivity analysis for momentum stategy based on EMA crossover", sep = ": "),
                                  label_size = 3)
  
  sensitivities_cross[[selected_quarter]] <- summary.pair.trading
  rm(summary.pair.trading)
  heatmap_list_cross[[selected_quarter]] <- heatmap_sr_cross
  
  
  # Mean reversion
  
  # Single EMA
  
  EMA_pairs = list(c(1, 10), c(1, 20), c(1, 30), c(1, 40), c(1, 50), c(1, 60), c(1, 70), c(1, 80))
  data.group1a_mr <- data.group1
  
  for (pair in EMA_pairs) {
    # lets calculate EMAfast and EMAslow for SP
    data.group1a_mr$SP_EMA <- EMA(na.locf(data.group1a_mr$SP), pair[2])
    
    # put missing value whenever the original price is missing
    data.group1a_mr$SP_EMA[is.na(data.group1a_mr$SP)] <- NA
    
    # lets calculate the position for the MOMENTUM strategy
    # if price(t-1) > MA(t-1) => pos(t) = 1 [long]
    # if price(t-1) <= MA(t-1) => pos(t) = -1 [short]
    #  caution! this strategy is always in the market !
    data.group1a_mr$positionSP.mr <- ifelse(lag.xts(data.group1a_mr$SP) >
                                              lag.xts(data.group1a_mr$SP_EMA),
                                            -1, 1)
    
    # lets apply the remaining assumptions
    # - exit all positions 20 minutes before the session end, i.e. at 15:40
    # - do not trade within the first 25 minutes of stocks quotations (until 9:55)
    data.group1a_mr$positionSP.mr[times(times_) <= times("09:55:00") | 
                                    times(times_) > times("15:40:00")] <- 0
    
    
    # lets also fill every missing position with the previous one
    
    data.group1a_mr$positionSP.mr <- na.locf(data.group1a_mr$positionSP.mr, na.rm = FALSE)
    
    # calculating gross pnl
    
    data.group1a_mr$pnl_grossSP.mr <- data.group1a_mr$positionSP.mr * diff.xts(data.group1a_mr$SP) * 50
    
    
    # number of transactions
    data.group1a_mr$ntransSP.mr <- abs(diff.xts(data.group1a_mr$positionSP.mr))
    
    data.group1a_mr$ntransSP.mr[1] <- 0
    
    # net pnl
    data.group1a_mr$pnl_netSP.mr <- data.group1a_mr$pnl_grossSP.mr  -
      data.group1a_mr$ntransSP.mr * 10 # 10$ per transaction
    
    # total for strategy
    
    data.group1a_mr$pnl_gross.mr <- data.group1a_mr$pnl_grossSP.mr
    data.group1a_mr$pnl_net.mr <- data.group1a_mr$pnl_netSP.mr
    
    
    # aggregate pnls and number of transactions to daily
    my.endpoints <- endpoints(data.group1a_mr, "days")
    
    data.group1a_mr.daily <- period.apply(data.group1a_mr[,c(grep("pnl", names(data.group1a_mr)),
                                                             grep("ntrans", names(data.group1a_mr)))],
                                          INDEX = my.endpoints, 
                                          FUN = function(x) colSums(x, na.rm = TRUE))
    
    # summarize the strategy for this quarter
    
    # SR
    grossSR = mySR(x = data.group1a_mr.daily$pnl_gross.mr, scale = 252)
    netSR = mySR(x = data.group1a_mr.daily$pnl_net.mr, scale = 252)
    # CR
    grossCR = myCalmarRatio(x = data.group1a_mr.daily$pnl_gross.mr, scale = 252)
    netCR = myCalmarRatio(x = data.group1a_mr.daily$pnl_net.mr, scale = 252)
    
    # average number of transactions
    av.daily.ntrades = mean(data.group1a_mr.daily$ntransSP.mr, na.rm = TRUE)
    # PnL
    grossPnL = sum(data.group1a_mr.daily$pnl_gross.mr)
    netPnL = sum(data.group1a_mr.daily$pnl_net.mr)
    # stat
    stat = netCR * max(0, log(abs(netPnL/1000)))
    
    # summary of a particular strategy
    summary_ <- data.frame(Close = 1,
                           EMA = pair[2],
                           period = selected_quarter, # "2016-08-16 - 2016-11",
                           gross.SR = grossSR,
                           net.SR = netSR,
                           gross.PnL = grossPnL,
                           net.PnL = netPnL,
                           av.daily.ntrans = av.daily.ntrades,
                           stringsAsFactors = FALSE)
    
    # putting all summaries together
    
    if(!exists("summary.pair.trading")) summary.pair.trading <- summary_ else
      summary.pair.trading <- rbind(summary.pair.trading, summary_)
    
    # deleting working files not needed any more
    rm(grossSR, netSR, netCR,
       grossPnL, netPnL,
       av.daily.ntrades, stat,
       summary_)
  }
  
  # net.SR - spread av_ratio
  heatmap_sr_single_mr <- plotHeatmap(data_plot = summary.pair.trading, # dataset (data.frame) with calculations
                                      col_vlabels = "Close", # column name with the labels for a vertical axis (string)
                                      col_hlabels = "EMA", # column name with the labels for a horizontal axis (string)
                                      col_variable = "net.SR", # column name with the variable to show (string)
                                      main = paste(selected_quarter, "Sensitivity analysis for mean reversion stategy based on single EMA", sep = ": "),
                                      label_size = 3)
  
  sensitivities_single_mr[[selected_quarter]] <- summary.pair.trading
  rm(summary.pair.trading)
  heatmap_list_single_mr[[selected_quarter]] <- heatmap_sr_single_mr
  
  
  # EMA Crossover
  
  EMA_pairs = list(c(10, 20), c(10, 30), c(10, 40), c(10, 50), c(10, 60), c(10, 70), c(10, 80),
                   c(20, 30), c(20, 40), c(20, 50), c(20, 60), c(20, 70), c(20, 80),
                   c(30, 40), c(30, 50), c(30, 60), c(30, 70), c(30, 80),
                   c(40, 50), c(40, 60), c(40, 70), c(40, 80),
                   c(50, 60), c(50, 70), c(50, 80),
                   c(60, 70), c(60, 80),
                   c(70, 80))
  
  data.group1b_mr <- data.group1
  
  for (pair in EMA_pairs) {
    # lets calculate EMAfast and EMAslow for SP
    data.group1b_mr$SP_EMAfast <- EMA(na.locf(data.group1b_mr$SP), pair[1])
    data.group1b_mr$SP_EMAslow <- EMA(na.locf(data.group1b_mr$SP), pair[2])
    
    # put missing value whenever the original price is missing
    data.group1b_mr$SP_EMAfast[is.na(data.group1b_mr$SP)] <- NA
    data.group1b_mr$SP_EMAslow[is.na(data.group1b_mr$SP)] <- NA
    
    # lets calculate the position for the MOMENTUM strategy
    # if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
    # if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
    #  caution! this strategy is always in the market !
    data.group1b_mr$positionSP.mr <- ifelse(lag.xts(data.group1b_mr$SP_EMAfast) >
                                              lag.xts(data.group1b_mr$SP_EMAslow),
                                            -1, 1)
    
    # lets apply the remaining assumptions
    # - exit all positions 20 minutes before the session end, i.e. at 15:40
    # - do not trade within the first 25 minutes of stocks quotations (until 9:55)
    data.group1b_mr$positionSP.mr[times(times_) <= times("09:55:00") | 
                                    times(times_) > times("15:40:00")] <- 0
    
    
    # lets also fill every missing position with the previous one
    
    data.group1b_mr$positionSP.mr <- na.locf(data.group1b_mr$positionSP.mr, na.rm = FALSE)
    
    # calculating gross pnl
    
    data.group1b_mr$pnl_grossSP.mr <- data.group1b_mr$positionSP.mr * diff.xts(data.group1b_mr$SP) * 50
    
    
    # number of transactions
    data.group1b_mr$ntransSP.mr <- abs(diff.xts(data.group1b_mr$positionSP.mr))
    
    data.group1b_mr$ntransSP.mr[1] <- 0
    
    # net pnl
    data.group1b_mr$pnl_netSP.mr <- data.group1b_mr$pnl_grossSP.mr  -
      data.group1b_mr$ntransSP.mr * 10 # 10$ per transaction
    
    # total for strategy
    
    data.group1b_mr$pnl_gross.mr <- data.group1b_mr$pnl_grossSP.mr
    data.group1b_mr$pnl_net.mr <- data.group1b_mr$pnl_netSP.mr
    
    
    # aggregate pnls and number of transactions to daily
    my.endpoints <- endpoints(data.group1b_mr, "days")
    
    data.group1b_mr.daily <- period.apply(data.group1b_mr[,c(grep("pnl", names(data.group1b_mr)),
                                                             grep("ntrans", names(data.group1b_mr)))],
                                          INDEX = my.endpoints, 
                                          FUN = function(x) colSums(x, na.rm = TRUE))
    
    # summarize the strategy for this quarter
    
    # SR
    grossSR = mySR(x = data.group1b_mr.daily$pnl_gross.mr, scale = 252)
    netSR = mySR(x = data.group1b_mr.daily$pnl_net.mr, scale = 252)
    # CR
    grossCR = myCalmarRatio(x = data.group1b_mr.daily$pnl_gross.mr, scale = 252)
    netCR = myCalmarRatio(x = data.group1b_mr.daily$pnl_net.mr, scale = 252)
    
    # average number of transactions
    av.daily.ntrades = mean(data.group1b_mr.daily$ntransSP.mr, na.rm = TRUE)
    # PnL
    grossPnL = sum(data.group1b_mr.daily$pnl_gross.mr)
    netPnL = sum(data.group1b_mr.daily$pnl_net.mr)
    # stat
    stat = netCR * max(0, log(abs(netPnL/1000)))
    
    # summary of a particular strategy
    summary_ <- data.frame(EMA.fast = pair[1],
                           EMA.slow = pair[2],
                           period = selected_quarter, # "2016-08-16 - 2016-11",
                           gross.SR = grossSR,
                           net.SR = netSR,
                           gross.PnL = grossPnL,
                           net.PnL = netPnL,
                           av.daily.ntrans = av.daily.ntrades,
                           stringsAsFactors = FALSE)
    
    # putting all summaries together
    
    if(!exists("summary.pair.trading")) summary.pair.trading <- summary_ else
      summary.pair.trading <- rbind(summary.pair.trading, summary_)
    
    # deleting working files not needed any more
    rm(grossSR, netSR, netCR,
       grossPnL, netPnL,
       av.daily.ntrades, stat,
       summary_)
  }
  
  # net.SR - spread av_ratio
  heatmap_sr_cross_mr <- plotHeatmap(data_plot = summary.pair.trading, # dataset (data.frame) with calculations
                                     col_vlabels = "EMA.fast", # column name with the labels for a vertical axis (string)
                                     col_hlabels = "EMA.slow", # column name with the labels for a horizontal axis (string)
                                     col_variable = "net.SR", # column name with the variable to show (string)
                                     main = paste(selected_quarter, "Sensitivity analysis for mean reversion stategy based on EMA crossover", sep = ": "),
                                     label_size = 3)
  
  sensitivities_cross_mr[[selected_quarter]] <- summary.pair.trading
  rm(summary.pair.trading)
  heatmap_list_cross_mr[[selected_quarter]] <- heatmap_sr_cross_mr
}

# All heatmaps
combined_plot_single <- plot_grid(plotlist = heatmap_list_single, ncol = 1, align = 'v')
ggsave("combined_heatmaps_single.png", combined_plot_single, width = 8, height = 48)

combined_plot_cross <- plot_grid(plotlist = heatmap_list_cross, ncol = 1, align = 'v')
ggsave("combined_heatmaps_cross.png", combined_plot_cross, width = 8, height = 48)

combined_plot_single_mr <- plot_grid(plotlist = heatmap_list_single_mr, ncol = 1, align = 'v')
ggsave("combined_heatmaps_single_mr.png", combined_plot_single_mr, width = 8, height = 48)

combined_plot_cross_mr <- plot_grid(plotlist = heatmap_list_cross_mr, ncol = 1, align = 'v')
ggsave("combined_heatmaps_cross_mr.png", combined_plot_cross_mr, width = 8, height = 48)

# Mean heatmap
net_srs_single <- list()

for(i in 1:length(sensitivities_single)) {
  net_srs_single[[i]] <- as.list(sensitivities_single[[i]][c("net.SR")])[[1]]
}

average_net_sr_single <- sapply(seq_along(net_srs_single[[1]]), function(i) {
  mean(sapply(net_srs_single, function(x) x[[i]]))
})
average_net_sr_single <- data.frame(net.SR = average_net_sr_single)

sensitivities_average_single <- sensitivities_single[[1]][c("Close", "EMA")]
sensitivities_average_single <- cbind(sensitivities_average_single, "net.SR" = average_net_sr_single)

heatmap_sr_mean_single <- plotHeatmap(data_plot = sensitivities_average_single, # dataset (data.frame) with calculations
                                      col_vlabels = "Close", # column name with the labels for a vertical axis (string)
                                      col_hlabels = "EMA", # column name with the labels for a horizontal axis (string)
                                      col_variable = "net.SR", # column name with the variable to show (string)
                                      main = paste("Mean", "Sensitivity analysis for momentum stategy based on single EMA", sep = ": "),
                                      label_size = 3)

heatmap_sr_mean_single
ggsave("heatmap_sr_mean_single.png", heatmap_sr_mean_single, width = 8, height = 6)

net_srs_cross <- list()

for(i in 1:length(sensitivities_cross)) {
  net_srs_cross[[i]] <- as.list(sensitivities_cross[[i]][c("net.SR")])[[1]]
}

average_net_sr_cross <- sapply(seq_along(net_srs_cross[[1]]), function(i) {
  mean(sapply(net_srs_cross, function(x) x[[i]]))
})
average_net_sr_cross <- data.frame(net.SR = average_net_sr_cross)

sensitivities_average_cross <- sensitivities_cross[[1]][c("EMA.fast", "EMA.slow")]
sensitivities_average_cross <- cbind(sensitivities_average_cross, "net.SR" = average_net_sr_cross)

heatmap_sr_mean_cross <- plotHeatmap(data_plot = sensitivities_average_cross, # dataset (data.frame) with calculations
                                     col_vlabels = "EMA.fast", # column name with the labels for a vertical axis (string)
                                     col_hlabels = "EMA.slow", # column name with the labels for a horizontal axis (string)
                                     col_variable = "net.SR", # column name with the variable to show (string)
                                     main = paste("Mean", "Sensitivity analysis for momentum stategy based on EMA crossover", sep = ": "),
                                     label_size = 3)

heatmap_sr_mean_cross
ggsave("heatmap_sr_mean_cross.png", heatmap_sr_mean_cross, width = 8, height = 6)

net_srs_single_mr <- list()

for(i in 1:length(sensitivities_single_mr)) {
  net_srs_single_mr[[i]] <- as.list(sensitivities_single_mr[[i]][c("net.SR")])[[1]]
}

average_net_sr_single_mr <- sapply(seq_along(net_srs_single_mr[[1]]), function(i) {
  mean(sapply(net_srs_single_mr, function(x) x[[i]]))
})
average_net_sr_single_mr <- data.frame(net.SR = average_net_sr_single_mr)

sensitivities_average_single_mr <- sensitivities_single_mr[[1]][c("Close", "EMA")]
sensitivities_average_single_mr <- cbind(sensitivities_average_single_mr, "net.SR" = average_net_sr_single_mr)

heatmap_sr_mean_single_mr <- plotHeatmap(data_plot = sensitivities_average_single_mr, # dataset (data.frame) with calculations
                                         col_vlabels = "Close", # column name with the labels for a vertical axis (string)
                                         col_hlabels = "EMA", # column name with the labels for a horizontal axis (string)
                                         col_variable = "net.SR", # column name with the variable to show (string)
                                         main = paste("Mean", "Sensitivity analysis for mean reversion stategy based on single EMA", sep = ": "),
                                         label_size = 3)

heatmap_sr_mean_single_mr
ggsave("heatmap_sr_mean_single_mr.png", heatmap_sr_mean_single_mr, width = 8, height = 6)

net_srs_cross_mr <- list()

for(i in 1:length(sensitivities_cross_mr)) {
  net_srs_cross_mr[[i]] <- as.list(sensitivities_cross_mr[[i]][c("net.SR")])[[1]]
}

average_net_sr_cross_mr <- sapply(seq_along(net_srs_cross_mr[[1]]), function(i) {
  mean(sapply(net_srs_cross_mr, function(x) x[[i]]))
})
average_net_sr_cross_mr <- data.frame(net.SR = average_net_sr_cross_mr)

sensitivities_average_cross_mr <- sensitivities_cross_mr[[1]][c("EMA.fast", "EMA.slow")]
sensitivities_average_cross_mr <- cbind(sensitivities_average_cross_mr, "net.SR" = average_net_sr_cross_mr)

heatmap_sr_mean_cross_mr <- plotHeatmap(data_plot = sensitivities_average_cross_mr, # dataset (data.frame) with calculations
                                        col_vlabels = "EMA.fast", # column name with the labels for a vertical axis (string)
                                        col_hlabels = "EMA.slow", # column name with the labels for a horizontal axis (string)
                                        col_variable = "net.SR", # column name with the variable to show (string)
                                        main = paste("Mean", "Sensitivity analysis for mean reversion stategy based on EMA crossover", sep = ": "),
                                        label_size = 3)

heatmap_sr_mean_cross_mr
ggsave("heatmap_sr_mean_cross_mr.png", heatmap_sr_mean_cross_mr, width = 8, height = 6)


# Save the result for the best model: EMA crossover (60, 70)

write.csv(quarter_stats.all.group1, 
          "quarter_stats.all.group1.csv",
          row.names = FALSE)


