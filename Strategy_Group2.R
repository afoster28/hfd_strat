# setting the working directory if needed
setwd("C:\\Users\\afost\\Documents\\UW\\Y2\\S1\\Quantitative Strategies High Frequency Data\\hfd_strat")

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs
library(caTools)
library(lubridate)
library(dplyr) # for if_else()
library(lattice) # for levelplot()
library(grDevices) # for colorRampPalette
library(cowplot)
library(ggplot2)

options(scipen=999)

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

positionVB_new <- function(signal, 
                           lower, 
                           upper, 
                           pos_flat, 
                           strategy)
{
  require(xts)
  
  # lets check thevalue of the strategy parameter
  if (! strategy %in% c("mom", "mr"))
  {  print("Strategy parameter incorrect. Please use 'mom' or 'mr'!")
    stop
  }
  
  # convert inputs to simpler objects  
  signal = coredata(signal)
  lower = coredata(lower)
  upper = coredata(upper)
  pos_flat = coredata(pos_flat)
  
  
  # lets first create a vector of 0s
  position <- rep(0, length(signal))
  
  for (i in 2:length(signal))
  {
    if ( pos_flat[i] == 1 ) position[i] <- 0 
    else
    { # check if values are nonmissing (otherwise calculations not possible)
      if (!is.na(signal[i-1]) & 
          !is.na(upper[i-1]) & 
          !is.na(lower[i-1]))
      { 
        # what if previous position was 0
        if (position[i-1] == 0){
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==-1){
          # what if previous position was -1
          if (signal[i-1] > lower[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==1){
          # what if previous position was 1
          if (signal[i-1] < upper[i-1]){position[i] <- 1}
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
        }
      } else position[i] <- position[i-1]
      # if anything is missing, keep previous position
    }
  }
  # reverse the position if we use a momentum ("mom") strategy
  if(strategy == "mom") position <- (-position)
  
  # return() function clearly indicates 
  # what the function should return
  return(position)
}


#---------------------------------------------------------------------
#### plotHeatmap ####

# function plotting strategy summary
# in a form of a heatmap

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
  
  # sprawdz czy nie za duzo wierszy i zwroc komunikat
  
  
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


# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')

# do it simply in a loop on quarters

heatmap_list <- list()
heatmap_list2 <- list()
sensitivities <- list()
sensitivities2 <- list()

for (selected_quarter in c("2021_Q1", "2021_Q3", "2021_Q4", 
                           "2022_Q2", "2022_Q4", 
                           "2023_Q1", "2023_Q2")) {
  
  # selected_quarter <- "2021_Q1"
  
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
  
  # layout(matrix(1:4, 2, 2))
  # chart_Series(data.group2$XAG.close, theme = myTheme)
  # chart_Series(data.group2$XAU.close, theme = myTheme)
  # chart_Series(data.group2$XAG.return, theme = myTheme)
  # chart_Series(data.group2$XAU.return, theme = myTheme)
  # layout(matrix(1))
  
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
  
  ###################################################################
  # lets formulate a spread: P1 - m * P2 (P_NASDAQ - m * P_AAPL) 
  # where m = m1/m2 is based on average ratio between the prices
  # on the PREVIOUS day
  
  # spread is a signal to our model, which shows whether to take 
  # position or not (volatility bands around the spread)
  
  # CAUTION! we assume the mean reverting behavior of the spread!
  
  ####################################################################
  # lets calculate average ratio of prices on the daily basis
  
  index_posix <- index(data.group2)
  time_component <- format(index_posix, format = "%H:%M:%S")
  target_time <- "17:00:00"
  indices <- which(time_component == target_time)

  cmd.av.ratio <- period.apply(data.group2,
                              INDEX = indices,
                              function(x) mean(x$XAU.close/x$XAG.close, 
                                               na.rm = TRUE)
  )
  
  names(cmd.av.ratio) <- "av.ratio"
  
  chart_Series(cmd.av.ratio)
  
  # about 64-74 XAG units per each unit of XAU (future)
  
  head(cmd.av.ratio)
  
  # but calculations based on the first day
  # will be used on the second day, etc.
  # lets adjust the dataset accordingly
  # by moving the time index to 18:00 of the next trading day (same day)
  
  index(cmd.av.ratio)
  
  # lets use functions from lubridate
  # ceiling_date() rounds the date up to midnight
  # (in fact start of the next day)
  # hours(n), minutes(n) - create a period object
  # with specified values
  
  # but some of the dates might be Fridays and in this case
  # we would move the index to 18:00 on Sunday
  
  # 6 = Friday
  
  # lets use if_else() from dplyr instead
  # lets apply the changes in our data object
  
  index(cmd.av.ratio) <- 
    ceiling_date(index(cmd.av.ratio), "day") - 
    hours(6) + 
    minutes(0) +
    if_else(wday(index(cmd.av.ratio)) == 6, 
            days(2),
            days(0))  
  
  ###################################################################
  # alternative spread based on RETURNS:
  # r1 - ms * r2 (r_NASDAQ - ms * r_AAPL) 
  # where ms = s1/s2 is based on the ratio of standard
  # deviations of returns on the PREVIOUS day
  
  cmd.sds.ratio <- period.apply(data.group2,
                               INDEX = indices,
                               function(x) sd(x$XAU.return, na.rm = TRUE) /
                                 sd(x$XAG.return, na.rm = TRUE)
  )
  
  names(cmd.sds.ratio) <- "sds.ratio"
  
  chart_Series(cmd.sds.ratio)
  
  # between 0.2 and 0.65 XAG units 
  # per each unit of XAU (future)
  
  # lets move the index to 18:00 of the next trading day (same day)
  
  index(cmd.sds.ratio) <- 
    ceiling_date(index(cmd.sds.ratio), "day") -
    hours(6) + 
    minutes(0) +
    if_else(wday(index(cmd.sds.ratio)) == 6, 
            days(2), 
            days(0))
  
  #-----------------------------------------------------------
  # we need to merge our basic 5 min data with daily calculations
  
  data.group2b <- merge(data.group2,
                    cmd.av.ratio, 
                    cmd.sds.ratio)
  
  # lets see how it works
  head(data.group2b["T09:30/T09:35"], 25)
  
  # there are a lot of missings in a the last 2 columns
  # which should be filled with the last non-missing value
  # (last multiplier is used until there is a new one)
  
  data.group2b$av.ratio <- na.locf(data.group2b$av.ratio,
                               na.rm = FALSE)
  data.group2b$sds.ratio <- na.locf(data.group2b$sds.ratio,
                                na.rm = FALSE)
  
  # lets make sure that we exclude weekends from our data
  
  table(wday(data.group2b))
  
  # there are no rows with 7 (Saturday)
  
  # now we can calculate the spread (in 2 variants)
  data.group2b$spread_avratio <- 
    data.group2b$XAU.close -
    data.group2b$av.ratio * data.group2b$XAG.close
  
  data.group2b$spread_sdsratio <- 
    data.group2b$XAU.return - 
    data.group2b$sds.ratio * data.group2b$XAG.return
  
  # plot both spreads
  
  # lets check it on the plot
  # layout(matrix(1:2, 2, 1))
  # chart_Series(data.group2b$spread_avratio, theme = myTheme)
  # abline( h = 0)
  # chart_Series(data.group2b$spread_sdsratio, theme = myTheme)
  # abline( h = 0)
  # layout(matrix(1))
  
  # we assume that spread mean reverts to 0,
  # which is not that clear on the top panel...
  
  
  # lets assume we do not trade within the first 10-mins of the day
  # and exit all positions 10 minutes before the end of quotations
  
  # lets create a pos_flat vector and fill it with 0s
  pos_flat <- xts(rep(0, nrow(data.group2b)), index(data.group2b))
  
  # we do not trade within the first 10 mins (18:00-18:10) 
  # but also before that time when session was inactive
  # and last 10 mins of the session (16:51-17:00)
  # but also after this time when session was inactive
  
  pos_flat["T16:51/T18:10"] <- 1
  
  # note this covers Fridays and Sundays as the series goes from 17:00 Friday to 17:05 Sunday
  
  # !!! there are no weekends in our data, so we do not need 
  # to control for that in pos_flat
  
  
  # # lets apply the volatility breakout model
  # 
  # # standard deviation of the spread
  # # runsd - efficient function for rolling standard deviation
  # 
  # data.group2b$spread_avratio_rollsd120 <-
  #   runsd(data.group2b$spread_avratio,
  #         120,
  #         endrule = "NA",
  #         align = "right")
  # 
  # data.group2b$spread_sdsratio_rollsd120 <- 
  #   runsd(data.group2b$spread_sdsratio, 
  #         120, 
  #         endrule = "NA",
  #         align = "right")
  # 
  # # lets put missings whenever XAG price is missing
  # 
  # data.group2b$spread_avratio_rollsd120[is.na(data.group2b$XAG.close)] <- NA
  # data.group2b$spread_sdsratio_rollsd120[is.na(data.group2b$XAG.close)] <- NA
  # 
  # #---------------------------------
  # # applying a volatility breakout model
  # # sample upper and lower bounds for spreads
  # # for a volatility multiplier of 3
  # # (here we put the upper and lower band along zero)
  # 
  # data.group2b$upper <- 3 * data.group2b$spread_avratio_rollsd120
  # data.group2b$lower <- (-3 * data.group2b$spread_avratio_rollsd120)
  # 
  # # lets see it on the plot
  # chart_Series(data.group2b$spread_avratio, theme = myTheme)
  # add_TA(data.group2b$upper, col = "red", on = 1)
  # add_TA(data.group2b$lower, col = "red", on = 1)
  # abline(h = 0, lty = 2, col = "gray")
  # 
  # # the same for spread_sdsratio
  # 
  # data.group2b$upper2 <- 3 * data.group2b$spread_sdsratio_rollsd120
  # data.group2b$lower2 <- (-3 * data.group2b$spread_sdsratio_rollsd120)
  # 
  # # lets see it on the plot
  # chart_Series(data.group2b$spread_sdsratio, theme = myTheme)
  # add_TA(data.group2b$upper2, col = "red", on = 1)
  # add_TA(data.group2b$lower2, col = "red", on = 1)
  # abline(h = 0, lty = 2, col = "gray")
  # 
  # ### position based on relation of the spread to volatility bands
  # 
  # # lets assume we do not trade within the first 10-mins of the day
  # # and exit all positions 10 minutes before the end of quotations
  # 
  # # lets create a pos_flat vector and fill it with 0s
  # pos_flat <- xts(rep(0, nrow(data.group2b)), index(data.group2b))
  # 
  # # we do not trade within the 10 mins quarter (18:00-18:10) 
  # # but also before that time when session was inactive
  # # and last 10 mins of the session (16:51-17:00)
  # # but also after this time when session was inactive
  # 
  # pos_flat["T16:51/T18:10"] <- 1
  # 
  # # note this covers Fridays and Sundays as the series goes from 17:00 Friday to 17:05 Sunday
  # 
  # # !!! there are no weekends in our data, so we do not need 
  # # to control for that in pos_flat
  # 
  # # lets use the positionVB_new() function from previous labs
  # 
  # data.group2b$pos_strategy <- positionVB_new(signal = data.group2b$spread_avratio,
  #                                         lower = data.group2b$lower,
  #                                         upper = data.group2b$upper,
  #                                         pos_flat = pos_flat,
  #                                         strategy = "mr" # important !!!
  # )
  # 
  # # lets create a vector of number of transactions
  # 
  # data.group2b$ntrans <- abs(diff.xts(data.group2b$pos_strategy))
  # 
  # # caution !!!
  # # our strategy pnl would be position*(pnl of the spread)
  # # pnl of the spread = pos*[diff(XAU.close)*$100 - m*diff(XAG.close)*$5000]
  # 
  # data.group2b$gross.pnl <- (data.group2b$pos_strategy) *
  #   (diff.xts(data.group2b$XAU.close) * 100 -
  #      data.group2b$av.ratio * diff.xts(data.group2b$XAG.close) * 5000)
  # # 100 is point value of XAU and 5000 is the point value of XAG so multiply by those
  # # pnl after  costs
  # # costs = $7 for XAG and $12 for XAU = (12+m*7) in total
  # # there is NO minus "-" in the costs - they are always positive !!!
  # 
  # data.group2b$net.pnl <- data.group2b$gross.pnl -
  #   data.group2b$ntrans * (12 + data.group2b$av.ratio * 7)
  # 
  # 
  # data.group2b$cum.gross.pnl <- cumsum(ifelse(is.na(data.group2b$gross.pnl),
  #                                         0,
  #                                         data.group2b$gross.pnl))
  # 
  # data.group2b$cum.net.pnl <- cumsum(ifelse(is.na(data.group2b$net.pnl),
  #                                       0,
  #                                       data.group2b$net.pnl))
  # 
  # # lets see if it was profitable
  # 
  # chart_Series(data.group2b$cum.gross.pnl,
  #              theme = myTheme)
  # add_TA(data.group2b$cum.net.pnl,
  #        on = 1,
  #        col = "red")
  # abline(h = 0, lty = 2, col = "gray")
  
  # lets do a comparison within a loop for spread and spread2

  for(volat.sd in c(60, 90, 120, 150, 180)) { # different volatility memories
    for(m_ in c(0.5, 1, 1.5, 2, 2.5, 3, 3.5)) { # different multipliers
      
      message(paste0("volat.sd = ", volat.sd,
                     ", m_ = ", m_)) 
      
      # calculating elements of the strategy
      XAU_price <- coredata(data.group2b$XAU.close)
      XAG_price <- coredata(data.group2b$XAG.close)
      
      signal <- coredata(data.group2b$spread_avratio)
      signal2 <- coredata(data.group2b$spread_sdsratio)
      
      upper <- m_ * runsd(signal, volat.sd, 
                          endrule = "NA", 
                          align = "right")
      lower <- -m_ * runsd(signal, volat.sd, 
                           endrule = "NA", 
                           align = "right")
      
      upper2 <- m_ * runsd(signal2, volat.sd, 
                           endrule = "NA", 
                           align = "right")
      lower2 <- -m_ * runsd(signal2, volat.sd, 
                            endrule = "NA", 
                            align = "right")
      
      # position for mean-reverting strategy
      pos.mr <- positionVB_new(signal, lower, upper,
                               pos_flat = pos_flat,
                               strategy = "mr" # important !!!
      )
      pos.mr2 <- positionVB_new(signal2, lower2, upper2,
                                pos_flat = pos_flat,
                                strategy = "mr" # important !!!
      )
      # number of transactions
      ntrans <- abs(diff.xts(pos.mr))
      ntrans2 <- abs(diff.xts(pos.mr2))
      
      # gross pnl
      gross.pnl <- (pos.mr) *
        (diff.xts(XAU_price) * 100 # point value for XAU
         - coredata(data.group2b$av.ratio) * diff.xts(XAG_price) * 5000) # point value for XAG
      
      gross.pnl2 <- (pos.mr2) *
        (diff.xts(XAU_price) * 100  # point value for XAU
         - coredata(data.group2b$sds.ratio) * diff.xts(XAG_price) * 5000) # point value for XAG
      
      # pnl after  costs
      # costs = $7 for XAG and $12 for XAU = (12+m*7) in total
      # there is NO minus "-" in the costs - they are always positive !!!
      
      net.pnl <- gross.pnl - ntrans * (12 + coredata(data.group2b$av.ratio) * 7)
      net.pnl2 <- gross.pnl2 - ntrans2 * (12 + coredata(data.group2b$sds.ratio) * 7)
      
      # aggregate to daily
      # ends_ <- endpoints(data.group2b, "days")
      
      pnl.gross.d <- period.apply(gross.pnl, INDEX = indices, 
                                  FUN = function(x) sum(x, na.rm = TRUE))
      pnl.gross2.d <- period.apply(gross.pnl2, INDEX = indices, 
                                   FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.d <- period.apply(net.pnl, INDEX = indices,
                                FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net2.d <- period.apply(net.pnl2, INDEX = indices,
                                 FUN = function(x) sum(x, na.rm = TRUE))
      ntrans.d <- period.apply(ntrans, INDEX = indices, 
                               FUN = function(x) sum(x, na.rm = TRUE))
      ntrans2.d <- period.apply(ntrans2, INDEX = indices, 
                                FUN = function(x) sum(x, na.rm = TRUE))
      
      # calculate summary measures
      gross.SR <- mySR(pnl.gross.d, scale = 252)
      gross.SR2 <- mySR(pnl.gross2.d, scale = 252)
      net.SR <- mySR(pnl.net.d, scale = 252)
      net.SR2 <- mySR(pnl.net2.d, scale = 252)
      
      gross.CR <- myCalmarRatio(pnl.gross.d, scale = 252)
      gross.CR2 <- myCalmarRatio(pnl.gross2.d, scale = 252)
      net.CR <- myCalmarRatio(pnl.net.d, scale = 252)
      net.CR2 <- myCalmarRatio(pnl.net2.d, scale = 252)
      
      gross.PnL <- sum(pnl.gross.d, na.rm = TRUE)
      gross.PnL2 <- sum(pnl.gross2.d, na.rm = TRUE)
      net.PnL <- sum(pnl.net.d, na.rm = TRUE)
      net.PnL2 <- sum(pnl.net2.d, na.rm = TRUE)
      
      av.daily.ntrans <- mean(ntrans.d, na.rm = TRUE)
      av.daily.ntrans2 <- mean(ntrans2.d, na.rm = TRUE) 
      
      stat = net.CR * max(0, log(abs(net.PnL/1000)))
      stat2 = net.CR2 * max(0, log(abs(net.PnL2/1000)))
      
      # collecting all statistics for a particular quarter
      if(volat.sd == 180 & m_ == 1) {
        quarter_stats <- data.frame(quarter = selected_quarter,
                                    assets.group = 2,
                                    gross.SR,
                                    net.SR,
                                    gross.CR,
                                    net.CR,
                                    gross.PnL,
                                    net.PnL,
                                    av.daily.ntrans,
                                    stat,
                                    stringsAsFactors = FALSE
        )
        
        quarter_stats2 <- data.frame(quarter = selected_quarter,
                                    assets.group = 2,
                                    gross.SR2,
                                    net.SR2,
                                    gross.CR2,
                                    net.CR2,
                                    gross.PnL2,
                                    net.PnL2,
                                    av.daily.ntrans2,
                                    stat2,
                                    stringsAsFactors = FALSE
        )
        
        # collect summaries for all quarters
        if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
          quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
        
        if(!exists("quarter_stats2.all.group2")) quarter_stats2.all.group2 <- quarter_stats2 else
          quarter_stats2.all.group2 <- rbind(quarter_stats2.all.group2, quarter_stats2)
        
        # create a plot of gross and net pnl and save it to png file
        y_range <- range(c(cumsum(pnl.gross.d), cumsum(pnl.net.d)))

        png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
            width = 1000, height = 600)
        print( # when plotting in a loop you have to use print()
          plot(cumsum(pnl.gross.d),
               type = "l",
               main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter),
               col = "#377EB8",
               xlab = "Time",
               ylab = "Cumulative PnL",
               ylim = y_range
          )
        )
        lines(cumsum(pnl.net.d), col = "#E41A1C")
        legend("topleft", legend = c("Gross PnL", "Net PnL"), col = c("#377EB8", "#E41A1C"), lty = 1, cex = 1)
        dev.off()
      }
      
      # summary of a particular strategy
      summary_ <- data.frame(spread = "av.ratio",
                             volat.sd = volat.sd,
                             m = m_,
                             period = selected_quarter, # "2016-08-16 - 2016-11",
                             gross.SR,
                             net.SR,
                             gross.PnL,
                             net.PnL,
                             av.daily.ntrans,
                             stringsAsFactors = FALSE)

      summary2_ <- data.frame(spread = "sds.ratio",
                              volat.sd = volat.sd,
                              m = m_,
                              period = selected_quarter, # "2016-08-16 - 2016-11",
                              gross.SR = gross.SR2,
                              net.SR = net.SR2,
                              gross.PnL = gross.PnL2,
                              net.PnL = net.PnL2,
                              av.daily.ntrans = av.daily.ntrans2,
                              stringsAsFactors = FALSE)

      # putting all summaries together

      if(!exists("summary.pair.trading")) summary.pair.trading <- rbind(summary_, summary2_) else
        summary.pair.trading <- rbind(summary.pair.trading, summary_, summary2_)
      
      # deleting working files not needed any more
      rm(gross.SR, gross.SR2, net.SR, net.SR2, net.CR, net.CR2,
         gross.PnL, gross.PnL2, net.PnL, net.PnL2,
         av.daily.ntrans, av.daily.ntrans2, stat, stat2,
         pnl.gross.d, pnl.gross2.d, pnl.net.d, pnl.net2.d, 
         ntrans.d, ntrans2.d,
         pnl.gross, pnl.gross2, pnl.net, pnl.net2, 
         ntrans, ntrans2,
         pos.mr, pos.mr2, summary_, summary2_,
         XAU_price, XAG_price,
         signal, signal2, lower, lower2, upper, upper2)
      
    } # end of loop for m_
  } # end of loop for volatility  
  
  
  # lets see the results on the heatmap graph
  
  # net.SR - spread av_ratio
  heatmap_sr <- plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
              col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
              col_hlabels = "m", # column name with the labels for a horizontal axis (string)
              col_variable = "net.SR", # column name with the variable to show (string)
              main = paste(selected_quarter, "Sensitivity analysis for pair trading - spread based on prices ratio", sep = ": "),
              label_size = 3)
  
  # volat.sd = 180, m_ = 3.5
  # browser()
  #out <- summary.pair.trading
  heatmap_sr2 <- plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "sds.ratio",], # dataset (data.frame) with calculations
                            col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
                            col_hlabels = "m", # column name with the labels for a horizontal axis (string)
                            col_variable = "net.SR", # column name with the variable to show (string)
                            main = paste(selected_quarter, "Sensitivity analysis for pair trading - spread based on returns ratio", sep = ": "),
                            label_size = 3)
  
  # net.Pnl - spread av_ratio
  # plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
  #             col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
  #             col_hlabels = "m", # column name with the labels for a horizontal axis (string)
  #             col_variable = "net.PnL", # column name with the variable to show (string)
  #             main = "Sensitivity analysis for pair trading - spread based on prices ratio",
  #             label_size = 3)
  
  # av.daily.ntrans
  # plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
  #             col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
  #             col_hlabels = "m", # column name with the labels for a horizontal axis (string)
  #             col_variable = "av.daily.ntrans", # column name with the variable to show (string)
  #             main = "Sensitivity analysis for pair trading - spread based on prices ratio",
  #             label_size = 3)
  
  sensitivities[[selected_quarter]] <- summary.pair.trading[summary.pair.trading$spread == "av.ratio",]
  sensitivities2[[selected_quarter]] <- summary.pair.trading[summary.pair.trading$spread == "sds.ratio",]
  rm(summary.pair.trading)
  
  # collect summaries for all quarters
  # if(!exists("heatmaps.all.group2")) heatmaps.all.group2 <- heatmap_sr else
  #   heatmaps.all.group2 <- rbind(heatmaps.all.group2, heatmap_sr)
  
  heatmap_list[[selected_quarter]] <- heatmap_sr
  heatmap_list2[[selected_quarter]] <- heatmap_sr2
}

# All heatmaps
combined_plot <- plot_grid(plotlist = heatmap_list, ncol = 1, align = 'v')
ggsave("combined_heatmaps.png", combined_plot, width = 8, height = 48)

combined_plot2 <- plot_grid(plotlist = heatmap_list2, ncol = 1, align = 'v')
ggsave("combined_heatmaps2.png", combined_plot2, width = 8, height = 48)

# Mean heatmap
net_srs <- list()

for(i in 1:length(sensitivities)) {
  net_srs[[i]] <- as.list(sensitivities[[i]][c("net.SR")])[[1]]
}

average_net_sr <- sapply(seq_along(net_srs[[1]]), function(i) {
  mean(sapply(net_srs, function(x) x[[i]]))
})
average_net_sr <- data.frame(net.SR = average_net_sr)

sensitivities_average <- sensitivities[[1]][c("spread", "volat.sd", "m")]
sensitivities_average <- cbind(sensitivities_average, "net.SR" = average_net_sr)

heatmap_sr_mean <- plotHeatmap(data_plot = sensitivities_average, # dataset (data.frame) with calculations
                          col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
                          col_hlabels = "m", # column name with the labels for a horizontal axis (string)
                          col_variable = "net.SR", # column name with the variable to show (string)
                          main = paste("Mean", "Sensitivity analysis for pair trading - spread based on prices ratio", sep = ": "),
                          label_size = 3)

heatmap_sr_mean
ggsave("heatmap_sr_mean.png", heatmap_sr_mean, width = 8, height = 6)

# volat.sd = 180 and m = 1 are the combination producing the largest net SR on average across in-sample quarters: 1.14
# The main driver of this is Q2 2023
# The best model run receives goes into an additional IF block above and records stats per quarter

write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = FALSE)


# Repeat for returns ratio
net_srs2 <- list()

for(i in 1:length(sensitivities2)) {
  net_srs2[[i]] <- as.list(sensitivities2[[i]][c("net.SR")])[[1]]
}

average_net_sr2 <- sapply(seq_along(net_srs2[[1]]), function(i) {
  mean(sapply(net_srs2, function(x) x[[i]]))
})
average_net_sr2 <- data.frame(net.SR = average_net_sr2)

sensitivities_average2 <- sensitivities2[[1]][c("spread", "volat.sd", "m")]
sensitivities_average2 <- cbind(sensitivities_average2, "net.SR" = average_net_sr2)

heatmap_sr_mean2 <- plotHeatmap(data_plot = sensitivities_average2, # dataset (data.frame) with calculations
                               col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
                               col_hlabels = "m", # column name with the labels for a horizontal axis (string)
                               col_variable = "net.SR", # column name with the variable to show (string)
                               main = paste("Mean", "Sensitivity analysis for pair trading - spread based on returns ratio", sep = ": "),
                               label_size = 3)

heatmap_sr_mean2
ggsave("heatmap_sr_mean2.png", heatmap_sr_mean2, width = 8, height = 6)

# Very low SR overall - use price ratio in quarter_stats.all.group2 instead
