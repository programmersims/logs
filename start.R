syms <- c('ABT', 'ABBV', 'ACN', 'ACE', 'ADBE', 'ADT', 'AAP', 'AES', 'AET', 'AFL', 'AMG', 'A', 'GAS', 'APD', 'ARG', 'AKAM', 'AA', 'AGN', 'ALXN', 'ALLE', 'ADS', 'ALL', 'ALTR', 'MO', 'AMZN', 'AEE', 'AAL', 'AEP', 'AXP', 'AIG', 'AMT', 'AMP', 'ABC', 'AME', 'AMGN', 'APH', 'APC', 'ADI', 'AON', 'APA', 'AIV', 'AMAT', 'ADM', 'AIZ', 'T', 'ADSK', 'ADP', 'AN', 'AZO', 'AVGO', 'AVB', 'AVY', 'BHI', 'BLL', 'BAC', 'BK', 'BCR', 'BXLT', 'BAX', 'BBT', 'BDX', 'BBBY', 'BRK-B', 'BBY', 'BLX', 'HRB', 'BA', 'BWA', 'BXP', 'BSK', 'BMY', 'BRCM', 'BF-B', 'CHRW', 'CA', 'CVC', 'COG', 'CAM', 'CPB', 'COF', 'CAH', 'HSIC', 'KMX', 'CCL', 'CAT', 'CBG', 'CBS', 'CELG', 'CNP', 'CTL', 'CERN', 'CF', 'SCHW', 'CHK', 'CVX', 'CMG', 'CB', 'CI', 'XEC', 'CINF', 'CTAS', 'CSCO', 'C', 'CTXS', 'CLX', 'CME', 'CMS', 'COH', 'KO', 'CCE', 'CTSH', 'CL', 'CMCSA', 'CMA', 'CSC', 'CAG', 'COP', 'CNX', 'ED', 'STZ', 'GLW', 'COST', 'CCI', 'CSX', 'CMI', 'CVS', 'DHI', 'DHR', 'DRI', 'DVA', 'DE', 'DLPH', 'DAL', 'XRAY', 'DVN', 'DO', 'DTV', 'DFS', 'DISCA', 'DISCK', 'DG', 'DLTR', 'D', 'DOV', 'DOW', 'DPS', 'DTE', 'DD', 'DUK', 'DNB', 'ETFC', 'EMN', 'ETN', 'EBAY', 'ECL', 'EIX', 'EW', 'EA', 'EMC', 'EMR', 'ENDP', 'ESV', 'ETR', 'EOG', 'EQT', 'EFX', 'EQIX', 'EQR', 'ESS', 'EL', 'ES', 'EXC', 'EXPE', 'EXPD', 'ESRX', 'XOM', 'FFIV', 'FB', 'FAST', 'FDX', 'FIS', 'FITB', 'FSLR', 'FE', 'FSIV', 'FLIR', 'FLS', 'FLR', 'FMC', 'FTI', 'F', 'FOSL', 'BEN', 'FCX', 'FTR', 'GME', 'GPS', 'GRMN', 'GD', 'GE', 'GGP', 'GIS', 'GM', 'GPC', 'GNW', 'GILD', 'GS', 'GT', 'GOOGL', 'GOOG', 'GWW', 'HAL', 'HBI', 'HOG', 'HAR', 'HRS', 'HIG', 'HAS', 'HCA', 'HCP', 'HCN', 'HES', 'HPQ', 'HD', 'HON', 'HRL', 'HSP', 'HST', 'HCBK', 'HUM', 'HBAN', 'ITW', 'IR', 'INTC', 'ICE', 'IBM', 'IP', 'IPG', 'IFF', 'INTU', 'ISRG', 'IVZ', 'IRM', 'JEC', 'JBHT', 'JNJ', 'JOY', 'JPM', 'JNPR', 'KSU', 'K', 'KEY', 'GMCR', 'KMB', 'KIM', 'KMI', 'KLAC', 'KSS', 'KRFT', 'KR', 'LB', 'LLL', 'LH', 'LRCX', 'LM', 'LEG', 'LEN', 'LVLT', 'LUK', 'LLY', 'LNC', 'LLTC', 'LMT', 'L', 'LOW', 'LYB', 'MTB', 'MAC', 'M', 'MNK', 'MRO', 'MPC', 'MAR', 'MMC', 'MLM', 'MAS', 'MA', 'MAT', 'MKC', 'MCD', 'MHFI', 'MCK', 'MJN', 'MMV', 'MDT', 'MRK', 'MET', 'KORS', 'MCHP', 'MU', 'MSFT', 'MHK', 'TAP', 'MDLZ', 'MON', 'MNST', 'MCO', 'MS', 'MOS', 'MSI', 'MUR', 'MYL', 'NDAQ', 'NOV', 'NAVI', 'NTAP', 'NFLX', 'NWL', 'NFX', 'NEM', 'NWSA', 'NEE', 'NLSN', 'NKE', 'NI', 'NE', 'NBL', 'JWN', 'NSC', 'NTRS', 'NOC', 'NRG', 'NUE', 'NVDA', 'ORLY', 'OXY', 'OMC', 'OKE', 'ORCL', 'OI', 'PCAR', 'PLL', 'PH', 'PDCO', 'PAYX', 'PNR', 'PBCT', 'POM', 'PEP', 'PKI', 'PRGO', 'PFE', 'PCG', 'PM', 'PSX', 'PNW', 'PXD', 'PBI', 'PCL', 'PNC', 'RL', 'PPG', 'PPL', 'PX', 'PCP', 'PCLN', 'PFG', 'PG', 'PGR', 'PLD', 'PRU', 'PEG', 'PSA', 'PHM', 'PVH', 'QRVO', 'PWR', 'QCOM', 'DGX', 'RRC', 'RTN', 'O', 'RHT', 'REGN', 'RF', 'RSG', 'RAI', 'RHI', 'ROK', 'COL', 'ROP', 'ROST', 'RLC', 'R', 'CRM', 'SNDK', 'SCG', 'SLB', 'SNI', 'STX', 'SEE', 'SRE', 'SHW', 'SIAL', 'SPG', 'SWKS', 'SLG', 'SJM', 'SNA', 'SO', 'LUV', 'SWN', 'SE', 'STJ', 'SWK', 'SPLS', 'SBUX', 'HOT', 'STT', 'SRCL', 'SYK', 'STI', 'SYMC', 'SYY', 'TROW', 'TGT', 'TEL', 'TE', 'TGNA', 'THC', 'TDC', 'TSO', 'TXN', 'TXT', 'HSY', 'TRV', 'TMO', 'TIF', 'TWX', 'TWC', 'TJK', 'TMK', 'TSS', 'TSCO', 'RIG', 'TRIP', 'FOXA', 'TSN', 'TYC', 'UA', 'UNP', 'UNH', 'UPS', 'URI', 'UTX', 'UHS', 'UNM', 'URBN', 'VFC', 'VLO', 'VAR', 'VTR', 'VRSN', 'VZ', 'VRTX', 'VIAB', 'V', 'VNO', 'VMC', 'WMT', 'WBA', 'DIS', 'WM', 'WAT', 'ANTM', 'WFC', 'WDC', 'WU', 'WY', 'WHR', 'WFM', 'WMB', 'WEC', 'WYN', 'WYNN', 'XEL', 'XRX', 'XLNX', 'XL', 'XYL', 'YHOO', 'YUM', 'ZBH', 'ZION', 'ZTS',"FOSL","CORT","CEIX","ESIO","NKTR","VECO","QNST","SPPI","GEOS","MNTA","AMAG","DDD","MAT","KEM","FINL","IPXL","CTRL","CENX","CRZO","LITE","LCI","MDXG","ACLS","MDCO","RH","ACOR","LNTH","TLRD","SUPN","HIBB","WGO","TMST","DBD","BRS","AAOI","WRLD","MNK","ANF","BELFB","SM","B    KE","OFG","GES","PETS","EHTH","EGRX","EXTR","GCO","OSIS","RETL","TQQQ", "SSO", "FLGE", "UPRO","FBGX","BIB", "SDOW","DIG","KNOW","INDL","RUSS","CWEB","CHAU","NAIL","DRN","TECL","ERC","ERY","TMF","DRIP","LABU","YINN","CURE","FAS","MIDU","SRTY","DUG","TNA","SOXL","JPNL","BRZU","VXX","GDXJ","GBTC", "GDX","TWTR","TSLA","SNAP")

options(warn=-1)

args <- commandArgs(TRUE)
if(length(args) != 0){
  syms <- syms[(as.numeric(args[1])):(as.numeric(args[2]))]
} else{
  #system(paste("python stocks_daily.py --list", (paste(syms, collapse=" "))))   
   seq1 <- ceiling(seq(1, length(syms),length.out=9))
   for(i in 1:8){
     if(i != 8){
        system(paste("Rscript start.R", seq1[i], (seq1[i+1]-1)), wait = FALSE)
     } else{
     system(paste("Rscript start.R", seq1[i], (seq1[i+1])), wait = FALSE)
    }
  }
}

lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

if(length(args) != 0){

  suppressMessages(library(Rcpp))

  cppFunction('NumericVector get_variance(NumericVector x) {
              int n = x.size();
              NumericVector arr(n);
              double sum;
              double mean;
              /*double sum_weights = 0;
              for(int i =0; i<90; i++ ){
              sum_weights += w[i];
              }
              for(int i =0; i<90; i++ ){
              w[i] = (w[i]*90)/sum_weights;
              }*/
              for(int i = 91; i < n; i++) {
              for(int j = i-90; j < i; j++){
              sum = sum+(x[j]);
              }
              mean = sum/90;
              sum = 0;
              for(int j = i-90; j < i; j++){
              sum = sum+((mean-x[j])*(mean-x[j]));
              }
              arr[i] = pow(sum/89, 0.5);
              sum =0;
              }
              return arr;
              }')

  clean_data <- function(name){
    stock <- read.csv(paste("~/mods/stocks/",name, ".csv", sep=""), header = FALSE)
    stock$V8 <- NULL
    stock$V7 <- NULL
    colnames(stock) <- c("time", "high", "low", "open", "close", "volume")
    stock$change <- stock$close/stock$open
    stock$range <- stock$high/stock$low
    stock$change_rar <- (stock$change-1)/stock$range
    stock$range_var <- get_variance(stock$range)
    stock$change_var <- get_variance(stock$change)
    stock$volume_var <- get_variance(stock$volume)
    stock$cr_var <- get_variance(stock$change_rar)
    stock <- stock[101:nrow(stock),]
    stock$norm_range <- (stock$range-mean(stock$range))/stock$range_var
    stock$norm_change <- (stock$change-1)/stock$change_var
    stock$norm_cr <- (stock$change_rar)/stock$cr_var
    stock$norm_vol <- (stock$volume-mean(stock$volume))/stock$volume_var
    change <- stock$change[2:nrow(stock)]
    prev_norm_cge <- stock$norm_change[1:(nrow(stock)-1)]
    prev_norm_rge <- stock$norm_range[1:(nrow(stock)-1)]
    prev_norm_vol <- stock$norm_vol[1:(nrow(stock)-1)]

    return(stock)
    
  }

  get_data_prev2 <- function(stock, column, num1, num2){
    stock[, paste(column,num1,sep="x")] <- c(rep(0,num1),stock[1:((nrow(stock)-num1)),column])
    stock[, paste(column,num2,sep="x")] <- c(rep(0,num2),stock[1:((nrow(stock)-num2)),column])
    stock[, paste(column,num2,sep="_prev_")] <- stock[, paste(column,num1,sep="x")]/stock[, paste(column,num2,sep="x")]
    stock[, paste(column,num1,sep="x")] <- NULL
    stock[, paste(column,num2,sep="x")] <- NULL
    return(stock)
  }

  get_data_prev_bars <- function(stock, column, nums){
    for(i in 1:(length(nums)-1)){
      j <- nums[i]
      k <- nums[i+1]
      stock <- get_data_prev2(stock, column, j, k)
    }
    return(stock)
  }

  get_data_prev <- function(stock, column, num){
    for(i in num){
      stock[, paste(column,i,sep="_prev_")] <- c(rep(0,i),stock[1:((nrow(stock)-i)),column])
    }
    return(stock)
  }

  get_data_prev3 <- function(stock, column, num){
    for(i in num){
      stock[, paste(column,i,sep="_prev_")] <- ifelse(c(rep(0,i),stock[1:((nrow(stock)-i)),column])>0,1,-1)
    }
    return(stock)
  }
  
  normalize <- function(data){
    for(i in 1:ncol(data)){
      x <- data[,i] <- (data[,i] - mean(data[,i]))/sd(data[,i])
    }
    return(data)
  }
  date <- Sys.Date()

  suppressMessages(library(nnet))
  suppressMessages(library(e1071))
  suppressMessages(library(brnn))
  suppressMessages(library(ranger))
  suppressMessages(library(gam))

  for(i in syms){
    tryCatch( {
          stock <- clean_data(i)  
          targ <- stock$change[5:(nrow(stock))]
          stock <- cbind(stock$time,normalize(stock[,2:ncol(stock)]))
          stock <- stock[,c(7:ncol(stock))]
          train <- stock[4:(nrow(stock)-1),]
          test <- stock[nrow(stock),]

	  df <- cbind(train,targ)
          mod_lm <- lm(targ~., data=(df))
          preds <- predict(mod_lm, test)
      	  write(paste(date,preds, sep = ","),file=paste0("logs/lin_reg/", i,"_LINREG.txt"),append=TRUE) 
          
	  mod_lmw <- lm(targ~., data=(df), weights=seq(1,2.5,length.out=nrow(df)))
          preds <- predict(mod_lmw, test)
      	  write(paste(date,preds, sep = ","),file=paste0("logs/lin_reg_2_5/", i,"_LINREG2_5.txt"),append=TRUE)
	
	  mod_brnn <- brnn(targ~., data=(df),neurons=2, weights=seq(1,2.5,length.out=nrow(df)))
          preds <- predict(mod_brnn, test)
      	  write(paste(date,preds, sep = ","),file=paste0("logs/brnn/", i,"_BRNN.txt"),append=TRUE)
	  
	  mod_svm <- svm(targ~., data=(df), kernel="polynomial",degree=5)
          preds <- predict(mod_svm, test)
      	  write(paste(date,preds, sep = ","),file=paste0("logs/svm/", i,"_SVM.txt"),append=TRUE)
	  	
      }, error=function(e){})
      }
}




