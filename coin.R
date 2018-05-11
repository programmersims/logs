#syms <- c('ABT', 'ABBV', 'ACN', 'ACE', 'ADBE', 'ADT', 'AAP', 'AES', 'AET', 'AFL', 'AMG', 'A', 'GAS', 'APD', 'ARG', 'AKAM', 'AA', 'AGN', 'ALXN', 'ALLE', 'ADS', 'ALL', 'ALTR', 'MO', 'AMZN', 'AEE', 'AAL', 'AEP', 'AXP', 'AIG', 'AMT', 'AMP', 'ABC', 'AME', 'AMGN', 'APH', 'APC', 'ADI', 'AON', 'APA', 'AIV', 'AMAT', 'ADM', 'AIZ', 'T', 'ADSK', 'ADP', 'AN', 'AZO', 'AVGO', 'AVB', 'AVY', 'BHI', 'BLL', 'BAC', 'BK', 'BCR', 'BXLT', 'BAX', 'BBT', 'BDX', 'BBBY', 'BRK-B', 'BBY', 'BLX', 'HRB', 'BA', 'BWA', 'BXP', 'BSK', 'BMY', 'BRCM', 'BF-B', 'CHRW', 'CA', 'CVC', 'COG', 'CAM', 'CPB', 'COF', 'CAH', 'HSIC', 'KMX', 'CCL', 'CAT', 'CBG', 'CBS', 'CELG', 'CNP', 'CTL', 'CERN', 'CF', 'SCHW', 'CHK', 'CVX', 'CMG', 'CB', 'CI', 'XEC', 'CINF', 'CTAS', 'CSCO', 'C', 'CTXS', 'CLX', 'CME', 'CMS', 'COH', 'KO', 'CCE', 'CTSH', 'CL', 'CMCSA', 'CMA', 'CSC', 'CAG', 'COP', 'CNX', 'ED', 'STZ', 'GLW', 'COST', 'CCI', 'CSX', 'CMI', 'CVS', 'DHI', 'DHR', 'DRI', 'DVA', 'DE', 'DLPH', 'DAL', 'XRAY', 'DVN', 'DO', 'DTV', 'DFS', 'DISCA', 'DISCK', 'DG', 'DLTR', 'D', 'DOV', 'DOW', 'DPS', 'DTE', 'DD', 'DUK', 'DNB', 'ETFC', 'EMN', 'ETN', 'EBAY', 'ECL', 'EIX', 'EW', 'EA', 'EMC', 'EMR', 'ENDP', 'ESV', 'ETR', 'EOG', 'EQT', 'EFX', 'EQIX', 'EQR', 'ESS', 'EL', 'ES', 'EXC', 'EXPE', 'EXPD', 'ESRX', 'XOM', 'FFIV', 'FB', 'FAST', 'FDX', 'FIS', 'FITB', 'FSLR', 'FE', 'FSIV', 'FLIR', 'FLS', 'FLR', 'FMC', 'FTI', 'F', 'FOSL', 'BEN', 'FCX', 'FTR', 'GME', 'GPS', 'GRMN', 'GD', 'GE', 'GGP', 'GIS', 'GM', 'GPC', 'GNW', 'GILD', 'GS', 'GT', 'GOOGL', 'GOOG', 'GWW', 'HAL', 'HBI', 'HOG', 'HAR', 'HRS', 'HIG', 'HAS', 'HCA', 'HCP', 'HCN', 'HES', 'HPQ', 'HD', 'HON', 'HRL', 'HSP', 'HST', 'HCBK', 'HUM', 'HBAN', 'ITW', 'IR', 'INTC', 'ICE', 'IBM', 'IP', 'IPG', 'IFF', 'INTU', 'ISRG', 'IVZ', 'IRM', 'JEC', 'JBHT', 'JNJ', 'JOY', 'JPM', 'JNPR', 'KSU', 'K', 'KEY', 'GMCR', 'KMB', 'KIM', 'KMI', 'KLAC', 'KSS', 'KRFT', 'KR', 'LB', 'LLL', 'LH', 'LRCX', 'LM', 'LEG', 'LEN', 'LVLT', 'LUK', 'LLY', 'LNC', 'LLTC', 'LMT', 'L', 'LOW', 'LYB', 'MTB', 'MAC', 'M', 'MNK', 'MRO', 'MPC', 'MAR', 'MMC', 'MLM', 'MAS', 'MA', 'MAT', 'MKC', 'MCD', 'MHFI', 'MCK', 'MJN', 'MMV', 'MDT', 'MRK', 'MET', 'KORS', 'MCHP', 'MU', 'MSFT', 'MHK', 'TAP', 'MDLZ', 'MON', 'MNST', 'MCO', 'MS', 'MOS', 'MSI', 'MUR', 'MYL', 'NDAQ', 'NOV', 'NAVI', 'NTAP', 'NFLX', 'NWL', 'NFX', 'NEM', 'NWSA', 'NEE', 'NLSN', 'NKE', 'NI', 'NE', 'NBL', 'JWN', 'NSC', 'NTRS', 'NOC', 'NRG', 'NUE', 'NVDA', 'ORLY', 'OXY', 'OMC', 'OKE', 'ORCL', 'OI', 'PCAR', 'PLL', 'PH', 'PDCO', 'PAYX', 'PNR', 'PBCT', 'POM', 'PEP', 'PKI', 'PRGO', 'PFE', 'PCG', 'PM', 'PSX', 'PNW', 'PXD', 'PBI', 'PCL', 'PNC', 'RL', 'PPG', 'PPL', 'PX', 'PCP', 'PCLN', 'PFG', 'PG', 'PGR', 'PLD', 'PRU', 'PEG', 'PSA', 'PHM', 'PVH', 'QRVO', 'PWR', 'QCOM', 'DGX', 'RRC', 'RTN', 'O', 'RHT', 'REGN', 'RF', 'RSG', 'RAI', 'RHI', 'ROK', 'COL', 'ROP', 'ROST', 'RLC', 'R', 'CRM', 'SNDK', 'SCG', 'SLB', 'SNI', 'STX', 'SEE', 'SRE', 'SHW', 'SIAL', 'SPG', 'SWKS', 'SLG', 'SJM', 'SNA', 'SO', 'LUV', 'SWN', 'SE', 'STJ', 'SWK', 'SPLS', 'SBUX', 'HOT', 'STT', 'SRCL', 'SYK', 'STI', 'SYMC', 'SYY', 'TROW', 'TGT', 'TEL', 'TE', 'TGNA', 'THC', 'TDC', 'TSO', 'TXN', 'TXT', 'HSY', 'TRV', 'TMO', 'TIF', 'TWX', 'TWC', 'TJK', 'TMK', 'TSS', 'TSCO', 'RIG', 'TRIP', 'FOXA', 'TSN', 'TYC', 'UA', 'UNP', 'UNH', 'UPS', 'URI', 'UTX', 'UHS', 'UNM', 'URBN', 'VFC', 'VLO', 'VAR', 'VTR', 'VRSN', 'VZ', 'VRTX', 'VIAB', 'V', 'VNO', 'VMC', 'WMT', 'WBA', 'DIS', 'WM', 'WAT', 'ANTM', 'WFC', 'WDC', 'WU', 'WY', 'WHR', 'WFM', 'WMB', 'WEC', 'WYN', 'WYNN', 'XEL', 'XRX', 'XLNX', 'XL', 'XYL', 'YHOO', 'YUM', 'ZBH', 'ZION', 'ZTS')


#syms <- c("FOSL","CORT","CEIX","ESIO","NKTR","VECO","QNST","SPPI","GEOS","MNTA","AMAG","DDD","MAT","KEM","FINL","IPXL","CTRL","CENX","CRZO","LITE","LCI","MDXG","ACLS","MDCO","RH","ACOR","LNTH","TLRD","SUPN","HIBB","WGO","TMST","DBD","BRS","AAOI","WRLD","MNK","ANF","BELFB","SM","B    KE","OFG","GES","PETS","EHTH","EGRX","EXTR","GCO","OSIS")
syms <- c("RETL","TQQQ", "SSO", "FLGE", "UPRO","FBGX","BIB", "SDOW","DIG","KNOW","INDL","RUSS","CWEB","CHAU","NAIL","DRN","TECL","ERC","ERY","TMF","DRIP","LABU","YINN","CURE","FAS","MIDU","SRTY","DUG","TNA","SOXL","JPNL","BRZU","VXX","GDXJ","GBTC", "GDX")
#syms <- c("DRIP")
options(warn=-1)

args <- commandArgs(TRUE)
if(length(args) != 0){
  syms <- syms[(as.numeric(args[1])):(as.numeric(args[2]))]
} else{
  system(paste("python stocks_daily.py --list", (paste(syms, collapse=" "))))
   
   seq1 <- ceiling(seq(1, length(syms),length.out=9))
   for(i in 1:8){
     if(i != 8){
        system(paste("Rscript coin.R", seq1[i], (seq1[i+1]-1)), wait = FALSE)
     } else{
     system(paste("Rscript coin.R", seq1[i], (seq1[i+1])), wait = FALSE)
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
  
  get_data_prev4 <- function(stock, column, num){
    new_mat <- matrix(nrow=nrow(stock),ncol=num)
    for(i in 1:num){
      new_mat[,i] <- ifelse(c(rep(0,i),stock[1:((nrow(stock)-i)),column])>1,1,0)
    }
    sums <- apply(new_mat, 1, sum)
    stock[,paste("sums",i,sep="_")] <- sums
    return(stock)
  }

  get_clusters <- function(data, centers){
    vect <- kmeans(x=data[,2:ncol(data)], centers=centers)
    for(i in 2:centers){
      data[,paste("cluster_",i, sep="")] <- ifelse(vect$cluster==i,1,0)
    }
    return(data)
  }

  normalize <- function(data){
    for(i in 1:ncol(data)){
      x <- data[,i] <- (data[,i] - mean(data[,i]))/sd(data[,i])
    }
    return(data)
  }

  suppressMessages(library(glmnet))
  suppressMessages(library(nnet))
  suppressMessages(library(e1071))
  suppressMessages(library(brnn))
  suppressMessages(library(ranger))
  suppressMessages(library(gam))

  #suppressMessages(library(doSNOW))
  #library(ranger)
  #cl <-makeCluster(7, type="SOCK")
  #registerDoSNOW(makeCluster(7, type="SOCK"))

  suppressMessages(library(Matrix))
  suppressMessages(library(xgboost))
  iters2 <- c(12)
  for(i in syms){
    for(w in iters2){
    tryCatch( {
          stock <- clean_data(i)  }, error=function(e){})
          targ <- stock$change[41:(nrow(stock))]
          #test_targ <- stock$change[((((nrow(stock)+40)/2)+1)):((nrow(stock)))]
          stock <- get_data_prev2(stock, "close", 0,2)
          stock <- get_data_prev2(stock, "close", 0,5) 
          stock <- get_data_prev2(stock, "close", 5,10) 
          stock <- get_data_prev2(stock, "close", 10,15) 
          stock <- get_data_prev2(stock, "close", 15,20) 
          stock <- get_data_prev2(stock, "close", 20,30)
	  
	  stock$cr2 <- stock$close_prev_2 * stock$norm_cr
	  stock$cr5 <- stock$close_prev_5 * stock$norm_cr
	  stock$cr10 <- stock$close_prev_10 * stock$norm_cr

          stock <- stock[40:nrow(stock),]
          stock$c2_5 <- stock$close_prev_5*stock$close_prev_2
          stock$c5_10 <- stock$close_prev_5*stock$close_prev_10
          stock$c15_10 <- stock$close_prev_10*stock$close_prev_15
          stock$c15_30 <- stock$close_prev_15*stock$close_prev_20
          stock$c20_30 <- stock$close_prev_20*stock$close_prev_30
	  #x <- ncol(stock)
	  #stock <- get_data_prev(stock, "change", c(1:30))
	  #stock <- get_data_prev(stock, "norm_change", c(1:10))
	  #stock <- get_data_prev(stock, "norm_range", c(1:10))
          #stock <- get_clusters(stock, 7)
          #targ <- stock$change[2:((nrow(stock)/2)+1)]
          #test_targ <- stock$change[(((nrow(stock)/2)+2)):((nrow(stock)))]
          stock <- cbind(stock$time,normalize(stock[,2:ncol(stock)]))
          colnames(stock)[1] <- "time"
          stock <- stock[,c(7:ncol(stock))]
          train <- stock[1:(nrow(stock)/2),]
	  test_targ <- targ[((nrow(stock)/2)+1):(nrow(stock)-1)]
	  targ <- targ[1:(nrow(stock)/2)]
          test <- stock[((nrow(stock)/2)+1):(nrow(stock)-1),]
          #lasso2 <- cv.glmnet(x=as.matrix(train), y=targ, weights = (seq(1,1.4,length.out = nrow(train))))
          #lasso2 <- glmnet(x=as.matrix(train), y=targ, lambda=(lasso2$lambda.min*0.9), weights = (seq(1,1.4,length.out = nrow(train))))
          #tmp_coeffs <- coef(lasso2)
          #str_data <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

          #cols <- as.character(str_data[c(FALSE,str_data[2:nrow(str_data),2]!=0),1])
          #print(cols)
          #preds_lasso2 <- predict(lasso2, as.matrix(test))
          #str7 <- cor(preds_lasso2, test_targ)
          #sharp7 <- log(preds_lasso2)*log(test_targ)
          #sharp7 <- mean(sharp7)/sd(sharp7) * sqrt(256)
          #print(colnames(train))   
          df <- cbind(train,targ)
          mod_lm <- lm(targ~., data=(df))
	  print(summary(mod_lm))
          preds <- predict(mod_lm, test)
          preds_lm <- predict(mod_lm, train)
          summ <- summary(mod_lm)
          if(sqrt(summ$r.squared) > 0.0){
            #w <- 1/(sqrt(summ$r.squared)*25)
            #print(w)
            #lasso3 <- cv.glmnet(x=as.matrix(train), y=targ, weights = (seq(1,3,length.out = nrow(train))), alpha=0.75)
            #lambda <- lasso3$lambda.min
            #preds_lasso3 <- numeric(length(test_targ))
            #parts <- ceiling(seq(1,length(test_targ), length.out=(5)))
            #k <- 1
            #for(h in 2:length(parts)){
            #    j <- parts[h]
            #    if(h == 2){
            #        lasso3 <- glmnet(x=as.matrix(train), y=targ, lambda=lambda, weights = (seq(1,3,length.out = nrow(train))),alpha=0.75)
            #        preds_lasso3[k:j] <- predict(lasso3, as.matrix(test[k:j,]))
            #    }
            #    else {
            #      train2 <- rbind(train,test[1:k,])
            #      lasso3 <- cv.glmnet(x=as.matrix(train2), y=c(targ,test_targ[1:k]), weights = (seq(1,3,length.out = nrow(train2))), alpha=0.75)
            #      lasso3 <- glmnet(x=as.matrix(train2), y=c(targ,test_targ[1:k]), lambda=(lasso3$lambda.min), weights = (seq(1,3,length.out = nrow(train2))),alpha=0.75)
            #      preds_lasso3[(k+1):j] <- predict(lasso3, as.matrix(test[(k+1):j,]))
            #    } 
            #    k <- j             
            #}
            preds_nnet <- numeric(length(test_targ))
            parts <- ceiling(seq(1,length(test_targ), length.out=(10)))
            k <- 1
            for(h in 2:length(parts)){
                j <- parts[h]
                if(h == 2){
		    for(iters in 1:10){
                      invisible(capture.output(nnet1 <- nnet(x=as.matrix(train), y=targ, linout=TRUE, size=w, maxit=350, decay=0.04, MaxNWts = 2000, weights = (seq(1,3,length.out = nrow(train))))))
                      preds_nnet[k:j] <- (predict(nnet1, as.matrix(test[k:j,])))/5+preds_nnet[k:j]
                    }
		}
                else {
                  train2 <- rbind(train,test[1:k,])
		  for(iters in 1:10){
			  invisible(capture.output(nnet1 <- nnet(x=as.matrix(train2), y=c(targ,test_targ[1:k]), linout=TRUE, size=w, maxit=350, decay=0.04, MaxNWts = 2000, weights = (seq(1,3,length.out = nrow(train2))))))
			  preds_nnet[(k+1):j] <- (predict(nnet1, as.matrix(test[(k+1):j,])))/5+preds_nnet[(k+1):j]
		  }
                } 
                k <- j             
            }
            #train$time <- seq(1,nrow(train))
            #train$time <- seq(1,nrow(train))
            #test$time <- seq(nrow(train)+1,nrow(train)+nrow(test))
            #k <- 1
            #for(h in 2:length(parts)){
            #    j <- parts[h]
            #    if(h == 2){
            #        dat <- cbind(train,targ)
            #        colnames(dat)[ncol(dat)] <- "targ"
            #        mod_gam <- gam(targ~s(time)+.,data=dat[,unique(c(cols,"time"))], weights = (seq(1,2,length.out = nrow(train))))
            #        preds_gam[k:j] <- predict(mod_gam, (test[k:j,unique(c(cols,"time","norm_change"))]))
            #    }
            #    else {
            #      train2 <- rbind(train,test[1:k,])
            #      dat <- cbind(train2, (c(targ,test_targ[1:k])))
            #      colnames(dat)[ncol(dat)] <- "targ"
            #      mod_gam <- gam(targ~s(time) + ., data=dat[,unique(c(cols,"time"))], weights = (seq(1,2,length.out = nrow(train2))))
            #      preds_gam[(k+1):j] <- predict(mod_gam, (test[(k+1):j,unique(c(cols,"time","norm_change"))]))
            #    } 
            #    k <- j             
            #}
            
            #str9 <- cor(preds_gam, test_targ)
            #sharp9 <- log(preds_gam)*log(test_targ)
            #sharp9 <- mean(sharp9)/sd(sharp9) * sqrt(256)
            
            #str8 <- cor(preds_lasso3, test_targ)
            #sharp8 <- log(preds_lasso3)*log(test_targ)
            #sharp8 <- mean(sharp8)/sd(sharp8) * sqrt(256)
            #preds_nnet <- preds_nnet[(length(preds_nnet)-100):length(preds_nnet)]
            #test_targ <- test_targ[(length(test_targ)-100):length(test_targ)]
            str9 <- cor(preds_nnet, test_targ)
            sharp9 <- log(preds_nnet)*log(test_targ)
            sharp9 <- mean(sharp9)/sd(sharp9) * sqrt(256)

            vals <- abs(preds_nnet-1) > 0.01
            str12 <- cor(preds_nnet[vals], test_targ[vals])
            sharp12 <- log(preds_nnet[vals])*log(test_targ[vals])
            sharp12  <- mean(sharp12)/sd(sharp12) * sqrt(256*sum(vals)/length(vals))
            
	    vals <- abs(preds_nnet-1) > 0.02
            str10 <- cor(preds_nnet[vals], test_targ[vals])
            sharp10 <- log(preds_nnet[vals])*log(test_targ[vals])
            sharp10  <- mean(sharp10)/sd(sharp10) * sqrt(256*sum(vals)/length(vals))

            vals <- abs(preds_nnet-1) > 0.05
            str11 <- cor(preds_nnet[vals], test_targ[vals])
            sharp11 <- log(preds_nnet[vals])*log(test_targ[vals])
            sharp11  <- mean(sharp11)/sd(sharp11) * sqrt(256*sum(vals)/length(vals))

           str <- cor(preds, test_targ)
          stream <- log(preds)*log(test_targ)
          sharp <- mean(stream)/sd(stream) * sqrt(256)
          #merged <- (preds_gam)
          #sharp6 <- log(merged) *log(test_targ)
          #sharp6 <- mean(sharp6)/sd(sharp6) * sqrt(256)
          #str6 <-  cor(merged, test_targ)
          #if(lmp(mod_lm) < 0.05)
          print(paste(i,w,   str, sharp, str9, sharp9,str12,sharp12,str10,sharp10,str11,sharp11,length(test_targ), sep = ","))
      } 
    # }, error=function(e){})
      }
    }
}




