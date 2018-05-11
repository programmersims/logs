syms <- c("RETL","TQQQ", "SSO", "FLGE", "UPRO","FBGX","BIB", "SDOW","DIG","KNOW","INDL","RUSS","CWEB","CHAU","NAIL","DRN","TECL","ERC","ERY","TMF","DRIP","LABU","YINN","CURE","FAS","MIDU","SRTY","DUG","TNA","SOXL","JPNL","BRZU","VXX","GDXJ","GBTC", "GDX")
options(warn=-1)

args <- commandArgs(TRUE)
if(length(args) != 0){
  syms <- syms[(as.numeric(args[1])):(as.numeric(args[2]))]
} else{
   system(paste("python vxx_daily5.py --lista", (paste(syms, collapse=" ")), "--time_in_minutes 390"))
   seq1 <- ceiling(seq(1, length(syms),length.out=9))
   for(i in 1:8){
     if(i != 8){
        system(paste("Rscript cron_new_etfs.R", seq1[i], (seq1[i+1]-1)), wait = FALSE)
     } else{
     system(paste("Rscript cron_new_etfs.R", seq1[i], (seq1[i+1])), wait = TRUE)
     Sys.sleep(300)
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
    stock <- read.csv(paste(name, ".csv", sep=""), header = FALSE)
    stock$V8 <- NULL
    stock$V7 <- NULL
    #stock <- stock[nrow(stock):1,]
    colnames(stock) <- c("time", "high", "low", "open", "close", "volume")
    stock$change <- stock$close/stock$open
    stock$range <- stock$high/stock$low
    stock$range_var <- get_variance(stock$range)
    stock$change_var <- get_variance(stock$change)
    stock$volume_var <- get_variance(stock$volume)
    stock <- stock[101:nrow(stock),]
    stock$norm_range <- (stock$range-mean(stock$range))/stock$range_var
    stock$norm_change <- (stock$change-1)/stock$change_var
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

  for(i in syms){
    tryCatch( {
          stock <- clean_data(i)  }, error=function(e){})
          targ <- stock$change[46:nrow(stock)]
          stock <- get_data_prev2(stock, "close", 0,2)/(stock$change_var*sqrt(3))
          #stock <- get_data_prev2(stock, "close", 0,1)
          stock <- get_data_prev2(stock, "close", 0,5)/(stock$change_var*sqrt(6))
          stock <- get_data_prev2(stock, "close", 5,10)/(stock$change_var*sqrt(3))
          stock <- get_data_prev2(stock, "close", 10,15)/(stock$change_var*sqrt(6))
          stock <- get_data_prev2(stock, "close", 15,20)/(stock$change_var*sqrt(6))
          stock <- get_data_prev2(stock, "close", 20,30)/(stock$change_var*sqrt(11))
          stock <- stock[45:nrow(stock),]
          stock$c5_10 <- (stock$close_prev_5*stock$close_prev_10)/sqrt(2)
          stock$c15_10 <- (stock$close_prev_10*stock$close_prev_15)/sqrt(2)
          stock$c15_30 <- (stock$close_prev_15*stock$close_prev_20)/sqrt(2)
          stock$c20_30 <- (stock$close_prev_20*stock$close_prev_30)/sqrt(2)
          #targ <- stock$change[2:nrow(stock)]
          #stock <- cbind(stock$time,normalize(stock[,2:ncol(stock)]))
	  colnames(stock)[1] <- "time"
          stock <- stock[,c(12:ncol(stock))]
          train <- stock[1:(nrow(stock)-1),]
          test <- stock[(nrow(stock)),]
	  print(colnames(train))
          stds <- numeric(16)
          preds_nnet <- numeric(1)
          for(j in 1:16){
              invisible(capture.output(nnet1 <- nnet(x=as.matrix(train), y=targ, linout=TRUE, size=12, maxit=350, decay=0.03, MaxNWts = 2000, weights = (seq(1,3.5,length.out = nrow(train))))))
              stds[j] <- predict(nnet1, as.matrix(test))
              preds_nnet <- predict(nnet1, as.matrix(test))/16+preds_nnet
          }
          str <- paste(i,": ",preds_nnet[1], sd(stds))
          print(str)
          saveRDS(str, file=paste(i,"10.rds", sep=""))
     #}, error=function(e){})
    }
}




