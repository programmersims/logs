cryptos <- c("btc","dash","xmr","btg","ada","eth","xem","neo","doge","dcr","ltc","bch","etc")
for(i in cryptos){
	print(paste("Downloading" i))
	coin <- read.csv(url(paste0("https://coinmetrics.io/data/", i,  ".csv")))
	write.csv(coin,paste0("cryptos/",i,".csv"))
}
