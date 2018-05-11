out <- read.csv("output.csv")
mean2 <- function(x){ mean(x,na.rm=TRUE) }
print(apply(out[,4:ncol(out)],2,mean2))
