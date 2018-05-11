path = "~/mods/logs/svm/"
out.file<-""
file.names <- dir(path, pattern =".txt")

for(i in 1:length(file.names)){
  file <- read.csv(paste0(path,file.names[i]),header=FALSE)
  if(file[nrow(file),2]<.99){
	j <- file.names[i]
	max1 <- file[nrow(file),2]
	print(j)
	print(file[nrow(file),2])
  }
}




