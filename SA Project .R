# install.packages("devtools")

install.packages("devtools")
devtools::install_github("jlmelville/mnist")
library(mnist)

#devide "mnist Data" into --> "mnist_train" and "mnist_test"
#"mnist_train" ----> first 60000 rows 
#"mnist_test" -----> last 10000 rows to test our train pictures 

mnist_train <- head(mnist, 60000)
mnist_test <- tail(mnist, 10000)

# declared 9 dataframes "Empty" to store pictures from 0 to 9 as a Matrix 

x0=data.frame();x1=data.frame();x2=data.frame();x3=data.frame();x4=data.frame();x5=data.frame();x6=data.frame();x7=data.frame();x8=data.frame();x9=data.frame()

#  include label column at mnist_train --->" the last column at mnist Data "

k<-mnist_train[1:60000,785]

# for loop to full of Dataframes of the same type of picture --->0 to 0 and 1 to 1 and so on 
# rbind function ---> to Append on existing dataframe(Add new row) and every row in dataframe represent one picture (from : 0 to 9)

for( i in 1:60000)
{
  if(0==k[i]) {df <- data.frame(mnist_train[i,1:785]);x0 <- rbind(x0,df)}
  else if(1==k[i]){df <- data.frame(mnist_train[i,1:785]);x1 <- rbind(x1,df)} 
  else if(2==k[i]){df <- data.frame(mnist_train[i,1:785]);x2 <- rbind(x2,df)}
  else if(3==k[i]){df <- data.frame(mnist_train[i,1:785]);x3 <- rbind(x3,df)}
  else if(4==k[i]){df <- data.frame(mnist_train[i,1:785]);x4 <- rbind(x4,df)}
  else if(5==k[i]){df <- data.frame(mnist_train[i,1:785]);x5 <- rbind(x5,df)}
  else if(6==k[i]){df <- data.frame(mnist_train[i,1:785]);x6<- rbind(x6,df)}
  else if(7==k[i]){df <- data.frame(mnist_train[i,1:785]);x7 <- rbind(x7,df)}
  else if(8==k[i]){df <- data.frame(mnist_train[i,1:785]);x8<- rbind(x8,df)}
  else if(9==k[i]){df <- data.frame(mnist_train[i,1:785]);x9 <- rbind(x9,df)}
}


# vectors to store the means of pictures from 0 to 9 ---> mean for every column 
# function mean to calculate mean (dataframes---> Error ) so must convert to numeric data 
# (as.numeric) ---> function to convert from dataframe to numeric data 

v0<-c();v1<-c();v2<-c();v3<-c();v4<-c();v5<-c();v6<-c();v7<-c();v8<-c();v9<-c()
for( i in 1:785)
{
  v0<-c(v0,mean(as.numeric (x0[ ,i])));v1<-c(v1,mean(as.numeric (x1[ ,i])));v2<-c(v2,mean(as.numeric (x2[ ,i])))
  v3<-c(v3,mean(as.numeric (x3[ ,i])));v4<-c(v4,mean(as.numeric (x4[ ,i])));v5<-c(v5,mean(as.numeric (x5[ ,i])))
  v6<-c(v6,mean(as.numeric (x6[ ,i])));v7<-c(v7,mean(as.numeric (x7[ ,i])));v8<-c(v8,mean(as.numeric (x8[ ,i])))
  v9<-c(v9,mean(as.numeric (x9[ ,i])))
}

# loop to test pictures in mnist_test by correlation among them and vectors of mean---> (v0 , ......,v9)
# cor(x , y) function --->  to calculate correlation 
# cat(paste(num," ")) --->  to print picture number (for example : 0,1, .....,9) in one row
# plot() ----> Drawing graphs (represent correlation for every picture as plots)
# plot(x , y , type ="h" , main = row_number)---> (type = "h") meaning : drawing as histogram , (main ) meaning : Title page

for(i in 1:10000)
{
  p <- mnist_test[i,1:785]
  p <- as.numeric(p)
  max <- 0
  num <- -1 
  if(cor(p,v0)>max){max=cor(p,v0);num=0}
  if(cor(p,v1)>max){max=cor(p,v1);num=1}
  if(cor(p,v2)>max){max=cor(p,v2);num=2}
  if(cor(p,v3)>max){max=cor(p,v3);num=3}
  if(cor(p,v4)>max){max=cor(p,v4);num=4}
  if(cor(p,v5)>max){max=cor(p,v5);num=5}
  if(cor(p,v6)>max){max=cor(p,v6);num=6}
  if(cor(p,v7)>max){max=cor(p,v7);num=7}
  if(cor(p,v8)>max){max=cor(p,v8);num=8}
  if(cor(p,v9)>max){max=cor(p,v9);num=9}
  cat(paste(num," "))
  correlation <- c(cor(p,v0),cor(p,v1),cor(p,v2),cor(p,v3),cor(p,v4),cor(p,v5),cor(p,v6),cor(p,v7),cor(p,v8),cor(p,v9))
  number<-c("0" ,"1" ,"2","3" ,"4" ,"5","6" ,"7" ,"8","9")
  plot(  number,correlation ,type = "h", main=i)
}
