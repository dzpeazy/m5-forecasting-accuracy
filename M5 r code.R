sales.tr = read.csv("~/Documents/Kaggle Competition Data/Walmart Forecast/sales_train_validation.csv")
prices = read.csv("~/Documents/Kaggle Competition Data/Walmart Forecast/sell_prices.csv")
calendar = read.csv("~/Documents/Kaggle Competition Data/Walmart Forecast/calendar.csv")
test.set = read.csv("~/Documents/Kaggle Competition Data/Walmart Forecast/sales_train_evaluation.csv")
te = test.set[,c(1:6,1920:1947)]
tr.data = sales.tr[,-c(1889:1919)]
te.data = sales.tr[,c(1889:1919)]

#generate model on the first item in the list

sales.tr[1,]
plot(sales.tr[1,-c(1:6)])
s = t(sales.tr[1,-c(1:6)])
par("mar")
par(mar=c(1,1,1,1))
plot(x = 1:1913, y = s)
?ts
s.ts=ts(s)
plot(s.ts)
loess.smooth(1:1913, s,  span = 2/3)
any(is.na(sales.tr))
levels(sales.tr$store_id)
str(sales.tr$state_id)


#create list of variables of independent stores and department id 
var_names = c()
for (store in levels(sales.tr$store_id )) {
  var_names = cbind(var_names, paste(store, levels(sales.tr$dept_id), sep = "."))
}
var_names = as.vector(var_names)
i = 1
variable.list = list()
while (i <= length(var_names)) {
  print(var_names[i])
  filter = unlist(strsplit(var_names[i], '[.]'))
  temp=sales.tr[which(sales.tr$store_id == filter[1] & sales.tr$dept_id == filter[2]),]
  variable.list = append(variable.list, list(temp))
  i = i + 1
}
#create master matrix containing column averages of each category of item at respective store 
data.matrix = as.data.frame(colMeans(variable.list[[1]][,-c(1:6)]))
data.matrix = data.matrix[,-1]
for (list in variable.list) {
  temp = as.data.frame(colMeans(list[,-c(1:6)]))
  data.matrix = cbind(data.matrix, (temp))
}
colnames(data.matrix) = (var_names)
acf(data.matrix[1],4)
ts = (ts(data.matrix[1], frequency = 365))
plot(data.matrix[,1]~c(1:1913))
plot.ts(ts)

#perform time series analysis on each averaged data set 
ts.map <- vector(mode="list", length=70)
names(ts.map) <- var_names
i = 1
while (i <= length(data.matrix)) {
  temp = ts(data.matrix[i], frequency = 365)
  ts.map[[i]] = temp
  i = i +1
}
#predicting test values 
library(forecast)
i = 1
predictions = as.data.frame(temp)
predictions = predictions[-1]
while (i <= length(ts.map)) {
  temp = as.data.frame(predict(ts.map[[i]], h = 28)$mean)
  predictions = cbind(predictions, temp)
  i = i + 1
}
names(predictions) = var_names
#setting predictions to respective items
i = 1
evaluation.matrix = as.data.frame(matrix(c(1:29), 1, 29))[-1,]
names(evaluation.matrix) = append("id", paste("F", 1:28, sep = ""))
while (i <= nrow(te)) {
    evaluation.matrix = rbind(evaluation.matrix, t(predictions[paste(te[i,]$store_id, te[i,]$dept_id, sep = ".")]))
    i = i + 1
}
predictions$CA_1.FOODS_1
predictions["CA_1.FOODS_1"]
ts.predictions = cbind(te$id, evaluation.matrix)
names(ts.predictions) = append("id", paste("F", 1:28, sep = ""))
write.csv(ts.predictions, "ts first attempt.csv")
