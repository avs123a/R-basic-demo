
#custom sorting function
cSort <- function(vector, dec = FALSE, na.last = NA, ...) {
  res = c()
  if(is.object(vector)) res <- vector[order(vector, na.last = na.lasr, dec = dec, ...)]
  else res <- sort.int(vector, dec = dec, na.last = na.last, ...)
  return(res)
}

#index search function
indexOf <- function(vect, el){
  # indx <- which(vect==el)
  # return(indx)
  for(i in 1:length(vect)){
    if(vect[i] == el) return (i)
  }
}

#random numbers
test_vector = as.integer(rnorm(30, 30, 10))
print('start random vector')
print(test_vector)
print('sorted vector')
print(cSort(test_vector))

el = 30
print(paste('index Of el = ', el))
print(indexOf(test_vector, el))

# #Import airquality dataset
library(datasets)
data(airquality)
print(airquality)
# print(airquality['Ozone'])

#monthly average temperature
tempAndMonthVector = airquality[,4:5];
avgTemp <- function (data, monthNumber) {
	sum = 0;
	count = 0;
	for(row in 1:nrow(data)) {
		if (data[row, 2] == monthNumber) {
			sum = sum + data[row, 1];
			count = count + 1;
		}
	}
	return (sum / count)
}
print(averageTempPerMonth(tempAndMonthVector, 5));

# avgTemp = function (data) {
#   return(aggregate(data['Temp'], list(data$Month), mean))
# }
print('monthly avg temperature')
print(avgTemp(airquality));

#average wind speed if solar radiation < 100
ws = function(data, maxSolarR) {
  return(aggregate(data['Wind'], list(data['Solar.R'] < maxSolarR), mean))
}
print('average wind speed if solar radiation < 100')
print(ws(airquality, 100));

#month of max ozone concetration
ozMax <- function(data){
  oz <- max(data$Ozone, na.rm = T)
  return(data[which(data$Ozone == oz), ]['Month'])
}
print('month of max ozone concetration')
print(ozMax(airquality))
