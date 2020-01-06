

check_stationarity = function(data, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  plot(data, type = "l", main = title, xlab = xlab, ylab = ylab)
  len = length(data)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  acf(data[1:len_by_2])
  acf(data[seg_2_start:len])
}