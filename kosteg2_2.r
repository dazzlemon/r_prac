n = 10

#2_2.1
data = read.table("ogrizok3.csv", sep = ";", dec =  ",", header = TRUE)
data = data[(3 * n):(3 * n + 6),]
names(data) = c(2011:2019)
data[] = lapply(data, function(x) as.numeric(sub(",", ".", as.character(x), fixed = TRUE)))

for(i in 1:7)
	data[i,] = data[i,] / data[i, 1] * 100

row.names(data) = c("olija", "margrarin", "produkti", "low fat milk", "normal milk", "extra fat milk", "dry milk") 
data
barplot(as.matrix(t(data[c(1:3),])), beside = TRUE, legend.text = TRUE, col = seq(5 * n, 5 * n + 10, len = 9), main = paste(row.names(data)[c(1:3)], sep = ", "))

barplot(as.matrix(t(data[c(5:7),])),
	beside = TRUE,
	legend.text = TRUE,
	col = seq(5 * n, 5 * n + 10, len = 9),
	density = seq(10, 90, len = 9),
	angle = seq(10, 90, len = 9),
	horiz = TRUE,
	args.legend = list(x = "bottomright", bty = "n"))
