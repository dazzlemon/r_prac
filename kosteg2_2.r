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
barplot(as.matrix(t(data[c(1:3),])),
	beside = TRUE,
	legend.text = TRUE,
	col = seq(5 * n, 5 * n + 10, len = 9),
	main = paste(row.names(data)[c(1:3)]))

barplot(as.matrix(t(data[c(5:7),])),
	beside = TRUE,
	legend.text = TRUE,
	col = seq(5 * n, 5 * n + 10, len = 9),
	density = seq(10, 90, len = 9),
	angle = seq(10, 90, len = 9),
	horiz = TRUE,
	args.legend = list(x = "bottomright", bty = "n"))

#2_2.2
x1 = rnorm(50 + 2 * n, mean = n, sd = 0.2 * n)
x2 = rnorm(40 + 3 * n, mean = 1.1 * n, sd = 0.5 * n)
x3 = rnorm(60 + n, mean = 1.5 * n, sd = 0.1 * n)
x4 = runif(110 - n, min = 0.8 * n, max = 1.2 * n)

boxplot(list(x1, x2, x3, x4),
	names = c("x1", "x2", "x3", "x4"),
	horizontal = TRUE,
	range = 1.2,
	staplewex = 1 / 150,#nihuya ne ponyal kak3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
	notch = TRUE,
	col = c(1:4)
)

boxplot(list(x1 + x4, c(x1, x4), (x2 + x3) / 2),
	names = c("x1 + x4", "x1 & x4", "(x1 + x2) / 2"),
	range = 0.8,
	staplewex = n / (2 * n + 6),
	varwidth = TRUE,
	col = c((n + 3):(n + 5)),
	border = c((n + 6):(n + 8)),
	outwex = 2.5)#33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
boxplot(list(x1 + x4, c(x1, x4), (x2 + x3) / 2), plot = FALSE)

#2_2.3
curve(sin(x), from = -20 / n, to = 20 / n, col = 50 + n, type = "o")
curve(sin(n * x) / cosh(n * x), from = -20 / n, to = 20 / n, col = 30 + n, type = "s", add = TRUE)#v obratnom poryadke 4tobi sinusoida pomestilas

#2_2.4
a1 = 1
a2 = 2
a3 = 3
a4 = 4#zna4eniya pridumay sam
a5 = 5
y = function(x) (a1 * x**2 + a2 * x + a3) / (a4 * x + a5)

curve(y, from = -n, to = n, col = 2 * n, type = "b")
abline(v = -a5 / a4, lty = 3, lwd = 2 + 15 / n, col = 2 * n + 1)
x = seq(-n, n, len = 100)
abline(reg = lm(y(x)~x), lty = 3, lwd = 2 + 15 / n, col = 2 * n + 1)

#2_2.5
theurl = "https://vstup.info/2015/41/i2015i41p240731.html#list"
library(xml2)
library(rvest)
file = read_html(theurl)
tables = html_nodes(file, "table")
table = html_table(tables[3])#3
table = as.data.frame(table)
table = table[(n + 1):(n + 20),]
table
