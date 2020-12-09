n = 10
#2_1.1

experiment = c(1:4)

t = seq(0, 10 + 2 * n, len = 12)
time = rep(t, each = 4)

concentration = c()
for(i in 1:12)
        concentration = c(concentration, sqrt(experiment[1:4] + t[i]))

table = data.frame("Experiment" = experiment,
                   "Time" = time,
                   "Concentration" = concentration)
        
print(table, row.names = FALSE, max.levels = 0)
        
for(i in 1:4)
    plot(t, sqrt(t + i), xlab = "TIME", ylab = "CONCENTRATION", main = paste("EXPERIMENT", i))

y = 0
for(i in 1:4) y = y + sqrt(t + i)
y = y / 4
plot(t, y, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE")
    
#2_1.2
type = c("p", "l", "b", "o", "h", "s")

plot(ifelse(t >= n & t <= 2 * n, t, NA),
     ifelse(y >= 2 & y <= 2 * n, y, NA),
     xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", type = "p", col = n)
plot(t, y, xlab = "", ylab = "", main = "AVERAGE", type = "l", col = n + 1)
plot(t, y, xlab = "", ylab = "", main = "AVERAGE", type = "b", col = n + 2, axes = "FALSE")
plot(t, y, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", type = "o", col = n + 3, log = "x")
plot(t, y, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", type = "h", col = n + 4, log = "y")
plot(t, y, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", type = "s", col = n + 5, log = "xy")
    
#2_1.3
x = seq(0, n, by = n / 15)
y_ = sqrt(2 * n - x)
plot(x, y_, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", pch = n, col = n + 30, cex = 2 * n / (n + 1))
plot(x, y_, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", pch = 70 + n, col = "red", cex = (n + 1) / 2 / n)
plot(x, y_, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", pch = "\u0417", col = c("0", "0", "255"))
plot(x, y_, xlab = "TIME", ylab = "CONCENTRATION", main = "AVERAGE", pch = 5, col = c("255", "228", "181"), lwd = 2.5, bg = 22 + n)
    
#2_1.4
trap = function(start, width, height) {
    x = c(start[1],
          start[1] + width,
          start[1] + 3 / 4 * width,
          start[1] + width / 4,
          start[1])
    y = c(start[2],
          start[2],
          start[2] + height,
          start[2] + height,
          start[2])
    list(x, y)
}
l = trap(c(n, n), 3 * n, 2 * n)
m = trap(c(1.2 * n, 1.2 * n), 2.6 * n, 1.6 * n)
s = trap(c(1.5 * n, 1.5 * n), 2 * n, n)
plot(l[[1]], l[[2]], type  =  "l",  col  = n + 5, lwd = 15 + n / 5, lty = 2, xlab = "", ylab = "", ljoin = 0)
lines(m[[1]], m[[2]], type  =  "l",  col  = n + 6, lwd = 15 + n / 5, lty = 3, ljoin = 0)
lines(s[[1]], s[[2]], type  =  "l", col  = n + 7, lwd = 15 + n / 5, lty = 4, ljoin = 0)
    
#2_1.5
c = seq(-(n + 1) / n, (n + 1) / n, len = 100 - n)
plot(x, x, type = "o", lwd = 3, pch = n, lty = 1, col = n + 10)
lines(x, x**3, type = "o", lwd = 3, pch = n + 1, lty = 3, col = n + 11)
lines(x, x**(n+1), type = "o", lwd = 3, pch = n + 2, lty = 5, col = n + 12)
legend(8, 4, c("x", "x**3", "x**(n+1)"), col = c((n + 10):(n + 12)), lty = c(1, 3, 5), pch = c(n:(n+2)))
