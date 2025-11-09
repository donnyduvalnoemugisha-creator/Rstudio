# Module -- TP 1 : ...
# Donny MUGISHA
setwd("C:/Users/User/Downloads/TP Test d'hypothèse")
#1.
#2.
set.seed(123)  
x <- rnorm(10000, mean = 0, sd = 1)
#3.
moyenne_globale <- mean(x)
print(moyenne_globale)  
#4.
moyenne <- numeric(10000)  
for (i in 1:10000) {
  moyenne[i] <- mean(x[1:i])  
}
#5.
plot(1:10000, moyenne, type = "l", xlab = "Nombre d'observations (i)", ylab = "Moyenne empirique", main = "Convergence de la moyenne empirique")
plot(1:10000, moyenne, type = "l", xlab = "Nombre d'observations (i)", ylab = "Moyenne empirique", main = "Convergence de la moyenne empirique")
#6.
abline(h = 0, lty = 2, col = "red") 
#7.
