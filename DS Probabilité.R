####################### DS TP - Probabilités 2 #################
#### MUGISHA
#### DONNY DUVAL NOE

########## Exercice 1 - Loi de Student ##########

### Question 1 ---
# Fonction de densité de la loi de Student
student <- function(t, k) {
  num <- gamma((k + 1)/2)
  den <- sqrt(k * pi) * gamma(k/2) * (1 + t^2/k)^((k + 1)/2)
  return(num/den)
}

### Question 2 ---
# Vérification avec la fonction dt() de R
t_values <- seq(-3, 3, by = 0.5)
k_test <- 5
verif <- data.frame(
  t = t_values,
  notre_fonction = student(t_values, k_test),
  fonction_R = dt(t_values, df = k_test)
)
print("Vérification de la fonction student:")
print(verif)

### Question 3 ---
# Représentation graphique pour k = 30
curve(student(x, 30), from = -4, to = 4, 
      main = "Densité de la loi de Student (k=30)",
      xlab = "t", ylab = "Densité",
      col = "blue", lwd = 2)

### Question 4 ---
# Calcul de l'aire sous la courbe
aire <- integrate(function(x) student(x, 30), lower = -Inf, upper = Inf)
print(paste("Aire sous la courbe:", aire$value, "(devrait être 1)"))

### Question 5 ---
# Construction d'une variable aléatoire T de la loi de Student:
# T = Z / sqrt(U/k) où:
# - Z suit une loi normale centrée réduite N(0,1)
# - U suit une loi du chi² à k degrés de liberté
# - Z et U sont indépendantes

### Question 6 ---
# Simulation des échantillons pour Z et U (k=50)
set.seed(123) # Pour la reproductibilité
n <- 1000
k <- 50
Z <- rnorm(n) # Loi normale standard
U <- rchisq(n, df = k) # Loi du chi² à k degrés de liberté

### Question 7 ---
# Construction de l'échantillon de T
T_simule <- Z / sqrt(U/k)

### Question 8 ---
# Représentation graphique de la distribution simulée de T
hist(T_simule, prob = TRUE, breaks = 30, 
     main = "Distribution simulée de T (k=50)",
     xlab = "t", ylab = "Densité",
     col = "lightblue")

### Question 9 ---
# Ajout de la courbe de la loi théorique
curve(dt(x, df = k), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Simulée", "Théorique"), 
       fill = c("lightblue", "red"))

### Question 10 ---
# Visualisation pour plusieurs degrés de liberté
par(mfrow = c(1, 3)) # 3 graphiques côte à côte

k_values <- c(50, 100, 200)
for (k in k_values) {
  # Simulation
  Z <- rnorm(n)
  U <- rchisq(n, df = k)
  T_simule <- Z / sqrt(U/k)
  
  # Graphique
  hist(T_simule, prob = TRUE, breaks = 30, 
       main = paste("k =", k),
       xlab = "t", ylab = "Densité",
       col = "lightblue", xlim = c(-4, 4), ylim = c(0, 0.4))
  curve(dt(x, df = k), add = TRUE, col = "red", lwd = 2)
}

# Réinitialisation de la fenêtre graphique
par(mfrow = c(1, 1))

########## Exercice 2 - Files d'attente ##########

### Question 1 ---
# Simulation du temps pour traiter 5 personnes (1 guichet)
lambda1 <- 1/15
temps_5_personnes <- sum(rexp(5, rate = lambda1))
print(paste("Temps pour 5 personnes:", round(temps_5_personnes, 2), "minutes"))

### Question 2 ---
# Simulation 1000 fois et calcul du temps moyen
n_sim <- 1000
simulations <- replicate(n_sim, sum(rexp(5, rate = lambda1)))
temps_moyen <- mean(simulations)
print(paste("Temps moyen pour 5 personnes:", round(temps_moyen, 2), "minutes"))
# Commentaire: On s'attend à 5 * 15 = 75 minutes car E[X] = 1/lambda = 15 pour une personne

### Question 3 ---
# Deuxième guichet (6 personnes, lambda = 1/17)
lambda2 <- 1/17
simulations_guichet2 <- replicate(n_sim, sum(rexp(6, rate = lambda2)))
temps_moyen_guichet2 <- mean(simulations_guichet2)
print(paste("Temps moyen pour 6 personnes (guichet 2):", 
            round(temps_moyen_guichet2, 2), "minutes"))

### Question 4 ---
# Probabilité que le guichet 2 termine avant le guichet 1
prob_guichet2_avant <- mean(simulations_guichet2 < simulations)
print(paste("Probabilité que le guichet 2 termine en premier:", 
            round(prob_guichet2_avant, 4)))

### Question 5 ---
# Temps jusqu'à fermeture de l'agence (les deux guichets)
temps_fermeture <- pmax(simulations, simulations_guichet2)
temps_moyen_fermeture <- mean(temps_fermeture)
print(paste("Temps moyen avant fermeture (cas de base):", 
            round(temps_moyen_fermeture, 2), "minutes"))

### Question 6 ---
# Nouveau temps espéré avant que l'agence ne puisse fermé
simul_acceleration <- function() {
  temps1 <- sum(rexp(5, rate = lambda1))
  temps2 <- sum(rexp(6, rate = lambda2))
  
  if (temps1 < temps2) {
    # Guichet 1 termine en premier -> guichet 2 accélère
    temps2 <- temps1 + sum(rexp(1, rate = 1/10)) * (6 - (temps1 / (1/lambda2)))
  } else {
    # Guichet 2 termine en premier -> guichet 1 accélère
    temps1 <- temps2 + sum(rexp(1, rate = 1/10)) * (5 - (temps2 / (1/lambda1)))
  }
  
  return(max(temps1, temps2))
}

temps_accelere <- replicate(n_sim, simul_acceleration())
temps_moyen_accelere <- mean(temps_accelere)
print(paste("Temps moyen avant fermeture (avec accélération):", 
            round(temps_moyen_accelere, 2), "minutes"))

### Question 7 ---
# Ajout d'un client difficile (lambda = 1/30)
lambda4 <- 1/30
simul_client_difficile <- function() {
  # Position aléatoire du client difficile
  pos_difficile1 <- sample(1:5, 1)
  pos_difficile2 <- sample(1:6, 1)
  
  # Guichet 1
  temps1 <- sum(c(
    rexp(pos_difficile1 - 1, rate = lambda1),
    rexp(1, rate = lambda4),
    rexp(5 - pos_difficile1, rate = lambda1)
  ))
  
  # Guichet 2
  temps2 <- sum(c(
    rexp(pos_difficile2 - 1, rate = lambda2),
    rexp(1, rate = lambda4),
    rexp(6 - pos_difficile2, rate = lambda2)
  ))
  
  return(max(temps1, temps2))
}

temps_client_difficile <- replicate(n_sim, simul_client_difficile())
temps_moyen_difficile <- mean(temps_client_difficile)
print(paste("Temps moyen avant fermeture (avec client difficile):", 
            round(temps_moyen_difficile, 2), "minutes"))