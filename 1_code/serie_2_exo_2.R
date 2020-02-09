#################################################################################################################################
### Description: Solutions de l'exercice 2 de la série 2                                                                      ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Février 2020                                                                                                        ###
#################################################################################################################################

library(here) # Package utile pour se débarasser des setwd(...)
library(tidyverse) # Un ensemble de plusieurs packages très utiles. 
library(magrittr) # Pour utiliser le piping operator "%<>%". Voir: https://r4ds.had.co.nz/pipes.html
library(FNN) # Package pour l'algorithme KNN
options(scipen = 999) # Pour ne pas avoir de notation scientifique


# Importer les données ----------------------------------------------------------------------------------------------------------
d <- read_csv(here("0_data", "dataEXO1.csv"))


# 1) ----------------------------------------------------------------------------------------------------------------------------

# Nuage de points
plot(d$X, d$Y, xlab = "x", ylab = "y", cex = 0.6, pch = 20)

# Ajuster le droite des moindres carrés
lin_fit <- lm(Y ~ X, data = d)
summary(lin_fit) # Les 2 paramètres sont significatifs parce que leur valeur-p est très petite.

# Visualiser l'ajustement sur le nuage de ponts
lines(d$X, lin_fit$fitted.values, col = "darkgreen", lw = 1.5)

# Calculer l'erreur quadratique moyenne (EQM), ou mean squared error en anglais (MSE)
mean((d$Y - lin_fit$fitted.values) ^ 2)


# 2) ----------------------------------------------------------------------------------------------------------------------------

# Ajout de x au carré comme prédicteur
d %<>% mutate(X_2 = X ^ 2) 

# Ajustement du modèle quadratique
quad_fit <- lm(Y ~ X + X_2, data = d)
summary(quad_fit) # Maintenant, seul 1 paramètre sur 3 est significativement différent de zéro

# Visualiser l'ajustement sur le nuage de ponts
lines(d$X, quad_fit$fitted.values, col = "blue", lw = 1.5)

# Calculer l'erreur quadratique moyenne
mean((d$Y - quad_fit$fitted.values) ^ 2) 


# 3) ----------------------------------------------------------------------------------------------------------------------------

# Entrainer les modèles pour K = 1, ..., 20 (la fonction map c'est un peu comme lapply, mais meilleur)
knn_fits <- map(1:20, ~ knn.reg(train = d$X, y = d$Y, k = .x)) 

# Obtenir les prédictions des 20 modèles
knn_pred <- map(knn_fits, ~ .x$pred)

# Obtenir le MSE pour chacun des 20 modèles
knn_mse <- map_dbl(knn_pred, ~ mean((.x - d$Y) ^ 2)) 

# K = 10 minimise le MSE d'entrainement
(opt_k <- which.min(knn_mse)) 

# Le MSE d'entrainement du modèle avec K = 10 est de 5 723 886 929
knn_mse[opt_k] 

x_grid <- tibble(X = (seq(min(d$X), max(d$X), length.out = 1000))) # Créer des valeurs de X pour la prédiction
knn_fit <- knn.reg(train = d["X"], test = x_grid, y = d$Y, k = opt_k) # Ajuster le modèle avec K = 10

lines(x_grid$X, knn_fit$pred, col = "red", lw = 1.5)
legend(
  "topleft", 
  legend = c("Modèle linéaire", "Modèle quadratique", "10-NN"), 
  col = c("darkgreen", "blue", "red"), 
  lwd = 1.5,
  bty = "n"
)


# 4) ----------------------------------------------------------------------------------------------------------------------------

# Fonction qui calcule le MSE avec leave one out cross-validation. x_train et y_train doivent être des vecteurs.
loocv_knn_1d <- function(x_train, y_train, k) {
  n <- length(x_train)
  preds <- map_dbl(1:n, ~ knn.reg(train = x_train[-.x], test = x_train[.x], y = y_train[-.x], k = k)$pred) 
  mse <- mean((preds - y_train) ^ 2)
  return(mse)
}


# Calculer le LOOCV mse pour K = 1, ..., 20
knn_loocv_mse <- map_dbl(1:20, ~ loocv_knn_1d(d$X, d$Y, k = .x))

# Déterminer la valeur de K optimale
opt_k_loocv <- which.min(knn_loocv_mse)

# Le MSE LOOCV du modèle avec K = 10 est de 5 723 886 929
# Pour le KNN, calculer le MSE d'entrainement est équivalent à calculer le MSE LOOCV
knn_loocv_mse[opt_k_loocv]


# 5) ----------------------------------------------------------------------------------------------------------------------------
x0 <- 333.2522
y0 <- 99508.44
newdata <- tibble(X = x0, X_2 = x0 ^ 2)

# Modèle linéaire
(y_hat_lin_fit <- predict(lin_fit, type = "response", newdata = newdata) %>% as.numeric()) # Prédiction
(y_hat_lin_fit - y0) ^ 2 # Erreur quadratique de prédiction

# Modèle quadratique
(y_hat_quad_fit <- predict(quad_fit, type = "response", newdata = newdata) %>% as.numeric()) # Prédiction
(y_hat_quad_fit - y0) ^ 2 # Erreur quadratique de prédiction

# 10-NN
y_hat_10_nn <- knn.reg(train = d$X, test = x0, y = d$Y, k = opt_k)$pred # Prédiction
(y_hat_10_nn - y0) ^ 2 # Erreur quadratique de prédiction
