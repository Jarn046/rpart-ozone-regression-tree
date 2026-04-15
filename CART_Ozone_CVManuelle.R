data("Ozone", package = "mlbench")
str(Ozone)

y <- Ozone$V4
X <- Ozone[, -4]

# construction Amax(cp=0) + extraction cp table + affichage de l'arbre 
library(rpart)
Arbre0 <-rpart(V4 ~ ., data = Ozone, method = "anova", cp = 0)
cp_values <- Arbre0$cptable[, "CP"]

# K-Fold Cross Validation manuelle

set.seed(123)  # reproductibilité

n <- nrow(Ozone)
K <- 10  # nb Fold standard, ajustable

# Création des folds + matrice d'erreurs 
folds <- sample(rep(1:K, length.out = n))
errors <- matrix(NA, nrow = K, ncol = length(cp_values))

for (k in 1:K) {
  train_data <- Ozone[folds != k, ] # Séparation train / test
  test_data  <- Ozone[folds == k, ]
  tree <- rpart(V4 ~ ., data = train_data, method = "anova", cp = 0) # Construiction Amax sur train
  
  for (i in 1:length(cp_values)) {
    pruned_tree <- prune(tree, cp = cp_values[i]) # Arbre élagué
    pred <- predict(pruned_tree, test_data) # Prédiction 
    valid <- !is.na(test_data$V4)
    errors[k, i] <- mean((test_data$V4[valid] - pred[valid])^2) # Remplissage de la matrice d'erreurs
  }
}

cv_error <- colMeans(errors) # Moyennes sur les folds des erreurs pour chaque cp
cv_sd <- apply(errors, 2, sd) # Ecart-type sur les folds des erreurs pour chaque cp

best_index <- which.min(cv_error)

# 1-SE
threshold <- cv_error[best_index] + cv_sd[best_index]
candidats <- which(cv_error <= threshold)
best_index_SE <- min(candidats)

# Construction arbres final (min error, min error +- 1SE) sur toutes les données
final_tree <- prune(Arbre0, cp = cp_values[best_index])
final_tree_SE <- prune(Arbre0, cp = cp_values[best_index_SE])

# Affichage 
library(rpart.plot)

plotcp(Arbre0)
rpart.plot(Arbre0)
summary(Arbre0)

rpart.plot(final_tree)
rpart.plot(final_tree_SE)

print(best_index)
print(best_index_SE)