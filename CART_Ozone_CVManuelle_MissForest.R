data("Ozone", package = "mlbench")

# Imputation des NA du dataset avec missForest
library(missForest)
set.seed(123)
Ozone_imputed <- missForest(Ozone)$ximp

# construction Amax(cp=0)
library(rpart)
Arbre0 <- rpart(V4 ~ ., data = Ozone_imputed, method = "anova", cp = 0)

# Grille de cp fixe indépendante des données

cp_grid <- seq(0, 0.5, by = 0.001)

# K-Fold Cross Validation manuelle avec grille fixe

set.seed(123)

n <- nrow(Ozone_imputed)
K <- 10

folds <- sample(rep(1:K, length.out = n))
errors <- matrix(NA, nrow = K, ncol = length(cp_grid))

for (k in 1:K) {
  train_data <- Ozone_imputed[folds != k, ]
  test_data  <- Ozone_imputed[folds == k, ]
  tree <- rpart(V4 ~ ., data = train_data, method = "anova", cp = 0)

  for (i in 1:length(cp_grid)) {
    pruned_tree <- prune(tree, cp = cp_grid[i])
    pred <- predict(pruned_tree, test_data)
    errors[k, i] <- mean((test_data$V4 - pred)^2)
  }
}

cv_error <- colMeans(errors)
cv_sd    <- apply(errors, 2, sd)

best_index <- which.min(cv_error)
best_cp    <- cp_grid[best_index]

# 1-SE
threshold     <- cv_error[best_index] + cv_sd[best_index]
candidats     <- which(cv_error <= threshold)
best_index_SE <- max(candidats)
best_cp_SE    <- cp_grid[best_index_SE]

# Construction arbres finaux sur toutes les données imputées
final_tree    <- prune(Arbre0, cp = best_cp)
final_tree_SE <- prune(Arbre0, cp = best_cp_SE)

# Affichage

library(rpart.plot)

plotcp(Arbre0)
rpart.plot(Arbre0)
summary(Arbre0)

rpart.plot(final_tree)
rpart.plot(final_tree_SE)

cat("cp optimal (min erreur) :", best_cp, "\n")
cat("cp optimal (règle 1-SE) :", best_cp_SE, "\n")

# Courbe d'erreur CV en fonction de cp
plot(cp_grid, cv_error, type = "l", xlab = "cp", ylab = "MSE CV",
     main = "CV manuelle - grille fixe")
lines(cp_grid, cv_error + cv_sd, lty = 2, col = "grey")
lines(cp_grid, cv_error - cv_sd, lty = 2, col = "grey")
abline(v = best_cp,    col = "blue",  lty = 2, lwd = 2)
abline(v = best_cp_SE, col = "red",   lty = 2, lwd = 2)
legend("bottomright", legend = c("min erreur", "1-SE"),
       col = c("blue", "red"), lty = 2)
