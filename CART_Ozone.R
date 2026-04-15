data("Ozone", package = "mlbench")

# construction Amax(cp=0) + affichage de l'arbre 
library(rpart)
Arbre0 <-rpart(V4 ~ ., data = Ozone, method = "anova", cp = 0)

library(rpart.plot)
rpart.plot(Arbre0)

# Pruning

# Construction suite arbres imbriqués 
cp_table <- Arbre0$cptable
print(cp_table) # L'ordre des cp(noeuds) calculé sur Arbre0 est invariant car les sous arbres sont indépendants. Correspond exactement à l’élagage itératif car l'erreur globale est la somme des erreurs des noeuds. 
arbres <- list()
for(i in 1:nrow(cp_table)) {
  cp_val <- cp_table[i, "CP"]
  arbres[[i]] <- prune(Arbre0, cp = cp_val)}

# Cross validation
best_index <- which.min(cp_table[, "xerror"])
best_tree <- arbres[[best_index]] 
rpart.plot(best_tree) # Arbre CV
print(best_index)

# CV 1-SE
min_error <- min(cp_table[, "xerror"])
se <- cp_table[which.min(cp_table[, "xerror"]), "xstd"]
candidats <- which(cp_table[, "xerror"] <= min_error + se)
best_index_SE <- min(candidats) #on prend le plus simple (index bas = arbre avec moins de split)
best_tree_SE <- arbres[[best_index_SE]]
rpart.plot(best_tree_SE) # Arbre CV 1-SE
print(best_index_SE)

plotcp(Arbre0) 
