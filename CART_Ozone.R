data("Ozone", package = "mlbench")
str(Ozone)

y <- Ozone$V4
X <- Ozone[, -4] #on enlève la 4e colonne V4 qu'on veut expliquer 

#construction Amax(cp=0) + affichage de l'arbre 
library(rpart)
Arbre0 <-rpart(V4 ~ ., data = Ozone, method = "anova", cp = 0)
print(Arbre0)

library(rpart.plot)
rpart.plot(Arbre0)
summary(Arbre0)

#Pruning
#construction suite arbres imbriqués 
cp_table <- Arbre0$cptable
print(cp_table)
#l'ordre des cp(t) calculé sur Arbre0 est invariant car les sous arbres sont indépendants. induit directement la séquence optimale et correspond exactement à l’élagage itératif
arbres <- list()
for(i in 1:nrow(cp_table)) {
  cp_val <- cp_table[i, "CP"]
  arbres[[i]] <- prune(Arbre0, cp = cp_val)}

#cross validation
best_index <- which.min(cp_table[, "xerror"])
best_tree <- arbres[[best_index]] 
rpart.plot(best_tree)
print(best_index)

#1-SE
min_error <- min(cp_table[, "xerror"])
se <- cp_table[which.min(cp_table[, "xerror"]), "xstd"]
candidats <- which(cp_table[, "xerror"] <= min_error + se)
best_index_SE <- min(candidats) #on prend le plus simple (index bas = arbre avec moins de split)
best_tree_SE <- arbres[[best_index_SE]]
rpart.plot(best_tree_SE)
print(best_index_SE)