# Projet de 5IBD en Math avec R
# Groupe 4 (Florian PAPIn, Ghiles CHERFAOUI, Thomas DUDOUX)


# Added Matrix package
# Added psych package

# Exercice 3
# Ne faire Q1 car non diagonalisable (on a pas vu)

# 2. Cas ε = 8
# 2.a Définition de la matrice S2 et démonstration de sa diagonalisation 
S2 <- matrix(c(0, 2, -2, -8), 2, 2, byrow=TRUE)
S2_dp <- eigen(S2)
S2 == round(S2_dp$vectors %*% diag(S2_dp$values, 2, 2) %*% solve(S2_dp$vectors), 0)  # La matrice est diagonalisable car égale (S2 = PDP^−1)

# 2.b Résoudre ce système localement à (0, 0) avec comme condition initiale x(0) = 1, y(0) = 0
res_S2 <- matrix(c(1, 0), 2, 1, TRUE)
S2_solutions <- matrix(0, 2, 10)  # Pexp(Dt)P^−1X0
# Calcul de la solution spécifique et général pour X(t)
for (t in 1:10) {
  S2_solutions[, t] <- matrix((S2_dp$vectors %*% expm(diag(S2_dp$values*t)) %*% solve(S2_dp$vectors) %*% res_S2), 1, 2)
}
S2_plot <- function(mx) {
  df <- data.frame(mx)
  plot(df[1, 1:10], df[2, 1:10])
}
S2_plot(S2_solutions)

# 2.c A l'aide des fonctionnalités de R, résoudre directement (S)
RS2_solutions <- solve(S2, res_S2)
