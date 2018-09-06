# Projet de 5IBD en Math avec R
# Groupe 4 (Florian PAPIN, Ghiles CHERFAOUI, Thomas DUDOUX)


# Added Matrix package
# Added psych package

# Exercice 3
# Ne faire Q1 car non diagonalisable (on a pas vu)

# 2. Cas ε = 8
# 2.a Définition de la matrice S2 et démonstration de sa diagonalisation 
S2 <- matrix(c(0, 2, -2, -8), 2, 2, byrow=TRUE)
S2_dp <- eigen(S2)
S2 == round(S2_dp$vectors %*% diag(S2_dp$values, 2, 2) %*% solve(S2_dp$vectors), 0)  # La matrice est diagonalisable car égale (S2 = PDP^−1)

# 2.b Résoudre ce système (S2) localement à (0, 0) avec comme condition initiale x(0) = 1, y(0) = 1
res_S2 <- matrix(c(1, 1), 2, 1, TRUE)
S2_solutions <- matrix(0, 2, 10)  # Pexp(Dt)P^−1X0
# Calcul de la solution spécifique pour S(t)
for (t in 1:10) {
  tt <- t / 100
  S2_solutions[, t] <- matrix((S2_dp$vectors %*% expm(diag(S2_dp$values*tt)) %*% solve(S2_dp$vectors) %*% res_S2), 1, 2)
}
S2_plot <- function(mx) {
  df <- data.frame(mx)
  xd = unlist(df[1, 1:10])
  yd = unlist(df[2, 1:10])
  plot(xd, yd, type='b')
}
S2_plot(S2_solutions)

# 2.c A l'aide des fonctionnalités de R, résoudre directement (S)
# On ne peut pas résoudre car calcul non matricielle (non linéaire)
S <- matrix(c(0, 2, -2-4^3, -8), 2, 2, byrow=TRUE)

