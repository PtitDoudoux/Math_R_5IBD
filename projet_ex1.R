# Projet de 5IBD en Math avec R
# Groupe 4 (Florian PAPIN, Ghiles CHERFAOUI, Thomas DUDOUX)


# Added Matrix package
# Added psych package


# Exercice 1

# 1. Définition du vecteur à composante principale à 1

# Pour la première matrice
xk <- c(1, 0)
A <- matrix(c(5, 2, 2, 2), 2, 2)
for (k in 1:1000) {
  Axk <- A %*% xk
  Uk <- max(abs(Axk))
  xk <- (1 / Uk) * Axk
}
print(Uk)
round(Uk, 4) == round(sort(eigen(A)$values, decreasing = TRUE)[1], 4)  # Approximation OK

# Pour la seconde matrice
xk <- c(1, 0)
A <- matrix(c(-3, 2, 2, 2), 2, 2)
for (k in 1:1000) {
  Axk <- A %*% xk
  Uk <- max(abs(Axk))
  xk <- (1 / Uk) * Axk
}
print(Uk)
round(Uk, 4) == round(sort(eigen(A)$values, decreasing = TRUE)[1], 4)  # Approximation OK

# 2. Test pour une matrice symétrique d'ordre 8 et x0 à composantes positives dont la plus grande est 1
xk8 <- c(1, 0, 0, 0, 0, 0, 0, 0)
A8 <- matrix(1:64, 8)
A8[lower.tri(A8)] = t(A8)[lower.tri(A8)]

for (k in 1:1000) {
  A8xk <- A8 %*% xk8
  Uk8 <- max(abs(A8xk))
  xk8 <- (1 / Uk8) * A8xk
}
print(Uk8)
round(Uk8, 4) == round(sort(eigen(A8)$values, decreasing = TRUE)[1], 4)  # Approximation OK
