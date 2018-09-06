# Projet de 5IBD en Math avec R
# Groupe 4 (Florian PAPIN, Ghiles CHERFAOUI, Thomas DUDOUX)


# Added Matrix package
# Added psych package


# Exercice 2

# Définition du modèle
work_model <- function (employed, non_employed, epochs) {
  for (e in 1:epochs) {
    employed <- employed * 0.9 + non_employed * 0.4
    non_employed <- employed * 0.1 + non_employed * 0.6
  }
  return(c(employed, non_employed))
}

# 1. Taux de chomage à long terme (1000 ans) et une forte pop 
work_pop = work_model(2000, 1600, 1000)
print("Taux de chomage : ")
print(((work_pop[2] / sum(work_pop)) * 100))

# 2. Pour x 0 = (20, 16) , tracer x k , pour les 15 premières années.
work_pop = work_model(20, 16, 15)
print("Taux de chomage : ")
print(((work_pop[2] / sum(work_pop)) * 100 ))

# On remarque que cela se stabilise vers 20%
