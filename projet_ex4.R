# Projet de 5IBD en Math avec R
# Groupe 4 (Florian PAPIN, Ghiles CHERFAOUI, Thomas DUDOUX)


# Added Matrix package
# Added psych package

# Exercice 4

#2
data <- read.table("D:/Downloads/Données voitures.dat", header=TRUE)

x=2.5404^2
y=1.0666^2 
z=0.8105^2 
a=0.5762^2 
b=0.4867^2
(2.5404^2+1.0666^2)*100/9


res.pca <- PCA(data, graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res.pca, col.var = "black")

#question 10
0.4451*sqrt(19/6.4534)*sqrt((62-1)/(62-19))

(-0.6599)*sqrt(19/1.1376)*sqrt((62-1)/(62-19))
