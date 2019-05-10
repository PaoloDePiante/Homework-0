library(dslabs)
library(caret)
library(tidyverse)
data("swiss")
str(swiss)
dim(swiss)

write.table(swiss, file="swiss.csv", quote=T, sep=";", dec=",", na="NA", row.names=T, col.names=T)

# Visto che le colonne della amtrice originale si riferiscono a diverse unità di misura, Standardiziamo la Matrice di partenza
# Questo dovrebbe ridurre anche l'indice RMSE
swiss <- scale(swiss) # remmare questa riga e vedere le differenze

swiss.svd = svd(swiss)
class(swiss.svd)   #list
plot(swiss.svd$d^2/sum(swiss.svd$d^2), type="l", xlab="Singular Vector", ylab = "Varianza spiegata")
swiss.svd$d
swiss.svd$u
swiss.svd$v

sum(swiss.svd$d * swiss.svd$u[5, ] * swiss.svd$v[2, ]) # valore per la 5 riga 2 colonna matrice originale
sum(swiss.svd$d * swiss.svd$u[6, ] * swiss.svd$v[1, ]) # valore per la 6 riga 1 colonna matrice originale
swiss.svd$u %*% diag(swiss.svd$d) %*% t(swiss.svd$v) # ricrea la matrice originale

variance.explained = prop.table(swiss.svd$d^2) # varianza spiegata dalle colonne delle matrici  U (righe mat. originaria) e V (colonne mat. originaria)
cumsum(variance.explained) # varianza cumulata spiegata. la prima da sola spiega l'86% con la seconda si arriva al 96%
plot(cumsum(swiss.svd$d^2/sum(swiss.svd$d^2)), type="l", xlab="Singular Vector", ylab = "Varianza Cumulata spiegata")

# ora ricostruiamo i dati con un solo Singular Vector (che ci da l'86% della spiegazione totale)
swiss.recon_86 <- swiss.svd$u[,1] %*% diag(swiss.svd$d[1], length(1), length(1)) %*% t(swiss.svd$v[,1])
# comparazione grafica
par(mfrow=c(1,2))
image(as.matrix((swiss), main="swiss data Image"))
image(swiss.recon_86, main="Reconstructed Image")

# ora ricostruiamo i dati con 2 Singular Vector (che ci da il 96% della spiegazione totale)
swiss.recon_96 <- swiss.svd$u[,1:2] %*% diag(swiss.svd$d[1:2]) %*% t(swiss.svd$v[,1:2])
# comparazione grafica
par(mfrow=c(1,2))
image(as.matrix((swiss), main="swiss data Image"))
image(swiss.recon_96, main="Reconstructed Image")

# confrontro tra i dati originari e 1 e poi 2 Singular Vectors. 
# Si vede che con due aumenta la somiglianza con la matrice origianria 
par(mfrow=c(2,2))
image(as.matrix(swiss), main="swiss data Image")
image(swiss.recon_86, main="Reconstructed Image 1 SV 86%")
image(as.matrix(swiss), main="swiss data Image")
image(swiss.recon_96, main="Reconstructed Image 2 SVs 96%")


# Calcolo dell' RMSE
p <- swiss.svd$u %*% t(swiss.svd$v)
dim(p)

RMSE_UV <- sqrt(mean((data.matrix(swiss) - p)^2)) # siccome ci sono tutti gli elemti della amtrice, va bene mean
RMSE_UV

# Di seguito un breve esempio per testare il corretto calcolo del RMSE
#M <- c(5, 2, 4, 4, 3, 3, 1, 2, 4, 1, 2, NA,3, 1, 4, 2, 5, 4, 3, 5, 4, 4, 5, 4, NA)
M  <- c(5, 2, 4, 4, 3, 3, 1, 2, 4, 1, 2, 0, 3, 1, 4, 2, 5, 4, 3, 5, 4, 4, 5, 4, 0)
M <- t(matrix(M,5,5))
M
M1 <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0)
M1 <- t(matrix(M1,5,5))
M1
RMSE <- sqrt( mean((M - M1)^2) )  # corretta se cis ono tutti gli elementi nella matrice
RMSE2 <- sqrt( sum((M - M1)^2)/23 )  # ok giusto occorre che l'elemto NA abbia un equivalente 0 o NA nelle matrici U e V
RMSE
RMSE2

# per studiare la correlazione del primo e il secondo singular Vector ricostruiamo la matrice originaria solo utilizzando la prima riga U e la prima colonna V
# 53% è la spiegazione in caso di standardizzazione

swiss.recon_53 <- swiss.svd$u[,1] %*% diag(swiss.svd$d[1], length(1), length(1)) %*% t(swiss.svd$v[,1])
#swiss.recon_53
cor(swiss,swiss.recon_53)[,1] # Examination -.91
swiss.recon_73 <- swiss.svd$u[,2] %*% diag(swiss.svd$d[2], length(1), length(1)) %*% t(swiss.svd$v[,2])
#swiss.recon_73
cor(swiss,swiss.recon_73)[,2] # Infant.Mortality -.88
swiss.recon_87 <- swiss.svd$u[,3] %*% diag(swiss.svd$d[3], length(1), length(1)) %*% t(swiss.svd$v[,3])
#swiss.recon_87
cor(swiss,swiss.recon_87)[,3] # Catholic -.74

# Ora verificare se queste 3 singular vectors corrispondono alla stesse se invece si utilizza PCA

par(mfrow=c(1,1))
cp <- prcomp(swiss)
summary(cp)
x <- summary(cp)$importance[3,] # trasforma il summary in un data frame così posso prendere la cumulata
plot(x, type="l")

data.frame(cp$x[,1:2], Species = row.names(swiss)) %>% 
  ggplot(aes(PC1,PC2, fill = Species))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

biplot(cp, scale=0)
new_cp <- cbind(swiss, cp$x[,1:3])
new_cp <- as.data.frame(new_cp)
library(corrplot)
r<-cor(swiss,new_cp[,7:9])

corrplot(r, method = "pie", type ="full")
corrplot(r, method = "number", type ="full")

# Siiiiiiiiiii vengono le stesse 3 componenti principali!!!
