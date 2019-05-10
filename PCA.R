library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

cp <- prcomp(tissue_gene_expression$x)
summary(cp)

data.frame(cp$x[,1:2], Species=tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = Species))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

# cerebellum       colon endometrium hippocampus      kidney       liver    placenta

plot(cp, type="l")
biplot(cp, scale=0)

str(cp)
cp$x
# creo un nuovo data set con le sole 2 componenti principali PC1 e PC2
new_cp <- cbind(tissue_gene_expression$x, cp$x[,1:2])
new_cp <- as.data.frame(new_cp)
summary(new_cp)

#ggplot

#install.packages("ggplot2")
library(ggplot2) #richiama in memoria il pacchetto
ggplot(new_cp, aes(PC1, PC2, col=tissue_gene_expression$y, fill = tissue_gene_expression$y))+
  stat_ellipse(geom = "polygon", col="black", alpha = 0.5)+
  geom_point(shape = 21, col ="black")

#correlazione tra Variabili originarie e le PCs
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# install.packages("corrplot")
# library(corrplot)

r<-cor(tissue_gene_expression$x,new_cp[,501:502])

#corrplot(r, method = "pie", type ="upper")

max(cor(tissue_gene_expression$x,new_cp[,501]))
min(cor(tissue_gene_expression$x,new_cp[,501]))
r1 <- r == min(cor(tissue_gene_expression$x,new_cp[,501]))

symnum(r, cutpoints = c(-1, -0.94, -0.50, 0, 0.50, 0.94, 1),
       symbols = c("N", ".", ",", "+", "*", "P"),
       abbr.colnames = TRUE)

symnum(r1, abbr.colnames = FALSE)

max(cor(tissue_gene_expression$x,new_cp[,502]))
min(cor(tissue_gene_expression$x,new_cp[,502]))
r2 <- r == min(cor(tissue_gene_expression$x,new_cp[,502]))
symnum(r, cutpoints = c(-1, -0.82, -0.50, 0, 0.50, 0.82, 1),
       symbols = c("N", ".", ",", "+", "*", "P"),
       abbr.colnames = TRUE)

# PC2 = HAMP corr maggiore
# PC1 = GPM6B corr maggiore


avgs <- rowMeans(tissue_gene_expression$x)
cor(avgs, cp$x[,1])

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
