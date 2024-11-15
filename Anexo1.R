setwd("C:/Users/jvive/Desktop/Ecology/Taller")
Family_channels <- read.csv("C:/Users/jvive/Desktop/Ecology/Taller/Familias.csv")

library("vegan")

#Índices de diversidad alpha

Channels=nrow(Family_channels)

Diversity_index=data.frame(
  Channel=numeric(),
  S=numeric(),
  H=numeric(),
  Simpson=numeric(),
  J=numeric()
)


for (i in 1:Channels){
  canales=data.frame(
  Channel=i,
  S=specnumber(Family_channels[i,]),
  H=diversity(Family_channels[i,],index="shannon"),
  Simpson=diversity(Family_channels[i,],index="simpson"),
  J=((diversity(Family_channels[i,],index="shannon"))/(log(specnumber(Family_channels[i,]))))
  )
  
  Diversity_index=rbind(Diversity_index,canales)
}
  
View(Diversity_index)

png("Riqueza.png", width=1200, height=900)
barplot(Diversity_index[,2],
        main="Riqueza de familias por channel",
        col="#ca9cf1",
        ylab="Número de familias",
        xlab="Channel",
        cex.main = 2,
        cex.lab = 1.5,
        cex.axis = 1.5,
        names.arg=rownames(Diversity_index)
        )
dev.off()

png("índice de Shannon.png", width=1200, height=900)
barplot(Diversity_index[,3],
        main="índice de Shannon-Wiener",
        col="#f19cd1",
        ylab="Índice",
        xlab="Channel",
        cex.main = 2,
        cex.lab = 1.5,
        cex.axis = 1.5,
        names.arg=rownames(Diversity_index)
)
dev.off()

png("índice de Simpson.png", width=1200, height=900)
barplot(Diversity_index[,4],
        ylim=c(0,0.8),
        main="índice de Gini-Simpson",
        col="#9f9cf1",
        ylab="Índice",
        xlab="Channel",
        cex.main = 2,
        cex.lab = 1.5,
        cex.axis = 1.5,
        names.arg=rownames(Diversity_index)
)
dev.off()

png("índice de Pielou.png", width=1200, height=900)
barplot(Diversity_index[,5],
        ylim=c(0,0.7),
        main="índice de equitatividad de Pielou",
        col="#9cf1cd",
        ylab="Índice",
        xlab="Channel",
        cex.main = 2,
        cex.lab = 1.5,
        cex.axis = 1.5,
        names.arg=rownames(Diversity_index)
)
dev.off()


#Descargar la tabla con los índices de diversidad
install.packages("openxlsx")
library(openxlsx)
write.xlsx(Diversity_index, "C:/Users/jvive/Desktop/Ecology/Taller/Diversity_index.xlsx")

#Agrupamiento jerárquico

Family_channelsW=wisconsin(Family_channels)
Family_channelsB=vegdist(Family_channelsW,method="bray")

png("agrupamientojerarquico.png", width=1200, height=900)
FCW=hclust(Family_channelsB,method="ward.D2")
plot(FCW,las=1)
dev.off()

#iNEXT

library("iNEXT")
library("ggplot2")

Channels_=apply(Family_channels,1,as.numeric)

H0=iNEXT(Channels_, q = 0, datatype = "abundance")

png("plotH00.png", width=800, height=600)
plotH0=ggiNEXT(H0,type=1)+ 
  theme(legend.position = "right", 
        plot.title = element_text(size = 14, face = "bold",hjust = 0.5)) + 
  ggtitle("Curvas de rarefacción y extrapolación, número de Hill 0") +  
  xlab("Tamaño de muestra") +                         
  ylab("Riqueza de familias estimada")              
print(plotH0)
dev.off()


H1=iNEXT(Channels_, q = 1, datatype = "abundance")

png("plotH1.png", width=800, height=600)
plotH1=ggiNEXT(H1,type=1)+ 
  theme(legend.position = "right", 
        plot.title = element_text(size = 14, face = "bold",hjust = 0.5)) + 
  ggtitle("Curvas de rarefacción y extrapolación, número de Hill 1") +  
  xlab("Tamaño de muestra") +                         
  ylab("Exp de diversidad de Shannon")              
print(plotH1)
dev.off()






