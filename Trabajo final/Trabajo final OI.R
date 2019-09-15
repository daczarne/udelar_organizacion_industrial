#$#############################
#### TRABAJO FINAL OI 2017 ####
#$#############################

rm(list=ls())
setwd("C:/Users/dacza/Dropbox/UdelaR/CCEEA/Semestre 9/Organización Industrial/Trabajo final")
setwd("C:/Users/Daniel/Dropbox/UdelaR/CCEEA/Semestre 9/Organización Industrial/Trabajo final")

library(dplyr)
library(ggplot2)

# Carga la base de estaciones
eess <- read.csv("eess.csv", header=T)

# Estaciones por distribuidor
n=dim(eess)[1]
group_by(eess, distri) %>% summarise(obs=n(), pobs=round(n()/n*100,2))

# Estaciones por localización geográfica
length(unique(eess$depto))
eessdepto <- group_by(eess, depto) %>% summarise(eess=n(), peess=n()/n*100)

# Empadronamiento por depto y categoría de vehículo
emp <- read.table("Empadronamiento por depto y cat.txt", header=T, sep=",")
autos=sum(emp$cant)
autosdepto <- group_by(emp, depto) %>% summarise(tutus=sum(cant), ptutus=round(sum(cant)/autos*100,2))

# Estaciones por auto por depto
datos <- left_join(eessdepto, autosdepto, copy=TRUE)
datos <- as.data.frame(datos)
datos$tutuseess <- round(datos$tutus/datos$eess,2)
datos

# Estaciones por superficie
sup <- read.table("sup.txt", header=T, sep=",")
datos <- left_join(datos, sup, copy=TRUE)
datos <- mutate(datos, pdens=superficie/sum(superficie))
datos <- mutate(datos, denseess=round(eess/superficie,4))
datos <- mutate(datos, denstutus=round(tutus/superficie, 4))
datos <- mutate(datos, tutuspond=tutuseess/pdens)

datos %>% select(depto, tutuspond)

# Compras de petróleo
petro <- read.table("petro.txt", header=T, sep=",")
petro$Period.Desc. <- as.factor(petro$Period.Desc.)

# Mapa de las EESS (Uruguay)
uru <- read.table("uruguay2.txt", header=F, sep=",")
names(uru) <- c("y","x")

# Mapa de las EESS (MAM)
mammap <- read.table("mam.txt", header=F, sep=",")
names(mammap) <- c("y","x")

# EESS en MAM
mam <- dplyr::filter(eess, MAM==1)

# Plotea el polígono y las EESS
pdf(file="mapaeess.pdf", height=6, width=6)
ggplot(uru, aes(x,y)) + 
      geom_polygon(aes(x,y), data=uru) +
      geom_point(aes(color=distri, shape=distri), data=eess, na.rm=TRUE, size=3, alpha=.8) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      #scale_color_discrete("Distribuidoras")
      labs(title="Estaciones de Servicio en Uruguay") +
      theme(plot.title=element_text(size=25, hjust=0.5), legend.title=element_blank(),
            legend.position=c(.9,.9))
dev.off()

# Plotea el polígono y las EESS en Montevideo y Canelones
pdf(file="mapaeessmam.pdf", height=6, width=6)
#X11(15,15)
ggplot(mammap, aes(x,y)) + 
      geom_polygon(aes(x,y), data=mammap) +
      geom_point(aes(color=distri, shape=distri), data=mam, na.rm=TRUE, 
                 size=3, alpha=.8) +
      # coord_cartesian(ylim=c(min(mam$y, na.rm=TRUE), max(mam$y, na.rm=TRUE)),
      #                 xlim=c(min(mam$x, na.rm=TRUE), max(mam$x, na.rm=TRUE))) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      #scale_color_discrete("Distribuidoras")
      labs(title="Estaciones de Servicio en MAM") +
      theme(plot.title=element_text(size=25, hjust=0.5), 
            legend.title=element_blank(),
            legend.position=c(.9,.1))
dev.off()

# Plotea las estaciones
# ggplot(eess, aes(x,y)) +
#       geom_point(aes(color=distri), size=1, alpha=.8) +
#       theme(axis.title.x=element_blank(),
#             axis.text.x=element_blank(),
#             axis.ticks.x=element_blank(),
#             axis.title.y=element_blank(),
#             axis.text.y=element_blank(),
#             axis.ticks.y=element_blank()) +
#       labs(title="Estaciones de Servicio en Uruguay") +
#       theme(plot.title=element_text(hjust=0.5))

# pdf(file="mapaeess.pdf", height=6, width=6)
# X11(15,15)
# par(oma=c(0,0,1,2))
# plot(x=eess$x, y=eess$y, pch=as.numeric(eess$distri), col=as.numeric(eess$distri), axes=FALSE,xlab=NA,ylab=NA)
# mtext(text="Estaciones de Servicio en Uruguay", cex=2, side=3, line=1)
# legend('topright', legend=levels(eess$distri), col=1:3, cex=0.8, pch=1:3)
# for(i in 1:dim(uru)[1]){
#       segments(y0=uru[i,1], y1=uru[i+1,1], x0=uru[i,2], x1=uru[i+1,2])
# }
# dev.off()

#$##############################
#### FIN DE LA PROGRAMACIÓN ####
#$##############################