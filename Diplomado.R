library(gapminder)
library(ggplot2)
library(psych)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(gganimate)
library(readr)

datos <- read_excel("/cloud/project/jaula.xlsx") 
as.data.frame(datos)


#histograma de las variables#
hist(x = datos$pprom)
hist(x = datos$znum)
hist(x = datos$fcr)
#histograma con mas datos de ejes y mas cadenas de comandos#
#histograma para pprom#
hist(x = datos$pprom, main = "Histograma de Pesos promedio", 
     xlab = "Gramos", ylab = "Frecuencia",
     col = "brown")

#histograma para znum#
hist(x = datos$znum, main = "Histograma de numero de mortalidad", 
     xlab = "N mortalidad", ylab = "Frecuencia",
     col = "grey")




##homocedasticidad de los datos###

par(mfrow=c(2, 3)) 
plot(density(datos$pprom))
plot(density(datos$znum))
plot(density(datos$fcr))

plot(ecdf(datos$pprom))
plot(ecdf(datos$znum))
plot(ecdf(datos$fcr))

##inndependencia y variacion entre los datos

ggplot(datos, aes(x=grupo, y=pprom, fill = grupo)) +
  geom_boxplot()


ggplot(datos, aes(x=grupo, y=pprom, fill = grupo)) +
  labs(y = "Peso promedio (g")+
  geom_boxplot() + 
  geom_jitter()

#interaccion entre los datos##
interaction.plot(datos$grupo, datos$pprom, datos$fcr)


pairs.panels(datos[,1:6], method = "pearson", hist.col = "red",  density = TRUE, font=3)


#GRAFICAS EN MOVIMIENTO#

grafico <- datos %>%
  ggplot() + 
  geom_point(aes(x = pprom, y = fcr, col = grupo, size = znum), alpha = 0.8) + theme_classic() + 
  theme(legend.position = "bottom") + guides(size = "none") + 
  labs(x = "Peso Promedio [gr]" ,y = "FCR",  col = "") 

grafico +
  transition_time(sem)
#grafico para evidenciar crecimiento entre grupos#

###grafico para evolucion ####


velocidades <- read_excel("/cloud/project/velocidades.xlsx")
as.data.frame(velocidades)
#evolucion sealand##

velocidades %>%
  filter(grupo == "SEALAND") %>%
  ggplot(aes(sem, grporsem)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 54, by = 2))+
  theme_minimal() +
  transition_reveal(sem)+
  ggtitle("Crecimiento semanal",
          subtitle = "SEALAND, 1 - 54")
#evoluci?n sisa#

velocidades %>%
  filter(grupo == "SISA") %>%
  ggplot(aes(sem, grporsem)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 54, by = 2))+
  theme_minimal() +
  transition_reveal(sem)+
  ggtitle("Crecimiento semanal",
          subtitle = "SISA, 1 - 54")


#### VELOCIDAD CRECIMIENTO KUDI?AM

velocidades %>%
  filter(grupo == "SISA") %>%
  ggplot(aes(sem, grporsem)) + geom_point() + geom_line() + 
  geom_text(aes(x = min(sem), y = min(grporsem), label = as.factor(sem)) , hjust=-2, vjust = -0.2, alpha = 0.5,  col = "gray", size = 20) +
  theme_minimal() +
  transition_reveal(sem) + 
  ggtitle("Velocidad crecimiento",
          subtitle = "SISA")+
  
  view_follow()

#VELOCIDAD CRECIMIENTO SEALAND
velocidades %>%
  filter(grupo == "SEALAND") %>%
  ggplot(aes(sem, grporsem)) + geom_point() + geom_line() + 
  geom_text(aes(x = min(sem), y = min(grporsem), label = as.factor(sem)) , hjust=-2, vjust = -0.2, alpha = 0.5,  col = "gray", size = 20) +
  theme_minimal() +
  transition_reveal(sem) + 
  ggtitle("Velocidad crecimiento",
          subtitle = "SEALAND")+
  
  view_follow()





########PROBANDO OTRO TIPO DE GRAFICO######

library(tidyverse)
library(ineq)
library(gganimate)
library(gameofthrones)


all <- read_excel("/cloud/project/jaulas.xlsx")
as.data.frame(all)


all%>%
  ggplot(aes(x = semana, y = pprom, color = jaula))+
  geom_line( )+
  geom_point()+
  xlab("Semana")+
  ylab("Peso [gr]")+
  labs(color = "Jaula")+
  ggtitle("Evoluci?n peso por jaula")+
  theme_minimal()

#Creando el objeto de ggplot

g1 = all%>%
  ggplot(aes(x = semana, y = pprom, color = jaula))+
  geom_line(size = 1)+
  geom_point(aes(group = seq_along(semana)), size = 2)+
  
  #Los segmentos que van a unir nuestras l?neas con las anotaciones:
  geom_segment(aes(xend = 54, yend = pprom, group = jaula),
               linetype = 2, colour = 'grey') +
  
  #Las anotaciones:
  geom_text(aes(x = 54, label = jaula), hjust = 0)+
  guides(color = FALSE)+
  scale_color_got_d(option = "Daenerys")+
  scale_x_continuous(breaks = seq(1, 54, by = 2),
                     limits = c(1, 54))+
  xlab("")+
  ylab("")+
  ggtitle("Crecimiento Forsyth 2021",
          subtitle = "Crecimiento semanal por jaula, 1 - 54")+
  labs(caption="Alain Mu?oz ")+
  theme_minimal()+
  
  
  #Agrandando el texto para adaptarlo al gif:
  
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 10, color = "grey40"))+

#animacion###:

transition_reveal(semana)+
  coord_cartesian(clip = 'off') +
  enter_fade() +
  exit_shrink()

#movimiento el objeto:

animate(g1)

#####escenario 30% mas de alimento###


all1 <- read_excel("/cloud/project/jaula.xlsx")
as.data.frame(all1)


all1%>%
  ggplot(aes(x = semana, y = pprom, color = jaula))+
  geom_line( )+
  geom_point()+
  xlab("Semana")+
  ylab("Peso [gr]")+
  labs(color = "Jaula")+
  ggtitle("Evoluci?n peso por jaula")+
  theme_minimal()

#creando el objeto

g1 = all1%>%
  ggplot(aes(x = semana, y = pprom, color = jaula))+
  geom_line(size = 1)+
  geom_point(aes(group = seq_along(semana)), size = 2)+
  
  #Los lineas que van a unir las anotaciones:
  geom_segment(aes(xend = 54, yend = pprom, group = jaula),
               linetype = 2, colour = 'grey') +
  
  #Las anotaciones de mi grafico:
  geom_text(aes(x = 54, label = jaula), hjust = 0)+
  guides(color = FALSE)+
  scale_color_got_d(option = "Daenerys")+
  scale_x_continuous(breaks = seq(1, 54, by = 2),
                     limits = c(1, 54))+
  xlab("")+
  ylab("")+
  ggtitle("Crecimiento Forsyth 2021 ESC_1",
          subtitle = "Crecimiento semanal por jaula, 1 - 54 ESC_1")+
  labs(caption="Alain Mu?oz ")+
  theme_minimal()+
  
  
  #Agrandando el texto para adaptarlo al gif:
  
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 10, color = "grey40"))+
  
  #Animaci?n:
  
  transition_reveal(semana)+
  coord_cartesian(clip = 'off') +
  enter_fade() +
  exit_shrink()

#Animando el objeto de ggplot:

animate(g1)


#hipotesis

ggplot(datos, aes(x=grupo, y=pprom, fill = grupo)) +
  labs(y = "Peso promedio (g")+
  geom_boxplot() + 
  geom_jitter()
#los boxplot no se traslapan, nos da un indicio de que las medias poblacionales son distintas

grupos <- read_excel("/cloud/project/grupos.xlsx")
as.data.frame(grupos)


t.test(x=grupos$ppromss, y=grupos$ppromsl, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=FALSE, conf.level=0.95)
