# EDA Scatter Plot Carrera Luz
library(paqueteMET)
plot(CarreraLuz22$timerun ~ CarreraLuz22$id, 
     xlab='Numero de corredores',
     ylab='Tiempo de Carrera',
     main='Relación de tiempo de carrera')

#
plot(CarreraLuz22$timerun ~ CarreraLuz22$edad, 
     xlab='Categoria',
     ylab='Tiempo de Carrera',
     main='Relación de tiempo de carrera con la categoria')

# EDA histogram
library(ggplot2)
ggplot(CarreraLuz22,aes(x=CarreraLuz22$edad))+
  geom_histogram(binwidth = 10)+
  labs(x='Edades',y='Numero de personas')+
  ggtitle('Histograma de Edades')

# EDA Boxplot
ggplot(CarreraLuz22,aes(x=CarreraLuz22$categoria,y=CarreraLuz22$timerun,fill=CarreraLuz22$categoria))+
  geom_boxplot()+
  theme(legend.position = 'bottom')+
  labs(x='Categoria',y='Tiempo de Carrera',
       caption = 'Tomada de XM',fill='Categoria_')

#
ggplot(CarreraLuz22,aes(x=CarreraLuz22$sex,y=CarreraLuz22$timerun,fill=CarreraLuz22$sex))+
  geom_boxplot()+
  theme(legend.position = 'bottom')+
  labs(x='Categoria',y='Tiempo de Carrera',
       caption = 'Tomada de XM',fill='Categoria_')

# grafica de dispersion
graph <- ggplot(CarreraLuz22,aes(x=CarreraLuz22$edad,y=CarreraLuz22$timerun,color=CarreraLuz22$categoria))+
  geom_point()
graph

#
library(plotly)
ggplotly(graph)

#
library(dplyr)
pairs(CarreraLuz22[,c(3,5)])
cor(data_men[,c(3,5)])
data_men <- filter(CarreraLuz22,CarreraLuz22$sex=='Hombre')
data_women <- filter(CarreraLuz22,CarreraLuz22$sex=='Mujer')
pairs(data_men[,c(3,5)])
summary(CarreraLuz22)
data_time_min <- CarreraLuz22 %>%
  mutate(timerun=CarreraLuz22$timerun/60)

CarreraLuz22 %>%
  mutate(timerun=CarreraLuz22$timerun/60)

# Facet_wrap
ggplot(CarreraLuz22,aes(x=CarreraLuz22$edad,y=CarreraLuz22$timerun,color=CarreraLuz22$sex))+
  geom_point()+
  facet_wrap(~CarreraLuz22$nacionalidad)

CarreraLuz22[2,3]

#
library(stringi)
new_data <- CarreraLuz22 %>%
  mutate(origen=toupper(stri_trans_general(CarreraLuz22$origen,'Latin-ASCII')))

new_data <- new_data %>%
  mutate(nacionalidad = ifelse(CarreraLuz22$nacionalidad == '0', ifelse(CarreraLuz22$origen=='CALI','COL','PAN'),CarreraLuz22$nacionalidad))

write.csv(new_data,'./prueba_01.csv')

#

library(summarytools)
freq(CarreraLuz22$nacionalidad)


