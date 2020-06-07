#############################################################
### Chopin, Mozart and Voiles emotions survey  data bases ###
#############################################################

library(ggplot2)
library(tidyverse)

# Crear vectores de sinonimos
calma <- c('tranquilidad|Tranquilidad|Armonia|Paz|Quietud|Sosiego|Placidez|Relajacion|Serenidad|Descanso|Equilibrio|Inocencia|Libertad|Ligereza|Orden|Organizacion|Refelxion|Reflexion|InCalma|Calma? ')
tension <- c('Intranquilidad|Impaciencia|Ansiedad|Inquietud|Desasosiego|Alerta|Angustia|Descontrol|Tension|Estres|Nerviosismo|Sobresalto|Sosobra')
alegria <- c('alegria|Regocijo|Jubilo|Entusiasmo|Exaltacion|Felicidad|Festividad|Jugueton|Optimismo')
tristeza <- c('Pesar|Nostalgia|Nolstalgia|Tostalgia|Melancolia|Soledad|Depresion|Anoranza|Desolacion')
compasion <- c('Agradecimiento|Apertura|Comprension|Introspeccion')
certeza <- c('Claridad|Seguridad|Esperanza|Resolucion|Aceptacion|Encuentro|Expectativa|Persistencia')
duda <- c('Incertidumbre|Duda|Desesperanza|Misterio|Suspenso')
placer <- c('Suavidad|Gusto|Contemplacion|Finesa|Pureza')
dolor <- c('Amargura|Congoja|Drama|Separacion')
amor <- c('Ternura|Compania|Romanticismo')
diversion <- c('Sorpresa|sorpresa|Distraccion|Juego|Asombro|Experimentacion|Flexibilidad')
aburrimiento <- c('Desesperacion|Monotonia')
frustracion <- c('Decepcion|Fracaso')
valor <- c('Arrojo|Emprendimiento|Valentia')
miedo <- c('Sospecha|Temor')
agrado <- c('Bienestar|Amabilidad|Exotismo|Gracia|Sencillez')
desagrado <- c('Desencanto|Incomodidad')
deseo <- c('Interes|Anhelo|Busqueda|Ensonacion')
entusiasmo <- c('Curiosidad|Dedicacion|Descubrimiento|Empuje|Intriga|Motivacion|Pasion')
apatia <- c('Pereza|Recogimiento')
vigor <- c('Fortaleza|Energia|Fuerza|Concentracion|Resiliencia')
altivez <- c('Altaneria|Decencia|Pedanteria|Picardia|Poder')
humillacion <- c('Resignacion')

# Vector de sinonimos
sinonimos <- c(calma, tension, alegria, tristeza, compasion, certeza, duda, placer, 
               dolor, amor, diversion, aburrimiento, frustracion, valor, miedo, 
               agrado, desagrado, deseo, entusiasmo, apatia, vigor, altivez, humillacion)

# Vector de reemplazo
replaces <- c('Calma', 'Tension', 'Alegria', 'Tristeza', 'Compasion', 'Certeza', 'Duda', 'Placer', 
              'Dolor', 'Amor', 'Diversion', 'Aburrimiento', 'Frustracion', 'Valor', 'Miedo', 
              'Agrado', 'Desagrado', 'Deseo', 'Entusiasmo', 'Apatia', 'Vigor', 'Altivez','Humillacion')

----------- ### Chopin ### -----------

chopin_df <- read.csv('C:/Users/Erick/Documents/projects/emociones/db/chopin.csv', 
                      header = TRUE)

# Enlistar las emociones 
list_emotions_chopin <- strsplit(as.character(chopin_df$resumen), ' ')

# Anadir columna que cuente el numero de emociones por participante
chopin_df <- chopin_df %>%
  mutate(n_emociones = unlist(lapply(lapply(list_emotions_chopin, unique),length)))

# Obtener vector que contenga todo el listado de emociones (respuestas)
emotions_chopin <- unlist(strsplit(as.character(chopin_df$resumen), ' '))

n <- c(seq(1, length(sinonimos), 1)) # vector guia

# Reemplazar sinonimos
for(i in n){
 emotions_chopin <- str_replace_all(emotions_chopin, sinonimos[i], replaces[i])
}

emotions_chopin <- replace(emotions_chopin, 
                           emotions_chopin == 'InCalma', # Cambiar valor erroneo
                           'Calma') 

# Crear vector de emociones unicas 
unique_emotions_chopin <- sort(unique(emotions_chopin))

# Crear data frame de frecuencias de emociones
chopin_emotions <- as.data.frame(table(emotions_chopin))

# Grafica de frecuencia
chopin_emotions %>%
  ggplot(aes(x= Freq, y = reorder(emotions_chopin, -Freq), fill = Freq)) +
  geom_bar(stat = 'identity')  +
  scale_fill_gradient(name = 'Frecuencia', low = 'lightblue', high = 'royalblue4') +
  scale_x_continuous(breaks = c(seq(0, 100, 10))) +
  labs( x = 'Frecuencia', y = 'Emociones', 
        title = 'Emociones Resumidas Chopin')

----------- ### Mozart ### -----------

mozart_df <- read.csv('C:/Users/Erick/Documents/projects/emociones/db/mozart.csv', 
                      header = TRUE)

# Enlistar las emociones 
list_emotions_mozart <- strsplit(as.character(mozart_df$resumen), ' ')

# Anadir columna que cuente el numero de emociones por participante
mozart_df <- mozart_df %>%
  mutate(n_emociones = unlist(lapply(lapply(list_emotions_mozart, unique),length)))

# Obtener que contenga todo el listado de emociones (respuestas)
emotions_mozart <- (unlist(strsplit(as.character(mozart_df$resumen), ' ')))

# Eliminar 'Sin respuesta'
emotions_mozart <- emotions_mozart[! emotions_mozart %in% c('Sin', 'respuesta')]

# Reemplazar sinonimos 
for(i in n){
  emotions_mozart <- str_replace_all(emotions_mozart, sinonimos[i], replaces[i])
}

emotions_mozart <- replace(emotions_mozart, 
                           emotions_mozart == 'InCalma', # Cambiar valor erroneo
                           'Calma')

# Crear vector de emociones unicas 
unique_emotions_mozart <- sort(unique(emotions_mozart))

# Crear data frame de frecuencias de emociones
mozart_emotions <- as.data.frame(table(emotions_mozart))

# Grafica de frecuencia
mozart_emotions %>%
  ggplot(aes(x= Freq, y = reorder(emotions_mozart, -Freq), fill = Freq)) +
  geom_bar(stat = 'identity')  +
  scale_fill_gradient(name = 'Frecuencia', low = 'burlywood1', high = 'firebrick1') +
    scale_x_continuous(breaks = c(seq(0, 60, 5))) +
  labs( x = 'Frecuencia', y = 'Emociones', 
        title = 'Emociones Resumidas Mozart')

----------- ### Voiles ### -----------

voiles_df <- read.csv('C:/Users/Erick/Documents/projects/emociones/db/voiles.csv', 
                      header = TRUE)

# Enlistar las emociones 
list_emotions_voiles <- strsplit(as.character(voiles_df$resumen), ' ')

# Anadir columna que cuente el numero de emociones por participante
voiles_df <- voiles_df %>%
  mutate(n_emociones = unlist(lapply(lapply(list_emotions_voiles, unique),length)))

# Obtener vector de emociones que contenga todo el listado de emociones (respuestas)
emotions_voiles <- unlist(strsplit(as.character(voiles_df$resumen), ' ')) # Emociones Raw

# Reemplazar sinonimos 
for(i in n){
  emotions_voiles <- str_replace_all(emotions_voiles, sinonimos[i], replaces[i])
}


emotions_voiles <- str_replace(emotions_voiles, 
                               'InCalma', # Cambiar valor erroneo
                               'Calma')

# Crear vector de emociones unicas 
unique_emotions_voiles <- sort(unique(emotions_voiles))

# Crear data frame de frecuencias de emociones
voiles_emotions <- as.data.frame(table(emotions_voiles))

# histograma de frecuencia
voiles_emotions %>%
  ggplot(aes(x= Freq, y = reorder(emotions_voiles, -Freq), fill = Freq)) +
  geom_bar(stat = 'identity')  +
  scale_fill_gradient(name = 'Frecuencia', low = 'pink', high = 'purple') +
  scale_x_continuous(breaks = c(seq(0, 40, 5))) +
  labs( x = 'Frecuencia', y = 'Emociones', 
        title = 'Emociones Resumidas Voiles')
  
### Emociones de los tres compositores ###

# Unir emociones chopin - mozart
all_emotions <- merge(chopin_emotions, mozart_emotions, 
                      by.x = 'emotions_chopin',
                      by.y = 'emotions_mozart',
                      all.x = TRUE,
                      all.y = TRUE,
                      suffixes = c('chopin', 'mozart'),
                      no.dups = TRUE)

# Unir emociones chopin, mozart - voiles
all_emotions <- merge(all_emotions, voiles_emotions, 
                      by.x = 'emotions_chopin',
                      by.y = 'emotions_voiles',
                      all.x = TRUE,
                      all.y = TRUE,
                      suffixes = c('all' , 'voiles'),
                      no.dups = TRUE)

# Cambiar NAs a 0
all_emotions[is.na(all_emotions)] <- 0

# Cambiar nombres de columnas para el nuevo df 
colnames(all_emotions)[c(1, 2, 3, 4)] <- c('emotions', 
                                           'chopin', 
                                           'mozart', 
                                           'voiles')

# Lista de emociones basada en el apendice (Flores y Diaz, 2001)
emotions_list <- as.data.frame(sort((all_emotions$emotions))) # No se presento ira, aversion y agotamiento 


