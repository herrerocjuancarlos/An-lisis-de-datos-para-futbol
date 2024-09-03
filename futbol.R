install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)
devtools::install_github("FCrSTATS/SBpitch")

# Para datos abiertos
library(SBpitch)
library(tidyr)
library(dplyr)
library(tidyverse)
library(StatsBombR)
library(ggplot2)
library(ggsoccer)
library(viridis)


Comp <- viridisLiteComp <- FreeCompetitions() 
Matches <- FreeMatches(Comp) 
#Esto seria para obtener todos los datos (tardaria mucho)
# StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
#Trabajar con una muestra mas pequeña
sample_matches <- Matches[sample(nrow(Matches), 10), ]
StatsBombData <- free_allevents(MatchesDF = sample_matches, Parallel = FALSE)

#extraer los tiros y goles de cada equipo
shots_goals_team = StatsBombData %>%
  group_by(team.name) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))
#Basicamente de la muestra que tengo lo que hago es ordenar por equipo para que me agrupe los disparos  y los goles

#extraer los tiros y goles de cada partido

shots_goals_promedio = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id), 
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))
# Grafico de tiros
ggplot(data = shots_goals_promedio, aes(x = reorder(team.name, shots), y = shots)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "Shots", title = "Ejemplo Sample") +
  theme(axis.title.y = element_text()) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip()
# Tiros cada 90 minutos por jugador
#Tenemos los minutos por cada jugador en nuestra muestra
player_shots = StatsBombData %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE))

#Definir get minutes por si falla
get.minutesplayed <- function(data) {
  data %>%
    filter(type.name == "Substitution" | type.name == "Starting XI") %>%
    mutate(MinutesPlayed = ifelse(type.name == "Starting XI", 90, 90 - minute)) %>%
    select(player.id, MinutesPlayed)
}
#Ahora
player_minutes = get.minutesplayed(StatsBombData)

player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

player_shots = left_join(player_shots, player_minutes)

player_shots = player_shots %>%
  mutate(nineties = minutes/90) 

player_shots = player_shots %>%
  mutate(shots_per90 = shots/nineties)

#Pases graficamente, primero creamos columnas y depuramos lo necesario
LuisAlbertoSuárez <- StatsBombData %>% filter(player.name == "Luis Alberto Suárez Díaz")
unique(LuisAlbertoSuárez$player.id) #verificar el id de jugador
# Eliminar la 'c' y los paréntesis de pass.end_location
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(pass.end_location = gsub("c\\(|\\)", "", pass.end_location))
# Separar las coordenadas en dos columnas
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  separate(pass.end_location, into = c("pass.end_location.x", "pass.end_location.y"), sep = ",", convert = TRUE)
# Separar por location
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(location = gsub("c\\(|\\)", "", location))
# Separar las coordenadas en dos columnas
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  separate(location, into = c("location.x", "location.y"), sep = ",", convert = TRUE)
# Me puede dar error de coordenadas numericas, las convierto
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(across(c(location.x, location.y, pass.end_location.x, pass.end_location.y), as.numeric))


#Graficar los Pases de Luis alberto

passes = LuisAlbertoSuárez %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) & player.id==5246) %>%
filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18) 

create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y), lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))  +
labs(title = "Luis Alberto, Completed Box Passes", subtitle = "La Liga, 2005/2006")+
scale_y_reverse() +
  coord_fixed(ratio = 105/100)

### Filtrar datos de Luis Suárez
LuisAlbertoSuárez <- StatsBombData %>% filter(player.name == "Luis Alberto Suárez Díaz")

# Verificar el id de jugador
unique(LuisAlbertoSuárez$player.id)

# Eliminar la 'c' y los paréntesis de shot.end_location
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(shot.end_location = gsub("c\\(|\\)", "", shot.end_location))

# Separar las coordenadas en dos columnas
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  separate(shot.end_location, into = c("shot.end_location.x", "shot.end_location.y"), sep = ",", convert = TRUE)

# Eliminar la 'c' y los paréntesis de location
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(location = gsub("c\\(|\\)", "", location))

# Separar las coordenadas en dos columnas
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  separate(location, into = c("location.x", "location.y"), sep = ",", convert = TRUE)

# Convertir las coordenadas a numéricas
LuisAlbertoSuárez <- LuisAlbertoSuárez %>%
  mutate(across(c(location.x, location.y, shot.end_location.x, shot.end_location.y), as.numeric))

# Filtrar los tiros de Luis Suárez
shots = LuisAlbertoSuárez %>%
  filter(type.name == "Shot" & player.id == 5246)

# Crear el gráfico de tiros
create_Pitch() +
  geom_segment(data = shots, aes(x = location.x, y = location.y, xend = shot.end_location.x, yend = shot.end_location.y), 
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +
  labs(title = "Luis Suárez, Completed Shots", subtitle = "La Liga, 2005/2006") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)


# Crear el mapa de calor de movimientos

ggplot(LuisAlbertoSuárez, aes(x = location.x, y = location.y)) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(alpha = 0.7) +
  scale_fill_viridis_c() +
  labs(title = "Mapa de Calor de Movimientos de Luis Suárez", subtitle = "La Liga, 2005/2006") +
  theme_pitch() +
  coord_flip()




