##Con el siguiente ejemplo se busca explorar datos de un usuario de twitter
## 1. limpiar espacio de trabajo
rm(list = ls())
#fin limpiar espacio de trabajo
#2. importar librerias de trabajo
library(ggplot2)
library(twitteR)
library(plotly)
#fin importar librerias
#3. definir las llaves de acceso
access_token <- ""               #las llaves de acceso deben ser ingresadas aqui
access_token_secret <-""          #las llaves de acceso deben ser ingresadas aqui
consumer_key <- ""               #las llaves de acceso deben ser ingresadas aqui
consumer_secret <-""              #las llaves de acceso deben ser ingresadas aqui
#fin definir llaves de acceso
#4. acceder a la API
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_token_secret)
#fin acceder a la API
#5. solicitar información de usurio
username <- "UIS" #debe ingresar su usuario aqui
datos <- userTimeline(username, n = 100)
df <- twListToDF(datos)
#visualizar datos en forma df
View(df)
#fin solicitar información de usuario
#6. análisis descriptivo de los datos
#análisis de retweets
gtweets <-
  ggplot(data = df, mapping = aes(x = created)) +
  geom_line(mapping = aes(y = favoriteCount, colour = "Favorito")) + geom_line(mapping = aes(y = retweetCount, colour =
                                                                                               "Retweet")) + ggtitle("Análisis de Retweet y Favoritos") + xlab("Fecha") +
  ylab("Cuenta") + scale_colour_manual("", values = c("Favorito" = "blue", "Retweet" ="green"))

greply <-
  ggplot(data = df) + geom_bar(
    mapping = aes(x = replyToSN),
    fill = I("blue"),
    color = I("black")
  ) + ggtitle("Usuarios dieron Respuesta") + xlab("Usuario") + ylab("Cuenta")

#presentar graficas
ggplotly(gtweets)
ggplotly(greply)
