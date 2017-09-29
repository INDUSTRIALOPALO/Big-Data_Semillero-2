#importar librerias 
library(NLP)
library(bitops)
library(RCurl)
library(rjson)
library(twitteR)
library(tm)
library(Rcpp)
library(RColorBrewer)
library(wordcloud)




#fin importar librerias

access_token <- ""               #las llaves de acceso deben ser ingresadas aquÃ?
access_token_secret <-""          #las llaves de acceso deben ser ingresadas aquÃ?
consumer_key <- ""               #las llaves de acceso deben ser ingresadas aquÃ?
consumer_secret <-""




#usuario
setup_twitter_oauth(consumer_key,consumer_secret, access_token,access_token_secret)


#Tweets del usuario
usuario <- "UIS" #digite su usuario aquí
tweets <- userTimeline(usuario, n=50)
#guardar los datos de los tweet en un data frame 
df<-twListToDF(tweets)
#mostrar en pantalla
View(df)

#seleccionar el texto de los tweets 
texto <- df$text
#limpiar los datos de elementos no deseados
textoclean <-gsub("http[[:alnum:][:punct:]]*", "", texto) #remover enlaces
textoclean <- gsub("(RT|via)((?:/b/w*@/w+)+)", "", textoclean) #Retuits
textoclean <- gsub("@/w+", "", textoclean) #@otragente
textoclean <- gsub("[[:punct:]]", "", textoclean) #simbolos de puntuacion
textoclean <- gsub("[[:digit:]]", "", textoclean) #numeros


#se construye un corpus
corpus <- Corpus(VectorSource(textoclean))



#quitamos las palabras vacias del contenido en ingles
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), "UNGRD"))

#quitamos los espacios en blanco
corpus <- tm_map(corpus, stripWhitespace)

#se crea la matriz de terminos a partir de corpus
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)

#ordena el uso de palabras
wf <- sort(rowSums(m), decreasing = TRUE)

#crea un data frame
dm <- data.frame(word = names(wf), freq = wf)

#imprime la nube de palabras

wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(6,"Accent"))





