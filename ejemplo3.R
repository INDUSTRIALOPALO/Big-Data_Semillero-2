    #este scrip permite extraer datos de la red social twitter y ademas
    #realiza un analisis de agrupamiento para estos datos 
    #limpiar espacio de trabajo
    rm(list = ls())
    #instalar librerias necesarias 
    #librerias <- c("twitteR", "tm", "Rcpp", "RColorBrewer", "wordcloud", "ggplot2")
    #install.packages(librerias, dependencies = TRUE)
    #importar librerias necesarias 
    library(twitteR)
    library(tm)
    library(Rcpp)
    library(RColorBrewer)
    library(wordcloud)
    library(ggplot2)
    #extraer datos de twitter 
    #ingreso de llaves
    access_token <- ""                #las llaves de acceso deben ser ingresadas aquí
    access_token_secret <-""          #las llaves de acceso deben ser ingresadas aquí
    consumer_key <- ""                #las llaves de acceso deben ser ingresadas aquí
    consumer_secret <-""              #las llaves de acceso deben ser ingresadas aquí
    #loging de usuario
    setup_twitter_oauth(consumer_key,
                        consumer_secret,
                        access_token,
                        access_token_secret)
    #para un usuario 
    datos <- userTimeline("realDonaldTrump", n = 500)#ingrese el usuario a analizar en las comillas
    #para la consulta de una cadena de texto
    #datos <- searchTwitter("", n = 100)#ingrese la cadena de texto a consultar en las comillas 
    #convertir la lista a data frame
    df <- twListToDF(datos)
    #definir el texto a analizar
    texto <- df$text
    texto <- iconv(texto, to = "UTF-8") #esto permite que en los tweets se identifiquen las tildes
    #Paso 1:Preprocesamiento
    #limpiar
    texto <- gsub("http[[:alnum:][:punct:]]*", "", texto) #remover enlaces
    texto <- gsub("https//t.co/w+", "", texto) #links
    texto <- gsub("(RT|via)((?:/b/w*@/w+)+)", "", texto) #Retuits
    texto <- gsub("@/w+", "", texto) #@otragente
    texto <- gsub("[[:punct:]]", "", texto) #simbolos de puntuacion
    texto <- gsub("[[:digit:]]", "", texto) #numeros
    texto <- gsub("httpst", "", texto) #links
    texto <- gsub("#", "", texto)
    texto <- gsub("%", "", texto)
    
    #definir el corpus
    corpus <- Corpus(VectorSource(texto))
    
    #quitamos las palabras vacias del contenido en español
    #corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    #quitamos los espacios en blanco
    corpus <- tm_map(corpus, stripWhitespace)
    
    #se pasa a minuscula
    corpus <- tm_map(corpus , content_transformer(tolower))
    
    #Paso 2: transformar el corpus a un espacio vectorial
    #TfIdf
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) #calcular la matriz tfidf
    TfIdf <- as.matrix(dtm) #definirla como matriz
    TfIdf <- t(TfIdf) #encontrar la matriz transpuesta 
    #analisis descriptivo
    #extraer la matrix Tf
    dtm <- DocumentTermMatrix(corpus) #calcular la matriz tf
    palabras <- as.vector(colnames(dtm))
    Tf<- as.matrix(dtm) 
    Tf<- apply(Tf, MARGIN = 2, sum)
    #Tf<- as.matrix(Tf)
    Tf <- as.data.frame(cbind(palabras, freq = as.numeric(paste(Tf))))
    Tf <- Tf[order(Tf[,2], decreasing = TRUE),]
    
    #restringir valores de Tf
    Tfmost <- as.data.frame(Tf[1:10,])
    Tfmost$freq <- as.numeric(paste(Tfmost$freq))
    #construir histograma
    #grafico1 <-qplot(Tfmost$palabras, Tfmost$freq, geom = "bar", stat = "identity")
    grafico1 <- ggplot(data = Tfmost, mapping = aes(x= palabras, y= freq))+geom_bar(stat = "identity", fill = "blue")+ggtitle("histograma de palabras frecuentes")
    print(summary(as.numeric(paste(Tf$freq))))
    grafico1
    #Paso 3: modelado para el descubrimiento de conocimiento 
    
    modelo <- kmeans(t(TfIdf),4)#puede variar el valor de la constante a conveniencia
    resumen <- summary(as.factor(modelo$cluster))
    resumen <- as.data.frame(resumen)
    resumen <- as.data.frame(cbind( grupo=row.names(resumen), resumen = resumen))
    grafico2 <- ggplot(data = resumen, mapping = aes(x =grupo, y=resumen))+geom_bar(stat = "identity", fill = "blue")+ggtitle("distribucion de grupos")
    grafico2
    # realizar histogramas de palabras para cada grupo
    for (i in seq(1,nrow(resumen), by = 1)){
      dtm <- DocumentTermMatrix(corpus) #calcular la matriz tf
      palabras <- as.vector(colnames(dtm))
      Tf<- as.matrix(dtm)
      palabras <- palabras
      Tf<- as.data.frame(Tf[modelo$cluster==i,])
      Tf<- as.data.frame( apply(Tf, MARGIN = 2, sum))
      #Tf<- as.matrix(Tf)
      #Tf <- as.data.frame(row.names(Tf), Tf$`apply(Tf, MARGIN = 2, sum)`)
      Tf <- cbind(palabras[order(Tf$`apply(Tf, MARGIN = 2, sum)`, decreasing = TRUE)] , Tf[order(Tf$`apply(Tf, MARGIN = 2, sum)`, decreasing = TRUE),])
      Tf <- as.data.frame(Tf)
      #restringir valores de Tf
      valor <-10
      if(nrow(Tf)<=valor){
        valor<-nrow(Tf)
      }
      Tfmost <- as.data.frame(Tf[1:valor,])
      #Tfmost$freq <- as.numeric(Tfmost$freq)
      #construir histograma
      #grafico1 <-qplot(Tfmost$palabras, Tfmost$freq, geom = "bar", stat = "identity")
      print(ggplot(data = Tfmost, mapping = aes(x= V1, y= V2))+geom_bar(stat = "identity", fill = "blue")+ggtitle(paste("histograma de palabras frecuentes grupo ", i))+ylab("freq")+xlab("palabra"))
      #print(summary(as.numeric(paste(Tf$freq))))
    }
    
    
