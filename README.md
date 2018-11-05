# ufcwordcloud
# UFC tweets wordcloud

```{r message=FALSE, warning=FALSE}
library(twitteR)
library(tm)
library(qdap)
library(wordcloud2)
```

```{r message=FALSE, warning=FALSE, include=FALSE}
# Change consumer_key, consume_secret, access_token, and 
# access_secret based on your own keys
consumer_key <- ""
consumer_secret <-""
access_token <- ""
access_secret <- "" 
```

```{r}
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
```

```{r}
tw = searchTwitter('#UFC', n = 2000)
tw <- strip_retweets(tw, strip_manual=TRUE, strip_mt=TRUE)
tweets = twListToDF(tw)
```


```{r}
qdap_clean <- function(x){
                x <- replace_abbreviation(x)
                x <- replace_contraction(x)
                x <- replace_ordinal(x)
                x <- replace_ordinal(x)
                x <- replace_symbol(x)
                x <- tolower(x)
                return(x)}
```

```{r}
tweets_text <- tweets$text
```

```{r}
tweets_qdapclean <- qdap_clean(tweets_text)
```

```{r}
source <- VectorSource(tweets_qdapclean)
corpus <- VCorpus(source)
```

```{r}
content(corpus[[10]])
```

```{r}
tm_clean <- function(corpus){
                corpus <- tm_map(corpus, removePunctuation)
                corpus <- tm_map(corpus, stripWhitespace)
                corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "day", "debitcredit",
                                                        "iptvgeolocked", "vpn", "꽁머니", "드라마 ",
                                                        "김창수  ", "온에어 ", "김창수", "윤정수",
                                                        "number", "ufc", "mma", "free", "cards",
                                                        "1만이상", "sites", "websites", "required",
                                                        "test", "trial", "openup", "without"))
                return(corpus)
}
```


```{r}
corpus_clean <- tm_clean(corpus)
content(corpus_clean[[10]])
```

```{r}
dtm <- TermDocumentMatrix(corpus_clean)
dtm_m <- as.matrix(dtm)
```

```{r}
term_frequency <- rowSums(dtm_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency[1:20]
```

```{r}
barplot(term_frequency[1:10], col = "tan", las = 2)
```

```{r}
freq <- head(term_frequency, 400)
tabla_freq <- as.data.frame(freq)
names(tabla_freq)[1]<-paste("freq")
tabla_freq$word <- row.names(tabla_freq)
tabla_final <- tabla_freq[, c("word", "freq")]

wordcloud2(tabla_final, 
           size = 0.5,
           backgroundColor = "white",
           shape = "octagon"
           )
```
