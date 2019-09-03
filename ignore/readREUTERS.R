library(tosca)

article = readLines("reut2-000.xml", encoding = "UTF-8")

lines <- grep(pattern = "</REUTERS>", article)
lines <- cbind(c(1, lines[-length(lines)]), lines)
article <- apply(lines, 1, function(x) paste(article[x[1]:x[2]], collapse = " "))
article = article[1:100]
id <- stringr::str_extract(article, "ID=\"(.*?)\"")
id <- gsub(pattern = "ID=", replacement = "", x = id)
id <- gsub(pattern = "\"", replacement = "", x = id)

date = as.Date("1987-02-26")
title = removeHTML(removeXML(
  gsub(pattern = "&lt;", replacement = "<", x = stringr::str_extract(article, "<TITLE>(.*?)</TITLE>"))))

text = gsub(
  pattern = "</?BODY>", replacement = "",
  x = stringr::str_extract(article, "<BODY>(.*?)</BODY>"))
names(text) = id

obj = textmeta(
  meta = data.frame(id = id, date = date, title = title, stringsAsFactors = FALSE),
  text = text)

obj = cleanTexts(obj, sw = c(tm::stopwords(), "Reuters"))

wl =makeWordlist(obj$text)

reuters_vocab = wl$words
reuters_docs = LDAprep(obj$text, vocab)

save(reuters_docs, file = "reuters_docs.rda")
save(reuters_vocab, file = "reuters_vocab.rda")
