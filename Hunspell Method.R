library(hunspell)
words <- c("beer", "wisley", "wine", "wek")
hunspell_check(words)

bad_words <- hunspell_find("spell wek plz checkers are not necesary availbel for languae ninja's")
print(bad_words)

bad_words[[1]][3]

(hunspell_suggest(bad_words[[1]][2]))[[1]][1]

(hunspell_suggest(bad_words[[1]][2]))

hunspell_analyze(words)

hunspell_stem(words[[1]])

hunspell_suggest("week")

correct("wek")
