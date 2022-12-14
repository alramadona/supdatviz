library(rtweet)
library(ggplot2)

library(dplyr)
library(stringi)
library(wordcloud)

auth_setup_default()

?search_tweets
tw_fkkmk <- search_tweets("kopi", n = 10000,
                          retryonratelimit = T,
                          include_rts = FALSE)

saveRDS(tw_fkkmk, "dat/tw_fkkmk.rds")

ts_plot(tw_fkkmk) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of 'teh' Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )


# analisis teks -----------------------------------------------------------

comments_text <- as.character(tw_fkkmk$full_text)
comments_text = tibble(text = Reduce(c, comments_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

remove = c("ada","adanya","adalah","adapun","agak","agaknya","agar",
           "akan","akankah","akhirnya","aku","akulah","amat","amatlah",
           "anda","andalah","antar","diantaranya","antara","antaranya",
           "diantara","apa","apaan","mengapa","apabila","apakah",
           "apalagi","apatah","atau","ataukah","ataupun","bagai",
           "bagaikan","sebagai","sebagainya","bagaimana","bagaimanapun",
           "sebagaimana","bagaimanakah","bagi","bahkan","bahwa",
           "bahwasanya","sebaliknya","banyak","sebanyak","beberapa",
           "seberapa","begini","beginian","beginikah","beginilah",
           "sebegini","begitu","begitukah","begitulah","begitupun",
           "sebegitu","belum","belumlah","sebelum","sebelumnya",
           "sebenarnya","berapa","berapakah","berapalah","berapapun",
           "betulkah","sebetulnya","biasa","biasanya","bila","bilakah",
           "bisa","bisakah","sebisanya","boleh","bolehkah","bolehlah",
           "buat","bukan","bukankah","bukanlah","bukannya","cuma",
           "percuma","dahulu","dalam","dan","dapat","dari","daripada",
           "dekat","demi","demikian","demikianlah","sedemikian","dengan",
           "depan","di","dia","dialah","dini","diri","dirinya","terdiri",
           "dong","dulu","enggak","enggaknya","entah","entahlah",
           "terhadap","terhadapnya","hal","hampir","hanya","hanyalah",
           "harus","haruslah","harusnya","seharusnya","hendak",
           "hendaklah","hendaknya","hingga","sehingga","ia","ialah",
           "ibarat","ingin","inginkah","inginkan","ini","inikah",
           "inilah","itu","itukah","itulah","jangan","jangankan",
           "janganlah","jika","jikalau","juga","justru","kala","kalau",
           "kalaulah","kalaupun","kalian","kami","kamilah","kamu",
           "kamulah","kan","kapan","kapankah","kapanpun","dikarenakan",
           "karena","karenanya","ke","kecil","kemudian","kenapa",
           "kepada","kepadanya","ketika","seketika","khususnya","kini",
           "kinilah","kiranya","sekiranya","kita","kitalah","kok","lagi",
           "lagian","selagi","lah","lain","lainnya","melainkan","selaku",
           "lalu","melalui","terlalu","lama","lamanya","selama","selama",
           "selamanya","lebih","terlebih","bermacam","macam","semacam",
           "maka","makanya","makin","malah","malahan","mampu","mampukah",
           "mana","manakala","manalagi","masih","masihkah","semasih",
           "masing","mau","maupun","semaunya","memang","mereka",
           "merekalah","meski","meskipun","semula","mungkin","mungkinkah",
           "nah","namun","nanti","nantinya","nyaris","oleh","olehnya",
           "seorang","seseorang","pada","padanya","padahal","paling",
           "sepanjang","pantas","sepantasnya","sepantasnyalah","para",
           "pasti","pastilah","per","pernah","pula","pun","merupakan",
           "rupanya","serupa","saat","saatnya","sesaat","saja","sajalah",
           "saling","bersama","sama","sesama","sambil","sampai","sana",
           "sangat","sangatlah","saya","sayalah","se","sebab","sebabnya",
           "sebuah","tersebut","tersebutlah","sedang","sedangkan",
           "sedikit","sedikitnya","segala","segalanya","segera",
           "sesegera","sejak","sejenak","sekali","sekalian","sekalipun",
           "sesekali","sekaligus","sekarang","sekarang","sekitar",
           "sekitarnya","sela","selain","selalu","seluruh","seluruhnya",
           "semakin","sementara","sempat","semua","semuanya","sendiri",
           "sendirinya","seolah","seperti","sepertinya","sering",
           "seringnya","serta","siapa","siapakah","siapapun","disini",
           "disinilah","sini","sinilah","sesuatu","sesuatunya","suatu",
           "sesudah","sesudahnya","sudah","sudahkah","sudahlah","supaya",
           "tadi","tadinya","tak","tanpa","setelah","telah","tentang",
           "tentu","tentulah","tentunya","tertentu","seterusnya","tapi",
           "tetapi","setiap","tiap","setidaknya","tidak","tidakkah",
           "tidaklah","toh","waduh","wah","wahai","sewaktu","walau",
           "walaupun","wong","yaitu","yakni","yang")
words = tibble(word = Reduce(c, stri_extract_all_words(comments_text$text))) %>%
  group_by(word) %>% count() %>% arrange(desc(n)) %>% filter(nchar(word) >= 3) %>%
  filter(n > 2 & word %in% remove == FALSE) 

words <- words[c(1:500),]

set.seed(3)
wordcloud(words$word, words$n, random.order = FALSE, random.color = TRUE,
          rot.per = 0.3, colors = 1:nrow(words))
