# Wir basteln uns ein Postillon-Korpus!
# ---------------------------------------


# Bevor wir beginnen: Beachten Sie bitte, dass allzu vehementes Crawlen
# Server stark belasten kann. Deshalb lassen Sie mich bitte das Skript
# im Seminar demonstrieren und führen Sie es nicht gleichzeitig selbst aus...


# Startseite des Postillon einlesen
postillon <- scan("http://www.der-postillon.com/", # selbsterklärend
                  what="character",                # was soll eingelesen werden? Textzeichen
                  sep="\n")                        # Zeilenumbruch trennt die einzelnen sog. Felder

# Seiten finden, auf die die Startseite verlinkt
seiten <- grep("<a href=\'http://www.der-postillon.com/.*\\.html", 
               postillon, 
               value=T)

# Die Teile, die nicht zum Link selbst gehören, entfernen
seiten <- gsub("<a href=\'|\'>",    # ersetze diesen String...
          "",                  # ... durch diesen String
          seiten)              # ... und zwar in diesem Objekt.


# den ersten Link einlesen
post1 <- scan(seiten[1], what="character", sep="\n")

# Die Inspektion des Seitenquelltexts zeigt, dass der eigentliche Text mit
# "<div class='post-body entry-content post-body-item'" beginnt
# und beim nächsten "</div> wieder endet.
# Daher suchen wir zunächst "<div class='post-body entry-content post-body-item'"

text_start <- grep("<div class='post-body entry-content post-body-item'", post1)

# Jetzt suchen wir die "</div>"-Tags:
div <- grep("^</div>", post1)


# Davon brauchen wir den, der auf "div class='post-body entry-content..." etc. folgt -
# also den ersten, dessen Zeilennummer größer ist als die, die wir als text_start gespeichert 
# haben.
text_end <- div[which(div>text_start)[1]]


# Jetzt können wir uns den Text anschauen:
post1[text_start:text_end]

# ... und den relevanten Text in ein eigenes Objekt extrahieren.
unser_text <- post1[text_start:text_end]


# Nun entfernen wir alle Zeilen, die mit "<" beginnen und nicht zum eigentlichen
# Text gehören.
unser_text <- unser_text[-grep("^<", unser_text)]

# Jetzt entfernen wir die "<br />" Tags, mit denen die Absatzumbrüche markiert sind.
unser_text <- gsub("<br />", # wieder: ersetze diesen String...
              " ",            # ... durch diesen String
              unser_text)    # ... und zwar in diesem Objekt.


# Als nächstes wollen wir den Text in einzelne Wörter aufsplitten.
# Wir wollen aber, dass dabei die Satzzeichen abgetrennt sind.
# Deshalb setzen wir vor allen nicht-alphanumerischen Zeichen ein Leerzeichen ein.
# Dafür benutzen wir reguläre Ausdrücke: Das ^ bedeutet "nicht", [:alnum:] bedeutet
# alphanumerisch.
unser_text <- gsub("(?=[^[:alnum:]])",  # (?=) ist eine sog. lookahead assertion.
                                        # Damit finden wir die Position VOR dem Zeichen nach dem =.
                                        # so können wir "gsub", das eigentlich zum Ersetzen da ist,
                                        # verwenden, ohne wirklich etwas zu ersetzen.
                                        # Stattdessen fügen wir ein Leerzeichen vor dem relevanten
                                        # Zeichen ein.
     " ", 
     unser_text, 
     perl = T)

# Jetzt splitten wir den Text auf, und zwar überall dort, wo Leerzeichen sind.
unser_text <- unlist(strsplit(unser_text, " "))

# Wir entfernen alle Elemente im Vektor, die keinen Text enthalten.
unser_text <- unser_text[which(unser_text!="")]


# Wir schauen uns das Ergebnis an...
unser_text


# ... und freuen uns!
# Aber jetzt wollen wir das Ganze auch für die anderen Texte wiederholen.
# Dafür benutzen wir einen Loop, in den wir alles das einbauen, was wir soeben gemacht haben.

for(i in 2:length(seiten)) {
  post1 <- scan(seiten[i], what="character", sep="\n")
  
  # Anfang des Textes finden
  text_start <- grep("<div class='post-body entry-content post-body-item'", post1)
  
  # Ende des Textes finden
  div <- grep("^</div>", post1)
  text_end <- div[which(div>text_start)[1]]
  
  # Relevanten Text extrahieren
  unser_text2 <- post1[text_start:text_end]
  
  
  # Nun entfernen wir alle Zeilen, die mit "<" beginnen und nicht zum eigentlichen
  # Text gehören.
  unser_text2 <- unser_text2[-grep("^<", unser_text2)]
  
  # Absatzumbruch-Marker entfernen
  unser_text2 <- gsub("<br />", # wieder: ersetze diesen String...
                     " ",            # ... durch diesen String
                     unser_text2)    # ... und zwar in diesem Objekt.
  
  # Leerzeichen vor Satzzeichen
  unser_text2 <- gsub("(?=[^[:alnum:]])",
                     " ", 
                     unser_text2, 
                     perl = T)
  
  # Text aufsplitten
  unser_text2 <- unlist(strsplit(unser_text2, " "))
  
  # Leere Elemente entfernen
  unser_text2 <- unser_text2[which(unser_text2!="")]
  
  # das Ganze an den bereits existierenden Text anhängen:
  unser_text <- c(unser_text, unser_text2)
  
  # Fortschritt anzeigen
  print(paste(i, " von ", length(seiten), " Seiten gecrawlt", sep="", collapse=""))
  
}


# Jetzt speichern wir den Text in einer Textdatei.
write.table(unser_text,        # das Objekt, das in die Datei geschrieben werden soll
            "post2018_1.txt",       # der Dateiname
            quote=F,           # die einzelnen Wörter sollen nicht in Anführungszeichen stehen
            row.names = F,     # keine Zeilennummerierung
            col.names = F,     # keine Spaltennamen (sonst fügt er ganz oben ein "x" ein)
            fileEncoding="UTF-8")







# Paket koRpus installieren und laden
# Hinweis: Um koRpus zu benutzen, muss der TreeTagger installiert sein!
if(!is.element("koRpus", installed.packages())) { install.packages("koRpus") }
library(koRpus)

# Vektor mit der Funktion treetag aus dem Paket koRpus taggen
# Hinweis: Pfad zum TreeTagger muss natürlich angepasst werden!
# Beachten Sie, dass unter Windows statt / bei Pfadangaben doppelte Backslashes \\
# verwendet werden müssen.
unser_getaggter_text <- treetag(unser_text,
                                treetagger = "manual", 
                                TT.options = list(path = "/Users/stefanhartmann/Downloads/TreeTagger/",
                                                          preset = "de"), lang = "de", format = "obj")

unser_getaggter_text

# alternativ können wir den TreeTagger in der Kommandozeile verwenden

# Befehle dafür (Pfadangaben müssen natürlich angepasst werden):
# cd "/Users/stefanhartmann/Dropbox/Privat/Siegen2018/Siegen"
# tree-tagger-german post2018_1.txt > post2018_2.txt

# jetzt lesen wir den Text wieder ein: (entkommentieren)
# unser_getaggter_text <- read.table("post2.txt", sep="\t", 
#                                   head=F, encoding="UTF-8", quote="", fill=T)
# unser_getaggter_text
