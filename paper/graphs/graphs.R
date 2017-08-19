# jednoduché rozložení odpovědí pro jednotlivé otázky

write_answer_count <- function (responses, colname) {
    count <- length(na.omit(responses[[colname]]))

    path <- paste("graphs/counts/", colname, ".txt", sep="")
    sink(path)
    cat(count)
    sink()
}

responses <- read.csv("../data/normalised.csv", na.strings=c(""))

columns = list(
"na_signalech_mam_profil", "o_signalech_jsem_se_poprve_dozvedel", "na_signaly_chodim",
"na_signaly_chodim_hlavne", "signaly_jsem_vyuzil_k", "na_signalech_se_mi_povedlo", "jsem_autorizovany",
"na_signalech_jsem_zazil",
"jsem", "vek", "studuji", "moje_nejvyssi_dosazene_vzdelani"
)

for (colname in columns) {
    col <- responses[[colname]]

    # kolik uživatelů na otázku odpovědělo?
    write_answer_count(responses, colname)

    # graf
    path <- paste("graphs/img/", colname, ".pdf", sep="")
    pdf(path)
    frequency = sort(table(col))
    pie(frequency, col=rainbow(length(frequency)))
    dev.off()
}

# další sociální sítě: každý řádek může obsahovat několik hodnot
colname <- "pouzivas_dalsi_socialni_site"
write_answer_count(responses, colname)
col <- responses[[colname]]

networks <- unlist(lapply(col, function (x) strsplit(as.character(x), ", ")))

path <- paste("graphs/img/", colname, ".pdf", sep="")
pdf(path)
frequency = sort(table(networks))
par(las=2) # popisky horizontálně
par(mar=c(5,8,4,2)) # okraje
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()

# vypsat varování
warnings()