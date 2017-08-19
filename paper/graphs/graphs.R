# jednoduché rozložení odpovědí pro jednotlivé otázky

write_answer_count <- function (responses, colname) {
    count <- length(na.omit(responses[[colname]]))

    path <- paste("graphs/counts/", colname, ".txt", sep="")
    sink(path)
    cat(count)
    sink()
}

graph_path <- function (colname) paste("graphs/img/", colname, ".pdf", sep="")

responses <- read.csv("../data/normalised.csv", na.strings=c(""))

# koláčový graf rozložení odpovědí
columns = list(
"na_signalech_mam_profil", "o_signalech_jsem_se_poprve_dozvedel", "na_signaly_chodim",
"jsem_autorizovany",
"na_signalech_jsem_zazil",
"jsem", "vek", "studuji", "moje_nejvyssi_dosazene_vzdelani"
)

for (colname in columns) {
    col <- responses[[colname]]

    # kolik uživatelů na otázku odpovědělo?
    write_answer_count(responses, colname)

    # graf
    pdf(graph_path(colname))
    frequency = sort(table(col))
    pie(frequency, col=rainbow(length(frequency)))
    dev.off()
}

# sloupcový graf, kažá odpověď může obsahovat víc položek
columns <- list("pouzivas_dalsi_socialni_site", "signaly_jsem_vyuzil_k", "na_signalech_se_mi_povedlo")

for (colname in columns) {
    write_answer_count(responses, colname)
    col <- responses[[colname]]

    networks <- unlist(lapply(col, function (x) strsplit(as.character(x), ", ")))

    pdf(graph_path(colname))
    frequency = sort(table(networks))
    par(las=2) # popisky horizontálně
    par(mar=c(5,8,4,2)) # okraje
    barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
    dev.off()
}

# sloupcový graf, kažá odpověď může obsahovat víc položek,
# některé položky obsahují čárky
columns <- list("na_signaly_chodim_hlavne")

for (colname in columns) {
    write_answer_count(responses, colname)
    col <- responses[[colname]]

    networks <- unlist(lapply(col, function (x) strsplit(as.character(x), ",  "))) # čárka a dvě mezery, protože odpovědi často obsahují čárky

    pdf(graph_path(colname))
    frequency = sort(table(networks))
    par(las=2) # popisky horizontálně
    par(mar=c(5,20,4,2)) # okraje
    barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
    dev.off()
}

# vypsat varování
warnings()