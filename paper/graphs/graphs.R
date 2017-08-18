# jednoduché rozložení odpovědí pro jednotlivé otázky

responses <- read.csv("../data/normalised.csv", na.strings=c(""))

columns = list("na_signalech_mam_profil", "o_signalech_jsem_se_poprve_dozvedel", "na_signaly_chodim", "na_signaly_chodim_hlavne", "signaly_jsem_vyuzil_k", "na_signalech_se_mi_povedlo", "jsem_autorizovany")

for (colname in columns) {
    col <- responses[[colname]]

    # kolik uživatelů na otázku odpovědělo?
    responseCount = length(na.omit(col))
    print(colname)
    print(responseCount)

    # graf
    path <- paste("graphs/img/", colname, ".png", sep="")
    png(filename=path)
    pie(table(col))
    dev.off()
}