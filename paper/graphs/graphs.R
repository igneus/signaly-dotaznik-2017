# generuje grafy

source("graphs/functions.R") # relative to the 'paper' directory

## načíst data
responses <- read.csv("../data/normalised.csv", na.strings=c(""))

## koláčový graf rozložení odpovědí
columns = list(
"na_signalech_mam_profil", "o_signalech_jsem_se_poprve_dozvedel", "na_signaly_chodim",
"jsem_autorizovany",
"jsem", "vek", "studuji"
)

for (colname in columns) {
    col <- responses[[colname]]

    write_answer_count(responses, colname)
    pie_graph(col, graph_path(colname))
}

## Jsem žák / student

students <- responses[grep("ano", responses$studuji), ]

colname <- "studuji_co"
col <- students[[colname]]
write_answer_count(students, colname)
pie_graph(col, graph_path(colname))

## sloupcový graf, každá odpověď může obsahovat víc položek
# dalsi socialni site
colname <- "pouzivas_dalsi_socialni_site"
write_answer_count(responses, colname)
col <- responses[[rcolname(colname)]]
networks <- comma_separated_answers(col)
pdf(graph_path(colname))
frequency = sort(table(networks))
par(las=2) # popisky horizontálně
par(mar=c(5,8,4,2)) # okraje
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()

# neprijemne zazitky
colname <- "neprijemne[co]"
write_answer_count(responses, colname)
col <- responses[[rcolname(colname)]]
networks <- comma_separated_answers(col)
pdf(graph_path(colname), height=3.5, width=6) # velikost menších grafů
frequency = sort(table(networks))
par(las=2) # popisky horizontálně
par(mar=c(5,12,4,2)) # větší levý okraj
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()

colname <- "moje_nejvyssi_dosazene_vzdelani"
write_answer_count(responses, colname)
col <- responses[[rcolname(colname)]]
pdf(graph_path(colname))
frequency = sort(table(col))
par(las=2) # popisky horizontálně
par(mar=c(5,22,4,2)) # větší levý okraj
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()

## sloupcový graf, kažá odpověď může obsahovat víc položek,
# některé položky obsahují čárky
columns <- list("na_signaly_chodim_hlavne", "signaly_jsem_vyuzil_k", "na_signalech_se_mi_povedlo")

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

## frekvence využívání funkcionalit
functionalities = list("[status][pisu]", "[status][ctu]", "[status][komentuji]", "[sledovane_blogy][ctu]", "[blogy_vyber][ctu]", "[blogy_nesledovane][ctu]", "[blog][komentuji]", "[blog][pisu]", "[fotky][prohlizim]", "[fotky][vkladam]", "[akce][prohlizim]", "[akce][vkladam]", "[zed_spolecenstvi][ctu]", "[zed_spolecenstvi][pisu]", "[videa][prohlizim]", "[videa][vkladam]", "[autorizovani][chat]")
funcOptions <- c("vůbec", "méně než jednou týdně", "alespoň jednou týdně", "denně", "vícekrát za den")

authorized_users <- responses[responses$jsem_autorizovany == "ano", ]

for (f in functionalities) {
    csvcolname <- paste("funkcionality", f, sep="")

    f_responses <- responses
    if (f == "[autorizovani][chat]") {
       f_responses <- authorized_users
    }

    write_answer_count(f_responses, csvcolname)
    col <- f_responses[[rcolname(csvcolname)]]
    frequency_graph(col, funcOptions, graph_path(csvcolname))
}

## frekvence využívání jiných sociálních sítí
networks = list("[Facebook]", "[Twitter]", "[Youtube]", "[Instagram]", "[Googleplus]", "[Lidecz]")
netOptions = c("vůbec", "několikrát za měsíc nebo méně", "jednou nebo několikrát týdně", "každý den", "vícekrát za den")

for (n in networks) {
    csvcolname <- paste("jine_site", n, sep="")

    write_answer_count(responses, csvcolname)
    col <- responses[[rcolname(csvcolname)]]
    frequency_graph(col, netOptions, graph_path(csvcolname), mar=c(5,14,4,2))
}

## proč i v nynější konkurenci používám Signály
reasons = list("[jenom_signaly]", "[signaly_nejdulezitejsi]", "[kamaradi_kteri_jinde_nejsou]", "[zajimavi_lide_kteri_jinde_nejsou]", "[jedinecna_aktivita]", "[jedinecne_informace]", "[oblibene_blogy]", "[krestanske_prostredi]")
reasOptions <- c("zcela souhlasím", "spíše souhlasím", "ani souhlas, ani nesouhlas", "spíše nesouhlasím", "vůbec nesouhlasím")

for (r in reasons) {
    csvcolname = paste("proc_signaly", r, sep="")

    write_answer_count(responses, csvcolname)
    col <- responses[[rcolname(csvcolname)]]

    agree_disagree_graph(col, rev(reasOptions), graph_path(csvcolname))
}

## volná otázka - necháme si uložit pouze počet odpovědí
write_answer_count(responses, "proc_signaly_jine")

souhlas_jedno_nesouhlas <- c("zcela souhlasím", "spíše souhlasím", "je mi to jedno", "spíše nesouhlasím", "vůbec nesouhlasím")

## já a ostatní uživatelé
interactions <- list("[tesi_me][libi_se][kamaradi]", "[tesi_me][libi_se][cizi]", "[tesi_me][komentar][kamaradi]", "[tesi_me][komentar][cizi]", "[tesi_me][zprava][kamaradi]", "[tesi_me][zprava][cizi]", "[autorizovani_duveryhodnejsi]", "[neprijemne][komunikace_obtezuje]", "[neprijemne][neprijemne_situace]", "[neprijemne][zazivat_nechci]")

for (i in interactions) {
    csvcolname <- paste("ostatni", i, sep="")

    write_answer_count(responses, csvcolname)
    col <- responses[[rcolname(csvcolname)]]

    agree_disagree_graph(col, rev(souhlas_jedno_nesouhlas), graph_path(csvcolname))
}

## Nepříjemné ...

up_cfgs = list(
    list(selector="Nepříjemné komentáře", colnamepart="[komentare]"),
    list(selector="Nepříjemné vzkazy", colnamepart="[vzkazy]")
)

up_reactions = list("[zvazoval_odchod]", "[zakazat]", "[pisatele_pryc]")

for (cfg in up_cfgs) {
    # jen respondenti, kteří daný druh "nepříjemnosti" zažili
    relevant_responses <- responses[grep(cfg$selector, responses[[rcolname("neprijemne[co]")]]), ]

    ## ... mi byly nepříjemné, protože ...

    csvcolname = paste("neprijemne", cfg$colnamepart, "[protoze]", sep="")
    write_answer_count(relevant_responses, csvcolname)
    col <- relevant_responses[[rcolname(csvcolname)]]

    reasons <- unlist(lapply(col, function (x) strsplit(as.character(x), ",  "))) # čárka a dvě mezery, protože odpovědi často obsahují čárky

    pdf(graph_path(csvcolname))
    frequency = sort(table(reasons))
    par(las=2) # popisky horizontálně
    par(mar=c(5,20,4,2)) # okraje
    barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
    dev.off()

    ## ... uvažoval jsem, že kvůli nim odejdu, atd.

    for (r in up_reactions) {
        csvcolname <- paste("neprijemne", cfg$colnamepart, r, sep="")

        write_answer_count(relevant_responses, csvcolname)
        col <- relevant_responses[[rcolname(csvcolname)]]

        agree_disagree_graph(col, rev(souhlas_jedno_nesouhlas), graph_path(csvcolname))
    }
}

## vypsat varování
warnings()
