# grafy pro analytickou část vyhodnocení

source("graphs/functions.R") # relative to the 'paper' directory

## načíst data
responses <- read.csv("../data/normalised.csv", na.strings=c(""))

## Kdo Signály používá - jsou nějaké významnější charakteristicky
# společné významné části respondentů?

all_respondents <- responses
women <- all_respondents[all_respondents$jsem == "žena", ]
daily <- women[grep("denně", women$na_signaly_chodim), ]
university <- women[grep("VŠ", women$moje_nejvyssi_dosazene_vzdelani), ]
subgroup_values <- c(
nrow(all_respondents),
nrow(women),
nrow(daily),
nrow(university)
)
subgroup_colnames <- c(
"všichni",
"denně",
"ženy",
"absolventky VŠ"
)
subgroups <- matrix(subgroup_values, ncol=length(subgroup_values), byrow=TRUE)
colnames(subgroups) <- subgroup_colnames
subgroup_table <- as.table(subgroups)

pdf(graph_path("kdo_signaly_pouziva"))
par(las=2) # popisky horizontálně
par(mar=c(5,8,4,2)) # okraje
barplot(subgroup_table, horiz=TRUE)
dev.off()

## Věk x pohlaví
pdf(graph_path("vek_x_pohlavi"), height=3.5, width=6)
tbl <- table(responses$jsem, responses$vek)
barplot(tbl, col=rev(red_to_blue(2)), cex.names=0.8)
dev.off()

## Stáří profilu x pohlaví
pdf(graph_path("profil_x_pohlavi"), height=3.5, width=6)
tbl <- table(responses$jsem, responses$na_signalech_mam_profil)
par(las=2) # orientace popisků
par(mar=c(8,3,4,2)) # okraje
barplot(tbl, col=rev(red_to_blue(2)), cex.names=0.8)
dev.off()

## Absolventi: typy škol
absolvents <- responses[grep("ne", responses$studuji), ]
school_types <-
    as.character(lapply(na.omit(absolvents$moje_nejvyssi_dosazene_vzdelani), function (x) {
        if (grepl("VŠ", x))
           "VŠ"
        else if (grepl("SŠ", x))
           "SŠ"
        else
           "jiná"
    }))
tbl <- table(school_types)
pdf(graph_path("vzdelani_sloucene"), height=4, width=6)
par(mai=c(0.2, 0.1, 0.2, 0.1)) # okraje
pie(tbl, col=rainbow(length(tbl)))
dev.off()

# Jak staré jsou autorizované profily?
authorized_users <- responses[responses$jsem_autorizovany == "ano", ]
pie_graph(authorized_users$na_signalech_mam_profil, graph_path("stari_autorizovanych_profilu"))

# Signály vs. Facebook
netOptions <- c("vůbec", "několikrát za měsíc nebo méně", "jednou nebo několikrát týdně", "každý den", "vícekrát za den")

active_signaly_users <- responses[grep("denně", responses$na_signaly_chodim), ]
frequency_graph(active_signaly_users[[rcolname("jine_site[Facebook]")]], netOptions, graph_path("nejaktivnejsi_signalnici_a_fb"), mar=c(5,14,4,2))

lessactive_signaly_users <- responses[!grepl("denně", responses$na_signaly_chodim), ]
frequency_graph(lessactive_signaly_users[[rcolname("jine_site[Facebook]")]], netOptions, graph_path("mene_aktivni_signalnici_a_fb"), mar=c(5,14,4,2))

# Noví uživatelé
new_users <- responses[grep("(rok nebo|2 roky)", responses$na_signalech_mam_profil, perl=TRUE), ]
print(nrow(new_users))

pie_graph(new_users$vek, graph_path("novi_vek"))
pie_graph(new_users$jsem, graph_path("novi_pohlavi"))
pie_graph(new_users$moje_nejvyssi_dosazene_vzdelani, graph_path("novi_vzdelani"))
pie_graph(new_users$na_signaly_chodim, graph_path("novi_jakcasto"))
pie_graph(new_users$o_signalech_jsem_se_poprve_dozvedel, graph_path("novi_dozvedelise"))

col <- new_users[[rcolname("neprijemne[co]")]]
up_what <- comma_separated_answers(col)
pdf(graph_path("novi_neprijemne"), height=3.5, width=6)
frequency <- sort(table(up_what))
par(las=2) # popisky horizontálně
par(mar=c(5,13,4,2)) # větší levý okraj
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()

reasons <- comma_doublespace_separated_answers(new_users$na_signaly_chodim_hlavne)
print(reasons)
pdf(graph_path("novi_chodimhlavne"), height=5, width=6)
frequency = sort(table(reasons))
par(las=2) # popisky horizontálně
par(mar=c(4,20,2,2)) # okraje
barplot(frequency, col=rainbow(length(frequency)), horiz=TRUE, cex.names=0.8)
dev.off()
