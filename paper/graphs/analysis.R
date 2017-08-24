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
subgroups = matrix(subgroup_values, ncol=length(subgroup_values), byrow=TRUE)
colnames(subgroups) <- subgroup_colnames
subgroup_table <- as.table(subgroups)

pdf(graph_path("kdo_signaly_pouziva"))
par(las=2) # popisky horizontálně
par(mar=c(5,8,4,2)) # okraje
barplot(subgroup_table, horiz=TRUE)
dev.off()

## Věk x pohlaví
pdf(graph_path("vek_x_pohlavi"), height=3.5, width=6)
tbl = table(responses$jsem, responses$vek)
barplot(tbl, col=rev(red_to_blue(2)), cex.names=0.8)
dev.off()

## Stáří profilu x pohlaví
pdf(graph_path("profil_x_pohlavi"), height=3.5, width=6)
tbl = table(responses$jsem, responses$na_signalech_mam_profil)
par(las=2) # orientace popisků
par(mar=c(8,3,4,2)) # okraje
barplot(tbl, col=rev(red_to_blue(2)), cex.names=0.8)
dev.off()
