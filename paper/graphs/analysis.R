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
print(subgroup_table)

pdf(graph_path("kdo_signaly_pouziva"))
par(las=2) # popisky horizontálně
par(mar=c(5,8,4,2)) # okraje
barplot(subgroup_table, horiz=TRUE)
dev.off()