write_answer_count <- function (responses, csvcolname) {
    count <- length(na.omit(responses[[rcolname(colname)]]))

    path <- paste("graphs/counts/", basefilename(csvcolname), ".txt", sep="")
    sink(path)
    cat(count)
    sink()
}

graph_path <- function (csvcolname)
    paste("graphs/img/", basefilename(csvcolname), ".pdf", sep="")

# jak se sloupec z csv souboru jmenuje po načtení:
# R hranaté závorky v názvech sloupců nemá rádo a načte je jako tečky
rcolname <- function (csvcolname)
    gsub("[\\[\\]]", ".", csvcolname, perl=TRUE)

# podoba názvu sloupce vhodná pro název souboru (s grafem atd.)
basefilename <- function (csvcolname)
    gsub("[\\[\\]]+", "_", gsub("]$", "", csvcolname, perl=TRUE), perl=TRUE)

# returns a palette-generating _function_ for "semaphore-like"
# red-to-green colour transition
# credits: https://stackoverflow.com/a/13353264/2034213
red_to_green <- colorRampPalette(c("red", "yellow", "darkgreen"))

# ze sloupce, kde každá buňka obsahuje jednu nebo víc odpovědí
# oddělených čárkou, vrátí jeden plochý list se všemi odpověďmi
comma_separated_answers <- function (data)
    unlist(lapply(col, function (x) strsplit(as.character(x), ", ")))

# graf pro otázky s pevnou škálou od úplného souhlasu po nesouhlas
agree_disagree_graph <- function (data, options, save_path) {
    frequency <- table(data)
    frequency <- frequency[options] # seřadit podle daného pořadí

    pdf(save_path, height=3.5, width=6)
    par(las=2) # popisky horizontálně
    par(mar=c(5,12,4,2)) # okraje
    barplot(frequency, col=red_to_green(length(frequency)), horiz=TRUE, xlim=c(0, 140))
    dev.off()
}

# graf pro otázky s pevnou škálou odpovědí na otázku "jak často"
frequency_graph <- function (data, options, save_path, mar=c(5,10,4,2)) {
    frequency <- table(data)
    frequency <- frequency[options] # seřadit podle daného pořadí

    pdf(save_path, height=3.5, width=6)
    par(las=2) # popisky horizontálně
    par(mar=mar) # okraje
    colours <- rev(heat.colors(length(frequency)))
    # parametr xlim nastavuje rozsah osy x.
    # Použit je proto, aby všechny grafy v řadě měly osu stejně
    # rozvrženou a daly se od oka srovnávat
    barplot(frequency, horiz=TRUE, col=colours, xlim=c(0, 160))
    dev.off()
}

# koláčový graf
pie_graph <- function (data, save_path) {
    pdf(save_path, height=4, width=6)
    # the `as.character` is useful when working with subsets:
    # subsets inherit complete factors from the original data frame,
    # but some of them may have no entry. We want to get rid of these.
    frequency <- sort(table(as.character(data)))
    par(mai=c(0.2, 0.1, 0.2, 0.1)) # okraje
    pie(frequency, col=rainbow(length(frequency)))
    dev.off()
}
