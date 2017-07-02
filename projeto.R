# Projeto de avaliacao de avenidas movimentadas do Recife
# Vitor de Albuquerque Torreao (vat@cin.ufpe.br)

# funcoes de apoio

namesplit <- function(name, maxchars=50) {
    if (nchar(name) <= maxchars) {
        return (name)
    }
    words <- strsplit(name, " ")[[1]]
    size <- length(words)
    if (size < 1) {
        return (name)
    }
    result <- words[1]
    currLineSize <- nchar(result)
    ind = 2
    while (ind <= size) {
        word <- words[ind]
        wordSize <- nchar(word)
        if (currLineSize + wordSize + 1 > maxchars) {
            result <- paste(result, word, sep="\n")
            currLineSize <- wordSize
        }
        else {
            result <- paste(result, word, sep=" ")
            currLineSize <- currLineSize + wordSize + 1
        }
        ind <- ind + 1
    }
    return (result)
}

# Le os dados da base
dataset <- read.csv(file="fluxo-de-veiculo.csv",
                    header=T, sep=";",
                    encoding="utf-8",
                    colClasses=c("character", "character",
                                 "character", rep("numeric", 10)))

# Elimina as linhas que nao contem logradouro
dataset <- dataset[dataset$Logradouro != "",]

# Soma o fluxo de veiculos de cada tipo para obter o total
for (i in 1:nrow(dataset)) {
    dataset$Fluxo[i] <- sum(dataset[i,4:13])
}

# Deleta as colunas desnecessarias
dataset <- dataset[ , !(names(dataset) %in% names(dataset)[2:13])]

# Recupera os logradouros que estao na base
streets <- unique(dataset$Logradouro)

# cria a lista de amostras
samples <- list() # inicializa vazio
for (street in streets) {
    sample <- list(dataset$Fluxo[dataset$Logradouro == street])
    samples <- c(samples, sample)
}

# plota os graficos
ind <- 1
for (sample in samples) {
    title <- namesplit(street, 50)
    png(paste("histograms/street", ind, ".png", sep=""))
    hist(sample,
         main=title,
         xlab="Fluxo de Veiculos",
         ylab="Frequencia")
    dev.off()
    png(paste("boxplots/street", ind, ".png", sep=""))
    boxplot(sample, main=title)
    dev.off()
    png(paste("qqplots/street", ind, ".png", sep=""))
    normline <- rnorm(100, mean=mean(sample), sd=sd(sample))
    qqnorm(sample);qqline(normline, col=2)
    dev.off()
    ind <- ind + 1
}
