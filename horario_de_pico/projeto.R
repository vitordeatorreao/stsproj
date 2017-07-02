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
dataset <- read.csv(file="../fluxo-de-veiculo.csv",
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

# Recupera os logradouros que estao na base
streets <- unique(dataset$Logradouro)

# cria a lista de amostras
samples <- list() # inicializa vazio
for (street in streets) {
  if (street == "AVENIDA BOA VIAGEM - ENTRE OS NRS. 6114 E 5888") {
    datasubset <- dataset[dataset$Logradouro == street,][651:1322,]
  } else if (street == "AVENIDA BOA VIAGEM - TERCEIRO JARDIM") {
    datasubset <- dataset[dataset$Logradouro == street,][662:1329,]
  } else if (street == "AV. GOV. AGAMENON MAGALHAES - PROX. VIADUTO PRES. MEDICI") {
    datasubset <- dataset[dataset$Logradouro == street,][668:1338,]
  } else if (street == "AV. MAL. MASCARENHAS DE MORAES, AEROP BAIRRO IMBIRIBEIRA") {
    datasubset <- dataset[dataset$Logradouro == street,][1:670,]
  } else {
    datasubset <- dataset[dataset$Logradouro == street,]
  }
  fluxVector <- vector()
  i <- 1
  while (i <= nrow(datasubset)) {
    hour <- substring(datasubset$Hora[i], 1, 2)
    if (hour == "08" || hour == "09" ||
        hour == "18" || hour == "19") {
      fluxVector <- c(fluxVector, datasubset$Fluxo[i])
    }
    i <- i + 1
  }
  size <- length(fluxVector)
  while (size < 112) {
    fluxVector <- c(fluxVector, NA)
    size <- size + 1
  }
  sample <- list(fluxVector)
  samples <- c(samples, sample)
}

# plota os graficos
ind <- 1
for (sample in samples) {
  title <- namesplit(streets[ind], 50)
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
  normline <- rnorm(100, mean=mean(sample, na.rm=T), sd=sd(sample, na.rm=T))
  qqnorm(sample);qqline(normline, col=2)
  dev.off()
  print(streets[ind])
  print(paste("Tamanho: ", length(sample)))
  print(paste("Media:", mean(sample, na.rm=T)))
  print(paste("Desvio Padrao:", sd(sample, na.rm=T)))
  print(paste("Mediana:", median(sample, na.rm=T)))
  testResult <- shapiro.test(sample)
  print(paste("Shapiro Wilk p-valor:", testResult[[2]]))
  if (testResult[[2]] > 0.05) {
    print(paste("Pode ser considerado normal, como resultado do teste",
                "Shapiro-Wilk com significancia 5%"))
  } else {
    print(paste("Nao pode ser considerado normal, como resultado do teste",
                "Shapiro-Wilk com significancia 5%"))
  }
  print("==================================================================")
  ind <- ind + 1
}

# para o teste de friedman
i <- 3
datamatrix <- cbind(samples[[1]], samples[[2]])
while (i <= 20) {
  datamatrix <- cbind(datamatrix, samples[[i]])
  i <- i + 1
}

# para o teste de Wilcoxon
samples.means <- vector()
for (sample in samples) {
  samples.means <- c(samples.means, mean(sample, na.rm = T))
}

sortedMeans <- sort(samples.means)

j <- 1
equals <- list()
while (j <= 20) {
  equal.to <- vector()
  k <- 1
  while (k <= 20) {
    if (j == k) {
      k <- k + 1
      next
    }
    street1.name <- streets[j]
    street2.name <- streets[k]
    if (shapiro.test(samples[[j]])[[2]] > 0.05 &&
        shapiro.test(samples[[k]])[[2]] > 0.05) {
      p.value <- t.test(samples[[j]], samples[[k]])[[3]]
    } else {
      p.value <- wilcox.test(samples[[j]], samples[[k]])[3]
    }
    if (p.value > 0.05) {
      equal.to <- c(equal.to, k)
    }
    k <- k + 1
  }
  equals <- c(equals, list(equal.to))
  j <- j + 1
}

print("Igualdades:")

for (ind in 1:20) {
  street1 <- streets[ind]
  for (ind2 in equals[[ind]]) {
    street2 <- streets[ind2]
    strBuilder <- paste(street1, street2, sep=" = ")
    p.valor <- wilcox.test(samples[[ind]], samples[[ind2]])[3]
    strBuilder <- paste(strBuilder, p.valor, sep=", p-valor: ")
    print(strBuilder)
  }
}
print("======================================================================")

# ranking segundo wilcoxon
print("Ranking calculado com o teste de Wilcoxon")
sortedInd <- 20
rank <- 1
ranked <- vector()
while (length(ranked) < 20) {
  ind1 <- which(samples.means == sortedMeans[sortedInd])
  sortedInd <- sortedInd - 1
  street.name <- streets[ind1]
  sample1 <- samples[[ind1]]
  if (ind1 %in% ranked) {
    next
  }
  print(paste(rank, ") ", street.name, sep = ""))
  ranked <- c(ranked, ind1)
  for (ind2 in equals[[ind1]]) {
    if (ind2 %in% ranked) {
      next
    }
    street2.name <- streets[[ind2]]
    print(paste(rank, ") ", street2.name, sep = ""))
    ranked <- c(ranked, ind2)
  }
  rank <- rank + 1
}
print("======================================================================")
