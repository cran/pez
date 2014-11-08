## ----include=FALSE--------------------
require(plotrix)
require(pez)
require(ape)
require(picante)
require(methods)
options(width=40)

## ----tidy=TRUE------------------------
library(pez)
data(laja)
data <- comparative.comm(invert.tree, river.sites, invert.traits, river.env)

## ----tidy=TRUE------------------------
site.subset <- data[1:5,]
spp.subset <- data[,1:3]
data[-1,,warn=TRUE]

## ----tidy=TRUE------------------------
species(data)[1:2]
species(data)[1:2] <- c("new", "names")
sites(data)[1:2] <- c("newer", "names")
data <- data[, colSums(data$comm) > 5]

## ----tidy=TRUE------------------------
shape.output <- shape(data)
dim(coef(shape.output))
coef(shape.output)[1:3,1:3]

## ----tidy=TRUE------------------------
sqrt <- shape(data, sqrt.phy=TRUE)
traits <- shape(data, traitgram=1) #traits alone
traits <- shape(data, traitgram=c(0,0.5))#phylogeny and both
traits <- shape(data, ext.dist=as.dist(cophenetic(data$phy)))

## ----tidy=TRUE, fig.width=6, fig.height=4----
dist <- dissimilarity(data, "phylosor")
plot(hclust(dist$phylosor))

## ----tidy=TRUE------------------------
phy <- eco.phy.regression(data, permute=10)
trait <- eco.trait.regression(data, permute=10, method="quantile", tau=c(0.25,0.5,0.7))
trait <- eco.trait.regression(data, altogether=FALSE)

## ----warning=FALSE, tidy=TRUE---------
model <- fingerprint.regression(data, eco.permute=10)


## ----traitgram, warning=FALSE, fig.width=4, fig.height=4.5, dev.args=list(pointsize=8)----
assemblage <- c("Nerophilus", "Hydroptila", "Psorophora",
                "Simuliidae", "Psychodidae", "Ceratopogon",
                "Nectopsyche", "Pedomoecus", "Ceratopsyche")
dataAssemblage <- data[, species(data) %in% assemblage]
traitgram.cc(dataAssemblage, "length")

## ----FPDist---------------------------
fpd.data <- funct.phylo.dist(data, phyloWeight = 0.5, p = 2)

## ----sesFPD---------------------------
ses.mfpd.data <- picante::ses.mpd(data$comm, fpd.data)
head(ses.mfpd.data)[,c("ntaxa", "mpd.obs", "mpd.obs.p")]

