# This R script creates a scatter plot comparing the number of distinct CDR3 sequences at age 70 and at age 80. 

library(ggplot2)

df <- read.csv("/users/j29tien/ROP/results/IMMUNE/samples_cdr3.csv", header=TRUE)
rownames(df) <- NULL

spl <- split(df, rep(c(1, 2), length.out = nrow(df)))

age_70 <- spl$"1"
age_80 <- spl$"2"


## nIGH
igh70 <- age_70[,grep("nIGH", ignore.case=TRUE, colnames(age_70))]
igh80 <- age_80[,grep("nIGH", ignore.case=TRUE, colnames(age_80))]

#igh70 <- cbind(igh70, rowSums(igh70)) # no need to sum across columns, only one column of nIGH
#igh80 <- cbind(igh80, rowSums(igh80))

nIGH <- cbind(igh70, igh80)
rownames(nIGH) <- NULL
nIGH <- cbind(c(rep("nIGH", 65)), nIGH)
colnames(nIGH) <- c("Locus", "Age_70", "Age_80")
nIGH <- as.data.frame(nIGH)

nIGH$Age_70 <- as.numeric(as.character(nIGH$Age_70))
nIGH$Age_80 <- as.numeric(as.character(nIGH$Age_80))
nIGH$Locus <- as.factor(nIGH$Locus)

pvalue=wilcox.test(x = nIGH$Age_70, y = nIGH$Age_80, paired = T)$p.value

ggplot(nIGH, aes(x=Age_70, y=Age_80, shape=Locus, color=Locus)) + geom_point() + coord_cartesian(xlim =c(0, 300), ylim = c(0, 300)) + stat_function(fun = function(x) x, color="black") + geom_smooth(method="lm") + annotate(geom = "text", x = 200, y = 0, label=paste0("p-value=",format(x = pvalue, digits = 2, scientific = T)), size=5)
ggsave("/users/j29tien/imrep/results/nIGH_70-80_scatter.png")


## nIGK
igk70 <- age_70[,grep("nIGK", ignore.case=TRUE, colnames(age_70))]
igk80 <- age_80[,grep("nIGK", ignore.case=TRUE, colnames(age_80))]

nIGK <- cbind(igk70, igk80)
rownames(nIGK) <- NULL
nIGK <- cbind(c(rep("nIGK", 65)), nIGK)
colnames(nIGK) <- c("Locus", "Age_70", "Age_80")
nIGK <- as.data.frame(nIGK)

nIGK$Age_70 <- as.numeric(as.character(nIGK$Age_70))
nIGK$Age_80 <- as.numeric(as.character(nIGK$Age_80))
nIGK$Locus <- as.factor(nIGK$Locus)

pvalue=wilcox.test(x = nIGK$Age_70, y = nIGK$Age_80, paired = T)$p.value

ggplot(nIGK, aes(x=Age_70, y=Age_80, shape=Locus, color=Locus)) + geom_point() + coord_cartesian(xlim =c(0, 50), ylim = c(0, 50)) + stat_function(fun = function(x) x, color="black") + geom_smooth(method="lm") + annotate(geom = "text", x = 25, y = 0, label=paste0("p-value=",format(x = pvalue, digits = 2, scientific = T)), size=5)
ggsave("/users/j29tien/imrep/results/nIGK_70-80_scatter.png")


## nIGL
igl70 <- age_70[,grep("nIGL", ignore.case=TRUE, colnames(age_70))]
igl80 <- age_80[,grep("nIGL", ignore.case=TRUE, colnames(age_80))]

nIGL <- cbind(igl70, igl80)
rownames(nIGL) <- NULL
nIGL <- cbind(c(rep("nIGL", 65)), nIGL)
colnames(nIGL) <- c("Locus", "Age_70", "Age_80")
nIGL <- as.data.frame(nIGL)

nIGL$Age_70 <- as.numeric(as.character(nIGL$Age_70))
nIGL$Age_80 <- as.numeric(as.character(nIGL$Age_80))
nIGL$Locus <- as.factor(nIGL$Locus)

pvalue=wilcox.test(x = nIGL$Age_70, y = nIGL$Age_80, paired = T)$p.value

ggplot(nIGL, aes(x=Age_70, y=Age_80, shape=Locus, color=Locus)) + geom_point() + coord_cartesian(xlim =c(0, 50), ylim = c(0, 50)) + stat_function(fun = function(x) x, color="black") + geom_smooth(method="lm") + annotate(geom = "text", x = 25, y = 0, label=paste0("p-value=",format(x = pvalue, digits = 2, scientific = T)), size=5)
ggsave("/users/j29tien/imrep/results/nIGL_70-80_scatter.png")



## Combine and graph
cdr3s <- rbind(nIGH, nIGK, nIGL)

cdr3s$Age_70 <- as.numeric(as.character(cdr3s$Age_70))
cdr3s$Age_80 <- as.numeric(as.character(cdr3s$Age_80))
cdr3s$Locus <- as.factor(cdr3s$Locus)


# insert age_70$Sample and age_80$Sample as labels on the scatter 

ggplot(cdr3s, aes(x=Age_70, y=Age_80, shape=Locus, color=Locus)) + geom_point() + coord_cartesian(xlim =c(0, 100), ylim = c(0, 100)) + stat_function(fun = function(x) x, color="black")

ggsave("/users/j29tien/imrep/results/nCDR3_70-80_scatter.png")
 
