library(ggplot2)

df <- read.csv("/users/j29tien/ROP/results/IMMUNE/samples_cdr3.csv", header=TRUE)
rownames(df) <- NULL

spl <- split(df, rep(c(1, 2), length.out = nrow(df)))

age_70 <- spl$"1"
age_80 <- spl$"2"

## alphaIGH
igh70 <- age_70[,grep("alphaIGH", ignore.case=TRUE, colnames(age_70))]
igh80 <- age_80[,grep("alphaIGH", ignore.case=TRUE, colnames(age_80))]

alphaIGH <- cbind(igh70, igh80)
alphaIGH <- cbind(alphaIGH, igh80 - igh70)
alphaIGH <- cbind(c(rep("alphaIGH", 65)), alphaIGH)
colnames(alphaIGH) <- c("Locus", "Age_70", "Age_80", "Eighty_minus_Seventy")
alphaIGH <- as.data.frame(alphaIGH)

## alphaIGK
igk70 <- age_70[,grep("alphaIGK", ignore.case=TRUE, colnames(age_70))]
igk80 <- age_80[,grep("alphaIGK", ignore.case=TRUE, colnames(age_80))]

alphaIGK <- cbind(igk70, igk80)
alphaIGK <- cbind(alphaIGK, igk80 - igk70)
alphaIGK <- cbind(c(rep("alphaIGK", 65)), alphaIGK)
colnames(alphaIGK) <- c("Locus", "Age_70", "Age_80", "Eighty_minus_Seventy")
alphaIGK <- as.data.frame(alphaIGK)

## alphaIGL
igl70 <- age_70[,grep("alphaIGL", ignore.case=TRUE, colnames(age_70))]
igl80 <- age_80[,grep("alphaIGL", ignore.case=TRUE, colnames(age_80))]

alphaIGL <- cbind(igl70, igl80)
alphaIGL <- cbind(alphaIGL, igl80 - igl70)
alphaIGL <- cbind(c(rep("alphaIGL", 65)), alphaIGL)
colnames(alphaIGL) <- c("Locus", "Age_70", "Age_80", "Eighty_minus_Seventy")
alphaIGL <- as.data.frame(alphaIGL)

## Combine and graph
alphaD <- rbind(alphaIGH, alphaIGK, alphaIGL)
alphaD$Age_70 <- as.numeric(as.character(alphaD$Age_70))
alphaD$Age_80 <- as.numeric(as.character(alphaD$Age_80))
alphaD$Eighty_minus_Seventy <- as.numeric(as.character(alphaD$Eighty_minus_Seventy))
alphaD$Locus <- as.factor(alphaD$Locus)

ggplot(alphaD, aes(y=Eighty_minus_Seventy, fill=Locus)) + geom_boxplot()

ggsave("/users/j29tien/imrep/results/paired_alpha_boxplot.png")
