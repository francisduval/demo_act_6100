#################################################################################################################################
### Description: Solutions de l'exercice 8)c) du chapitre 2 de « An Introduction to Statistical Learning »                    ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Février 2020                                                                                                        ###
#################################################################################################################################

library(ISLR)

# i) ----------------------------------------------------------------------------------------------------------------------------

summary(College)


# ii) ---------------------------------------------------------------------------------------------------------------------------
pairs(College[ , 1:10])

# iii) --------------------------------------------------------------------------------------------------------------------------
plot(College$Private, College$Outstate, xlab = "Private?", ylab = "Outstate")

# iv) ---------------------------------------------------------------------------------------------------------------------------
Elite <- rep("No", nrow(College))
Elite[College$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
College <- data.frame(College, Elite)

summary(College$Elite)
plot(College$Elite, College$Outstate, xlab = "Elite?", ylab = "Outstate")

# v) ----------------------------------------------------------------------------------------------------------------------------
str(College)

par(mfrow = c(2, 2))
hist(College$Apps, breaks = 10, main = "10 intervalles")
hist(College$Apps, breaks = 20, main = "20 intervalles")
hist(College$Apps, breaks = 50, main = "50 intervalles")
hist(College$Apps, breaks = 100, main = "100 intervalles")

hist(College$Top10perc, breaks = 10, main = "10 intervalles")
hist(College$Top10perc, breaks = 20, main = "20 intervalles")
hist(College$Top10perc, breaks = 50, main = "50 intervalles")
hist(College$Top10perc, breaks = 100, main = "100 intervalles")
