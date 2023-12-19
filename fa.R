raq_new<-read.csv("raq.csv", header = TRUE)
raq_new

str(raq_new)

raq_new[1:23] <- lapply(raq_new[1:23], as.numeric)


raqmatrix<-cor(raq_new)

cortest.bartlett(raq_new)

cortest.bartlett(raqmatrix, n=2571)

KMO(raq_new)

pc1<-principal(raq_new, nfactors = 23, rotate = "none")
plot(pc1$values, type = "b")

pc1_m <- principal(raqMatrix, nfactors = 23, rotate = "none")
plot(pc1_m$values, type = "b")

pc2 <- principal(raq_new, nfactors = 4, rotate = "none")
factor.model(pc2$loadings)
residuals<-factor.residuals(raqmatrix, pc2$loadings)



raq_fa <- factanal(raq, factors = 2)
raq_fa$uniquenesses
apply(raq_fa$loadings^2, 1, sum)
1-apply(raq_fa$loadings^2, 1, sum)
Lambda <- raq_fa$loadings
Psi <- diag(raq_fa$uniquenesses)
S <- raq_fa$correlation
Sigma <- Lambda %*% t(Lambda) + Psi
round(S - Sigma, 6)


#rotation
##orthogonal
pc3<-principal(raq_new, nfactors = 4, rotate = "varimax")
print.psych(pc3, cut = 0.3, sort = TRUE)

#oblique
pc4<- principal(raq_new, nfactors = 4, rotate = "oblimin")
print.psych(pc4, cut = 0.3, sort = TRUE)


pairs(pc3$loadings, col=1:ncol(raq_new), upper.panel=NULL, main="varimax")
par(xpd=TRUE) 
legend('topright', bty='n', pch='o', col=1:ncol(raq_new), attr(pc3$loadings, 'dimnames')[[1]], title="Variables")

pairs(pc4$loadings, col=1:ncol(raq_new), upper.panel=NULL, main="oblimin")
par(xpd=TRUE) 
legend('topright', bty='n', pch='o', col=1:ncol(raq_new), attr(pc4$loadings, 'dimnames')[[1]], title="Variables")


raq_fa_none <- factanal(raq, factors = 2, rotation = "none")
raq_fa_varimax <- factanal(raq, factors = 2, rotation = "varimax")
pc4 <- principal(raqmatrix, nfactors = 4, rotate = "oblimin")
print.psych(pc4, cut = 0.3, sort = TRUE)
pc4$loadings %*% pc4$Phi

f <- factanal(raq_new, factors=3)
scatterplot3d(as.data.frame(unclass(f$loadings)), main="3D factor loadings", color=1:ncol(raq), pch=20)


raq_fa_none <- factanal(raq_new, factors = 2, rotation = "none")
raq_fa_varimax <- factanal(raq_new, factors = 2, rotation = "varimax")
raq_fa_promax <- factanal(raq_new, factors = 2, rotation = "promax")

#par(mfrow = c(1, 3))

plot(raq_fa_none$loadings[, 1],
     raq_fa_none$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "No rotation")
abline(h = 0, v = 0)

plot(raq_fa_varimax$loadings[, 1],
     raq_fa_varimax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "varimax rotation")
abline(h = 0, v = 0)

plot(raq_fa_promax$loadings[, 1],
     raq_fa_promax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "promax rotation")
abline(h = 0, v = 0)

plot(raq_fa_oblimin$loadings[, 1],
     raq_fa_oblimin$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "oblimin rotation")
abline(h = 0, v = 0)

f <- factanal(raq, factors=4)
pairs(f$loadings, col=1:ncol(raq), upper.panel=NULL, main="Factor loadings")
par(xpd=TRUE) 
legend('topright', bty='n', pch='o', col=1:ncol(raq), attr(f$loadings, 'dimnames')[[1]], title="Variables")
