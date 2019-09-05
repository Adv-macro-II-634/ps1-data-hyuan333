code for data analysis here
library(readxl)
SCFP2007 <- read_excel("C:/Users/13127/Desktop/SCFP2007.xlsx")
View(SCFP2007)

y<-getDataPart(SCFP2007)

income<-quantile(y[[1]]/5, probs = c(0, 0.01,0.05,0.1, 0.2,0.4,0.6,0.8,0.9,0.95,0.99), na.rm = FALSE,  names = TRUE)
earnings<-quantile(y[[2]]/5, probs = c(0,0.01,0.05,0.1, 0.2,0.4,0.6,0.8,0.9,0.95,0.99), na.rm = FALSE,  names = TRUE)
wealth<-quantile(y[[3]]/5, probs = c(0, 0.01,0.05,0.1, 0.2,0.4,0.6,0.8,0.9,0.95,0.99), na.rm = FALSE,  names = TRUE)

library(ineq)

g1<-Gini(income)
g2<-Gini(earnings)
g3<-Gini(wealth)


plot(Lc(income),main = "Lorenz Curve(income)")
plot(Lc(earnings),main = "Lorenz Curve(earnings)")
plot(Lc(wealth),main = "Lorenz Curve(wealth)")

cat("Gini index of income:",g1)
cat("Gini index of earnings:",g2)
cat("Gini index of wealth:",g3)


table1 <- matrix(c(income,earnings,wealth),ncol = 11,byrow = TRUE)
colnames(table1) <- c(0,1,5,10,20,40,60,80,90,95,99)
rownames(table1) <- c("income","earnings","wealth")
 table2 <- as.table(table1)
print(table1)



library(gridExtra)
pdf("table1.pdf", height=8, width=16)
grid.table(table1)
dev.off()

