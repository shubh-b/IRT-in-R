# Read file from working directory
fileName <- readline()

packages = c("readxl", "ggplot2", "gridExtra")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

input <- paste(fileName,".xlsx", sep = "")
myfile0 <- suppressMessages(read_excel(input, col_names = F))
myfile00 <- myfile0[-c(1, 2, 4), 14 : 43]
colnames(myfile00) <- myfile00[1, ]
myfile00 <- myfile00[-1, ]
myfile00[] <- lapply(myfile00, gsub, pattern ='X', replacement = '0')
resp.sheet <- as.data.frame(lapply(myfile00, as.numeric))

 
Total <- apply(resp.sheet, 1, sum)
resp.sheet <- cbind(resp.sheet, Total)
resp.asc <- resp.sheet[order(Total, decreasing = T), ]

scores <- sort(unique(Total))
ablty <- paste('theta.', scores, sep = '')
scaled.ablty <- (scores - mean(scores))*(3/(max(scores - mean(scores))))

scores.sect <- list(); prob <- list()
for(i in 1:length(scores)){
  scores.sect[[i]] <- resp.asc[resp.asc$Total == scores[i], -c(which(colnames(resp.asc) == 'Total'))]
  prob[[i]] <- colSums(scores.sect[[i]])/nrow(scores.sect[[i]])
}
prob.distr <- cbind(do.call('rbind', prob), scaled.ablty)
row.names(prob.distr) <- ablty

# Sigmoid curve for Birnbaum's 2-Parameter model
sigmoid <- function(params, x) {
  1 / (1 + exp(-params[1] * (x - params[2])))
}

x <- prob.distr[, 'scaled.ablty']
y <- prob.distr[, 'Q2319520']

# Fit the sigmoid curve
fitmodel <- nls(y ~ 1 /(1 + exp(-a * (x - b))), start = list(a = 1, b = 0))
params <- coef(fitmodel)
y2 <- sigmoid(params, x)

# ICC and IIC
plot1 <- ggplot(data = data.frame(x, y), aes(x = x, y = y)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = data.frame(x, y2), aes(x = x, y = y2)) + theme_bw() +
  expand_limits(y = c(0.0, 1.0)) + xlab('Ability') + ylab('Probability')

plot2 <- ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_line(color = 'red', data = data.frame(x, y2*(1 - y2)), aes(x = x, y = y2*(1 - y2))) + theme_bw() +
  expand_limits(y = c(0.0, 1.0)) + xlab('Scaled Score') + ylab('Information')

grid.arrange(plot1, plot2, ncol=2)
