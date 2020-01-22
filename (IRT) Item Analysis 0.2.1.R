packages = c("readxl", "ggplot2", "gridExtra", "shiny")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

## Read file
myfile0 <- suppressMessages(read_excel("FilePath/FileName.xlsx", col_names = F))
myfile00 <- myfile0[-c(1, 2, 4), 14 : 43]
colnames(myfile00) <- myfile00[1, ]
myfile00 <- myfile00[-1, ]
myfile00[] <- lapply(myfile00, gsub, pattern ='X', replacement = '0')

## Converting into Dichotomous (0-1) response sheet.
resp.sheet <- as.data.frame(lapply(myfile00, as.numeric))

## Finding theta (grouped-ability) on the basis of scaled scores of the candidates.
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



## Shiny Application
df <- as.data.frame(prob.distr)

ui <- fluidPage(
  titlePanel('Item Characteristic Curve and Item Information Curve'),
  sidebarLayout(
    sidebarPanel(
      selectInput('ycol', 'Item ID', colnames(df[-length(df)]))
    ),
    mainPanel(
      plotOutput('PLOT')
    )
  )
)

server <- function(input, output) {
  qid <- reactive({df[, input$ycol]})
  selectedData <- reactive({
    data.frame(qid(), sc.ablty = df$scaled.ablty)
  })
  
  # Sigmoid Curve for Birnbaum's 2-Parameter model
  sigmoid <- function(params, x) {
    1 / (1 + exp(-params[1] * (x - params[2])))
  }
  
  # Fit the sigmoid curve
  fitmodel <- reactive({nls(qid() ~ 1/(1 + exp(-a * (df$scaled.ablty - b))), start = list(a = 1, b = 0))})
  params <- reactive({coef(fitmodel())})
  y2 <- reactive({sigmoid(params(), df$scaled.ablty)})
  
  # Item Characteristic Curve
  plot1 <- reactive({
    ggplot(selectedData(), aes(x = selectedData()$sc.ablty, y = selectedData()$qid)) + ggtitle('ICC') +
      geom_point(color = 'blue') + theme_bw() + xlab('Ability') + ylab('Probability') + expand_limits(y = c(0.0, 1.0)) +
      geom_line(color = 'red', data = data.frame(selectedData()$sc.ablty, y2()), aes(x = selectedData()$sc.ablty, y = y2()))
  })
  
  # Item Information Curve
  plot2 <- reactive({
    ggplot(selectedData(), aes(x = selectedData()$sc.ablty, y = selectedData()$qid)) + ggtitle('IIC') +
      geom_line(color = 'red', data = data.frame(selectedData()$sc.ablty, (params()[1]^2)*y2()*(1 - y2())), aes(x = selectedData()$sc.ablty, y = (params()[1]^2)*y2()*(1 - y2()))) + 
      theme_bw() + expand_limits(y = c(0.0, 1.0)) + xlab('Scaled Score') + ylab('Information')
  })
  
  output$PLOT <- renderPlot({
    
    grid.arrange(plot1(), plot2(), ncol = 2)
    
  })
}

shinyApp(ui, server)
