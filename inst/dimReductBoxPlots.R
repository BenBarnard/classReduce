# Function to graph the parameter configuration

dimReductBoxPlots <- function(meltedData, yLower = 0, yUpper = 0.7){
  ggplot2::ggplot(data = meltedData,
                  aes(x = TargetDim, y = PMC, fill = variable)) +
    theme_bw() +
    ylab("Probability of Misclassification") +
    scale_y_continuous(limits = c(yLower, yUpper)) +
    xlab("Dimension") +
    ggtitle("Boxplots of Misclassification by Method for Target Dimension") +
    geom_boxplot() +
    stat_boxplot(geom ='errorbar') +
    geom_hline(aes(yintercept = Median), size = 1, alpha = .6, colour = "red")
}
