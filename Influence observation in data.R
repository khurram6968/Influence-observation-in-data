mydata = read.csv("C:\\Users\\MKhurram\\Desktop\\stats.csv")
model = lm(mydata$Scoring.Avg.~mydata$Greens.in.Reg.+mydata$Putting.Avg.)
library(ggplot2)
# find influence observa
df <- model$df.residual
p <- length(model$coefficients)
n <- nrow(model$model)
dffits_crit = 2 * sqrt((p + 1) / (n - p - 1))
influence3_dffits <- dffits(model)
df <- data.frame(obs = names(influence3_dffits),
                 dffits = influence3_dffits)
ggplot(df, aes(y = dffits, x = obs)) +
  geom_point() +
  geom_hline(yintercept = c(dffits_crit, -dffits_crit), linetype="dashed",) +
  labs(title = "DFFITS",
       subtitle = "Influential Observation",
       x = "Observation Number",
       y = "DFFITS")+theme(axis.text.x = element_text(angle = 60))

