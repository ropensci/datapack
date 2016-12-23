# Logistic regression, also called a logit model, is used to model dichotomous outcome variables. 
# In the logit model the log odds of the outcome is modeled as a linear combination of the predictor variables.

library(aod)
library(ggplot2)
library(Rcpp)

# Example 1. Suppose that we are interested in the factors that influence whether a political candidate wins an election. The outcome (response) variable is binary (0/1); win or lose. The predictor variables of interest are the amount of money spent on the campaign, the amount of time spent campaigning negatively and whether or not the candidate is an incumbent.
# Example 2. A researcher is interested in how variables, such as GRE (Graduate Record Exam scores), GPA (grade point average) and prestige of the undergraduate institution, 
# effect admission into graduate school. The response variable, admit/don't admit, is a binary variable.
# Description of the data
# For our data analysis below, we are going to expand on Example 2 about getting into graduate school. 
# We have generated hypothetical data, which can be obtained from our website from within R. 
# Note that R requires forward slashes (/) not back slashes (\) when specifying a file location even if the file is on your hard drive.

mydata <- read.csv(system.file("./extdata/pkg-example/binary.csv"))

mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

# Plot the predicted probability for gre
ggplot(newdata3, aes(x = gre, y = PredictedProb)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = .2) +
    geom_line(aes(colour = rank), size=1)

# Write the plot out
ggsave(sprintf("%s/gre-predicted.png", tempdir()))
