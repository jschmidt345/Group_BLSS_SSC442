### logistic regression ###
library(kernlab)
library(boot)
data('spam')
tibble::as_tibble(spam)

# response variable = type, spam or nonspam, nonspam = 0

is.factor(spam$type)
levels(spam$type)

set.seed(42)
spam_idx <- sample(nrow(spam), 1000)
spam_trn <- spam[spam_idx, ]
spam_tst <- spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal, 
                data = spam_trn, family=binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)


# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

