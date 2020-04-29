### logistic regression ###
library(kernlab)
library(boot)
library(caret)
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
#mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
#mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
#mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
#mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)


#set.seed(2)
#a = cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
#b = cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
#c = cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
#d = cv.glm(spam_trn, fit_over, K = 100)$delta[1]

### Exercise 1 ###
#1.
'
1. model a
2. model b
3. model d
4. model c
'
#2. 
'After re-running with k=100, the results are the same'

#3
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")

spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))

table(spam_tst$type) / nrow(spam_tst)




