library(ggplot2)

# example of linear demand curve (first equation) 

demand = function(p, alpha = -40, beta = 500, sd = 10) {
  
  error = rnorm(length(p), sd = sd)
  q = p*alpha + beta + error
  
  return(q)
}

set.seed(100)

prices = seq(from = 5, to = 10, by = 0.1)
q = demand(prices)

data = data.frame('prices' = prices,'quantity' = q)

ggplot(data, aes(prices, quantity)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Demand Curve')

set.seed(10)

hist.prices = rnorm(252, mean = 6, sd = .5) # random prices defined by the company
hist.demand = demand(hist.prices) # demand curve defined above
hist.revenue = hist.prices*hist.demand # From the revenue equation
unity.cost = 4 # production cost per unity
hist.cost = unity.cost*hist.demand
hist.profit = (hist.prices - unity.cost)*hist.demand # From the price equation

data = data.frame('Period' = seq(1,252),'Daily.Prices' = hist.prices,
                  'Daily.Demand' = hist.demand, 'Daily.Revenue' = hist.revenue,
                  'Daily.Cost' = hist.cost, 'Daily.Profit' = hist.profit)

ggplot(data, aes(Period, Daily.Prices)) +
  geom_line(color = 4) +
  ggtitle('Historical Prices used for explotation')

ggplot(data, aes(Period, Daily.Revenue, colour = 'Revenue')) +
  geom_line() +
  geom_line(aes(Period, Daily.Profit, colour = 'Profit')) +
  geom_line(aes(Period, Daily.Cost, colour = 'Cost')) +
  labs(title = 'Historical Performance', colour = '')

library(stargazer)

model.fit = lm(hist.demand ~ hist.prices) # linear model for demand
stargazer(model.fit, type = 'html', header = FALSE) # output

# estimated parameters
beta = model.fit$coefficients[1]
alpha = model.fit$coefficients[2]  

p.revenue = -beta/(2*alpha) # estimated price for revenue
p.profit = (alpha*unity.cost - beta)/(2*alpha) # estimated price for profit

true.revenue = function(p) p*(-40*p + 500) # Revenue with true parameters (chunck demand)
true.profit = function(p) (p - unity.cost)*(-40*p + 500) # price with true parameters

# estimated curves
estimated.revenue = function(p) p*(model.fit$coefficients[2]*p + model.fit$coefficients[1])
estimated.profit = function(p) (p - unity.cost)*(model.fit$coefficients[2]*p + model.fit$coefficients[1])

opt.revenue = true.revenue(p.revenue) # Revenue with estimated optimum price
opt.profit = true.profit(p.profit) # Profit with estimated optimum price

# plot
df = data.frame(x1 = p.revenue, x2 = p.profit,
                y1 = opt.revenue, y2 = opt.profit, y3 = 0)

ggplot(data = data.frame(Price = 0)) +
  stat_function(fun = true.revenue, mapping = aes(x = Price, color = 'True Revenue')) +
  stat_function(fun = true.profit, mapping = aes(x = Price, color = 'True Profit')) +
  stat_function(fun = estimated.revenue, mapping = aes(x = Price, color = 'Estimated Revenue')) +
  stat_function(fun = estimated.profit, mapping = aes(x = Price, color = 'Estimated Profit')) +
  scale_x_continuous(limits = c(4, 11)) +
  labs(title = 'True curves without noise') +
  ylab('Results') +
  scale_color_manual(name = "", values = c("True Revenue" = 2, "True Profit" = 3, "Estimated Revenue" = 4, "Estimated Profit" = 6)) +
  geom_segment(aes(x = x1, y = y1, xend = x1, yend = y3), data = df) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = df)

