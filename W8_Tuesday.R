

# EXAMPLE: Indirect Causation relationship
# fire ---> smoke ---> alarm

fire <- # fire_data
  sample(
    c(0,1),
    size = 10000,
    replace = T # must allow replacement, picks 0 once, 1 once, then breaks otherwise
  )

smoke <- ifelse(
  # (1) condition for true
  fire == 1,
  # (2) the thing you do if
  sample(
    c(0,1),
    size = 10000,
    replace = T,
    prob = c(0.05, 0.95) # map to the 0 and 1 
  ),
  # (3) the thing you do else 
  sample(
    c(0,1),
    size = 10000,
    replace = T,
    prob = c(0.8, 0.2) # map to the 0 and 1 
  )
)
  

alarm <- ifelse(
  # (1) condition for true
  smoke == 1,
  # (2) the thing you do if
  sample(
    c(0,1),
    size = 10000,
    replace = T,
    prob = c(0.01, 0.99) # map to the 0 and 1 
  ),
  # (3) the thing you do else 
  sample(
    c(0,1),
    size = 10000,
    replace = T,
    prob = c(0.99, 0.01) # map to the 0 and 1 
  )
)


data <- data.frame(
  fire = fire, 
  # fire_name = fire_data
  smoke = smoke,
  alarm = alarm
)

# just fire
model <- glm(
  alarm ~ fire,
  data = data,
  family = quasibinomial()
)

summary(model)

exp(model$coefficients[2]) / (exp(model$coefficients[2]) + 1)

# just smoke
model <- glm(
  alarm ~ smoke,
  data = data,
  family = quasibinomial()
)

summary(model)

exp(model$coefficients[2]) / (exp(model$coefficients[2]) + 1)


# fire -> alarm controlling for smoke 
model <- glm(
  alarm ~ fire + smoke,
  data = data,
  family = quasibinomial()
)

summary(model)

exp(model$coefficients[2]) / (exp(model$coefficients[2]) + 1)



#################################################################

# EXAMPLE 2: Confounding Variable
# shoe size <--- age ---> reading level

age <- sample(
  5:10,
  size = 10000,
  replace = T,
  # no probability, so it will be uniform distribution
)

shoe <- age + rnorm(10000) # n = 10000, mean = 0, std dev = 1 (by default)

reading <- age*2 -5 + rnorm(10000)*1.5
  

data <- data.frame(
  age = age, 
  shoe = shoe,
  reading = reading
)

# linear model
model <- lm(
  reading ~ shoe, 
  data = data
)

summary(model)

model <- lm(
  reading ~ shoe + age, 
  data = data
)


## EXAMPLE 3: COLLIDER -- example of when to NOT control for a 3rd variable
#  beauty --> celebrity <--- talent
# (rnorm)                   (rnorm)


## Challenges of RCT (random control trials) in real world 
# treatment vs. control group, where its unfair for a control group who doesn't get the benefits of 
# Stepped wedge RCT! --> incrementally increase the number of people in the treatment group 

