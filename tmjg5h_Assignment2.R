library(readxl)
library(dplyr)
library(ggplot2)

# Load data
unit3_file <- "Unit3_4_Data_All.xlsx"
mobile <- read_excel(unit3_file, sheet = "Mobile")

# ----------------------
# 1a. Relative frequency for Rural
# ----------------------

rural_freq <- table(mobile$Rural)
rural_rel_freq <- prop.table(rural_freq)

rural_freq
rural_rel_freq

# Proportion living in rural area
rural_rel_freq["1"]

# ----------------------
# 1b. Relative frequency for College
# ----------------------

college_freq <- table(mobile$College)
college_rel_freq <- prop.table(college_freq)

college_freq
college_rel_freq

# Proportion with college degree
college_rel_freq["1"]

# ----------------------
# 1c. Frequency distribution + histogram for Usage
# ----------------------

# Create intervals
mobile$Usage_Group <- cut(
  mobile$Usage,
  breaks = c(0, 60, 120, 180, 240, 300),
  right = FALSE
)

usage_freq <- table(mobile$Usage_Group)
usage_rel_freq <- prop.table(usage_freq)

usage_freq
usage_rel_freq

# Histogram
ggplot(mobile, aes(x = Usage)) +
  geom_histogram(
    breaks = c(0, 60, 120, 180, 240, 300),
    color = "black",
    fill = "skyblue"
  ) +
  labs(title = "Histogram of Mobile Usage",
       x = "Usage (minutes)",
       y = "Frequency")

# Proportion using more than 120 minutes
mean(mobile$Usage > 120)




car <- read_excel(unit3_file, sheet = "Car_Price")

# ----------------------
# 2a. Bubble Plot
# ----------------------

ggplot(car, aes(x = Age, y = Price, size = Mileage)) +
  geom_point(alpha = 0.6) +
  labs(title = "Bubble Plot: Age vs Price",
       x = "Age",
       y = "Price")

# ----------------------
# 2b. Create Mileage Category
# ----------------------

car <- car %>%
  mutate(Mileage_Category = ifelse(Mileage < 50000,
                                   "Low_Mileage",
                                   "High_Mileage"))

table(car$Mileage_Category)

# ----------------------
# 2c. Scatterplot with Categories
# ----------------------

ggplot(car, aes(x = Age, y = Price, color = Mileage_Category)) +
  geom_point(size = 3) +
  labs(title = "Price vs Age by Mileage Category",
       x = "Age",
       y = "Price")


#Question 4

# Risky Fund
mu1 <- 0.08
sd1 <- 0.14

# Less Risky Fund
mu2 <- 0.04
sd2 <- 0.05

# ----------------------
# 4a. Probability of Negative Return
# ----------------------

p_neg1 <- pnorm(0, mean = mu1, sd = sd1)
p_neg2 <- pnorm(0, mean = mu2, sd = sd2)

p_neg1
p_neg2

# ----------------------
# 4b. Probability of Return Above 8%
# ----------------------

p_above8_1 <- 1 - pnorm(0.08, mean = mu1, sd = sd1)
p_above8_2 <- 1 - pnorm(0.08, mean = mu2, sd = sd2)

p_above8_1
p_above8_2



#Question 5


p <- 0.23

# ----------------------
# 5a. n = 50
# ----------------------

n1 <- 50

# more than 20% → more than 10 employees
prob_50 <- 1 - pbinom(10, size = n1, prob = p)
prob_50

# ----------------------
# 5b. n = 200
# ----------------------

n2 <- 200

# more than 20% → more than 40 employees
prob_200 <- 1 - pbinom(40, size = n2, prob = p)
prob_200





#Question 6

startups <- read_excel("Unit4_Data_All.xlsx", sheet = "Startups")

# ----------------------
# 6a. CI for Research
# ----------------------

research_ci <- t.test(startups$Research, conf.level = 0.95)
research_ci

# ----------------------
# 6b. CI for Duration
# ----------------------

duration_ci <- t.test(startups$Duration, conf.level = 0.95)
duration_ci






#Question 7



wait_time <- read_excel("Unit4_Data_All.xlsx", sheet = "Wait_Time")

# ----------------------
# 7a & 7b Hypothesis Test
# H0: mu <= 5
# Ha: mu > 5
# ----------------------

t_test_result <- t.test(wait_time$Time,
                        mu = 5,
                        alternative = "greater",
                        conf.level = 0.90)

t_test_result
