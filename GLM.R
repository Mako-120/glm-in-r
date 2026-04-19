library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(MASS)


load('brevhins-B.RData')
ds <- ds[ds$ExposTotal > 0 & ds$Gender %in% c("Female", "Male"), ]
ds <- ds[!is.na(ds$DrivAge), ]
set.seed(2137) 
train_indices <- sample(seq_len(nrow(ds)), size = 0.8 * nrow(ds))


train_data <- ds[train_indices, ]
test_data  <- ds[-train_indices, ]

mod1_train <- glm(
  ClaimNb ~ Gender + DrivAge + VehYear + Area + offset(log(ExposTotal)),
  family = poisson(link = "log"),            
  data = train_data
)

avg_claim <- mean(train_data$ClaimAmount[train_data$ClaimAmount > 0])

modSev1_train <- glm( 
  ClaimAmount ~ Gender + DrivAge + VehYear + Area,
  family = inverse.gaussian(link = "log"),
  data = subset(train_data, ClaimAmount > 0),
  mustart = rep(avg_claim, sum(train_data$ClaimAmount > 0))
)

summary(modSev1_train)

test_data$PredClaimNb <- predict(mod1_train, newdata = test_data, type = "response")

#Przewidujemy wysokość szkody NA JEDNĄ SZKODĘ dlatego 1
test_data_for_sev <- test_data
test_data_for_sev$ClaimNb <- 1 

test_data$PredAvgClaimAmt <- predict(modSev1_train, newdata = test_data_for_sev, type = "response")


calibration_data <- test_data %>%
  filter(ClaimAmount > 0) %>%
  mutate(
    RiskGroup = ntile(PredAvgClaimAmt, 15)
  ) %>%
  group_by(RiskGroup) %>%
  summarise(
    Avg_Predicted = mean(PredAvgClaimAmt), 
    Avg_Actual = mean(ClaimAmount),        
    Num_Claims = n()                    
  )


ggplot(calibration_data, aes(x = Avg_Predicted, y = Avg_Actual)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(size = Num_Claims), color = "darkblue", alpha = 0.7) +
  geom_line(color = "blue", alpha = 0.3) +
  labs(
    title = "Wysokość szkody",
    x = "Przewidywana Średnia Szkoda",
    y = "Rzeczywista Średnia Szkoda",
    size = "Liczba szkód"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)

calibration_data_freq <- test_data %>%
  mutate(
    RiskGroup = ntile(PredClaimNb, 4)
  ) %>%
  group_by(RiskGroup) %>%
  summarise(
    Avg_Predicted = mean(PredClaimNb),
    Avg_Actual = mean(ClaimNb),       
    Num_Policies = n()                 
  )

ggplot(calibration_data_freq, aes(x = Avg_Predicted, y = Avg_Actual)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(size = Num_Policies), color = "darkgreen", alpha = 0.7) +
  geom_line(color = "darkgreen", alpha = 0.3,size = 0.7) +
  labs(
    title = "Częstość szkody",
    x = "Przewidywana średnia liczba szkód",
    y = "Rzeczywista średnia liczba szkód",
    size = "Liczba polis"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)


sev_lift_data <- test_data %>%
  filter(ClaimAmount > 0) %>% 
  arrange(PredAvgClaimAmt) %>% 
  mutate(Decile = ntile(PredAvgClaimAmt, 15)) %>% 
  group_by(Decile) %>%
  summarise(
    Avg_Predicted_Sev = mean(PredAvgClaimAmt),
    Avg_Actual_Sev = mean(ClaimAmount),
    Claim_Count = n()
  )

ggplot(sev_lift_data, aes(x = factor(Decile))) +
  geom_bar(aes(y = Avg_Actual_Sev), stat = "identity", fill = "lightblue", alpha = 0.7) +
  geom_point(aes(y = Avg_Predicted_Sev), color = "darkblue", size = 3) +
  geom_line(aes(y = Avg_Predicted_Sev, group = 1), color = "darkblue", size = 1.2) +
  labs(
    title = "Wysokość szkody",
    x = "Grupy ryzyka",
    y = "Średnia wysokość szkody"
  ) +
  theme_minimal()

freq_lift_data <- test_data %>%
  mutate(Decile = ntile(PredClaimNb, 15)) %>% 
  group_by(Decile) %>%
  summarise(
    Avg_Predicted = mean(PredClaimNb),
    Avg_Actual = mean(ClaimNb),
    Exposure = sum(ExposTotal)
  )

ggplot(freq_lift_data, aes(x = factor(Decile))) +
  geom_bar(aes(y = Avg_Actual), stat = "identity", fill = "gray70", alpha = 0.6) +
  geom_point(aes(y = Avg_Predicted), color = "darkgreen", size = 1) +
  geom_line(aes(y = Avg_Predicted, group = 1), color = "darkgreen", size = 0.5) +
  labs(
    title = "Częstotliwość szkody",
    x = "Grupy ryzyka",
    y = "Średnia liczba szkód"
  ) +
  theme_minimal()