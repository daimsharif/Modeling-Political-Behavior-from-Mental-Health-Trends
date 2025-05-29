# R code to generate central figure
install.packages("tidyverse")
library(tidyverse)
library(lme4)
library(corrplot)
install.packages("ggeffects")
library(sjPlot)    # For plotting model results
library(marginaleffects) # For extracting marginal effects
library(ggeffects) # For plotting predictions
library(GGally)
library(car)       # For VIF and other diagnostics
library(splines)   # For non-linear effects

setwd("C:/Users/daims/Desktop/ASM/final proj")
# Load data
df <- read_csv("USvotes.csv")

summary(df)
str(df)


df$STNAME=factor(df$STNAME)
df$CTYNAME=factor(df$CTYNAME)
str(df)

sum(is.na(df))


#visuals
# Create the ggpairs plot
ggpairs(df[-c(1,2,4,5)],
        diag = list(continuous = "density"), # Show density plots on the diagonal
        lower = list(continuous ="points"),                    # Scatter plots in the lower panels
        upper = list(continuous = "cor"), # 'upper' is now a list
        title = "Correlation Scatter Plot Matrix with Distributions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))



# Scatterplot of depression vs Republican votes
ggplot(df, aes(x = Crude.Prevalence.Estimate, y = per_gop)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Depression Estimates vs. Republican Vote Percentage",
       x = "Depression Prevalence (%)", y = "Republican Vote (%)")

# Examine the relationship across different states (boxplot)
ggplot(df, aes(x = reorder(STNAME, per_gop, FUN = median), y = per_gop)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Republican Vote % by State", x = "State", y = "Republican Vote %")

str(df)

# Create useful transformations and variables
df <- df %>%
  mutate(
    log_pop = scale(log(TOT_POP)),              # Log transform population 
    gender_ratio = scale(TOT_MALE/TOT_FEMALE),  # Gender ratio
    logit_gop = log((per_gop/100)/(1-(per_gop/100))), # Logit transform of GOP percentage
    gop_majority = ifelse(per_gop > 50, 1, 0), # Binary outcome for majority
    depression_centered = scale(Crude.Prevalence.Estimate), # Center depression
    race_sc=scale(race)
  )
str(df)
df$gop_majority=factor(df$gop_majority) 


#models

# 1. Weighted Linear Regression (weighted by population)
model_weighted <- lm(per_gop ~ Crude.Prevalence.Estimate + race + gender_ratio + log_pop, 
                     data = df, weights = sqrt(TOT_POP))
summary(model_weighted)

model_weighted <- lm(per_gop ~ depression_centered+ race_sc + gender_ratio + scale(log(total_votes)) , 
                     data = df, weights = sqrt(TOT_POP))
summary(model_weighted)

# 2. Model with non-linear terms and interaction effects
# model_complex <- lm(per_gop ~ Crude.Prevalence.Estimate * race + 
#                       I(Crude.Prevalence.Estimate^2) + 
#                       ns(gender_ratio, df = 3) + 
#                       log_pop, 
#                     data = df, weights = TOT_POP)
# summary(model_complex)

model_complex <- lm(per_gop ~  
                      I(depression_centered^2) + 
                      ns(gender_ratio, df = 3) + 
                      scale(log(total_votes)), 
                    data = df, weights = sqrt(TOT_POP))
summary(model_complex)

# 3. Mixed-effects model with random intercepts for states
model_ri <- lmer(log(per_gop) ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+ 
                   (1|STNAME), data = df, weights = sqrt(TOT_POP))
summary(model_ri)
plot(model_ri)
r2(model_ri)

model_ri_dep <- lmer(log(per_gop) ~ depression_centered  +
                   (1|STNAME), data = df, weights = sqrt(TOT_POP))
summary(model_ri_dep)


# 4. Mixed-effects model with random slopes for depression effect by state
model_rs_dep <- lmer(log(per_gop) ~ depression_centered+
                   (1 + depression_centered|STNAME), data = df, weights = sqrt(TOT_POP))
summary(model_rs_dep)

model_rs <- lmer(log(per_gop) ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+
                   (1 + depression_centered|STNAME), data = df, weights = sqrt(TOT_POP))
summary(model_rs)
plot(model_rs)

# 5. Log-odds (logit) transformation model (treating per_gop as a proportion)
model_logit <- lm(logit_gop ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+STNAME, 
                  data = df, weights = sqrt(TOT_POP))
summary(model_logit)
#6 non-linear rs
library(splines)
model_nonlin <- lmer(
  log(per_gop) ~ ns(depression_centered, df = 3) + race_sc + gender_ratio + scale(log(total_votes)) +
    (1 + depression_centered | STNAME),
  data = df,
  weights = sqrt(TOT_POP)
)
summary(model_nonlin)
plot(model_nonlin)
anova(model_rs,model_nonlin)

#7 interact rs
model_interact <- lmer(
  log(per_gop) ~ depression_centered * race_sc + gender_ratio + scale(log(total_votes)) +
    (1 + depression_centered | STNAME),
  data = df,
  weights = sqrt(TOT_POP)
)
summary(model_interact)
plot(model_interact)
anova(model_rs,model_interact)



#################model stregth
# Compare fixed-effects models
anova(model_weighted, model_complex)

# Compare mixed-effects models
anova(model_ri_dep,model_ri)
anova(model_rs_dep,model_rs)
anova(model_rs,model_ri)

r2(model_rs)
# Compare with AIC/BIC
AIC(model_weighted, model_complex)
BIC(model_weighted, model_complex)
plot(model_rs)

# Check for collinearity
vif(model_complex)


# # Check residual patterns across states
# df$residuals_weighted <- residuals(model_weighted)
# ggplot(df, aes(x = STNAME, y = residuals_weighted)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(title = "Residuals by State (Weighted Model)", 
#        x = "State", y = "Residuals")
#############extract insights 


library(ggplot2)
library(dplyr)

# Extract state-specific slopes for depression effect
ranef_df <- as.data.frame(ranef(model_rs)$STNAME)
ranef_df$STNAME <- rownames(ranef(model_rs)$STNAME)  # safer
names(ranef_df)[names(ranef_df) == "depression_centered"] <- "Depression_Effect"

# Add a column for positive/negative effect
ranef_df <- ranef_df %>%
  mutate(effect_direction = ifelse(Depression_Effect >= 0, "Positive", "Negative"))

# Plot
ggplot(ranef_df, aes(x = reorder(STNAME, Depression_Effect), 
                     y = Depression_Effect, 
                     fill = effect_direction)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
  coord_flip() +
  labs(title = "Variation in Depression Effect by State",
       subtitle = "Positive values: stronger association between depression and Republican voting",
       x = "State", 
       y = "State-specific Deviation in Depression Effect (log scale)") +
  theme_minimal(base_size = 10)


#########################################################3
# Extract fixed effects and random effects
fixefs <- fixef(model_rs)
ranefs <- ranef(model_rs)$STNAME %>%
  rownames_to_column(var = "STNAME") %>%
  rename(intercept_offset = `(Intercept)`, slope_offset = depression_centered)

# Build per-state coefficients
coef_df <- ranefs %>%
  mutate(
    intercept = fixefs["(Intercept)"] + intercept_offset,
    slope = fixefs["depression_centered"] + slope_offset
  )

library(dplyr)
library(tidyr)

# Assuming coef_df has columns: STNAME, intercept, slope
line_df <- coef_df %>%
  rowwise() %>%
  mutate(
    data = list(tibble(
      x = seq(min(df$depression_centered), max(df$depression_centered), length.out = 50),
      y = exp(intercept + slope * x)
    ))
  ) %>%
  unnest(data)

# Plot
library(ggplot2)
ggplot(df, aes(x = depression_centered, y = per_gop)) +
  geom_point(aes(size = TOT_POP, color = race), alpha = 0.6) +
  geom_line(data = line_df, aes(x = x, y = y), color = "black", size = 0.7) +
  facet_wrap(~ STNAME, scales = "free", ncol = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(1, 8), breaks = c(10000, 100000, 1000000)) +
  labs(title = "Depression Rates vs. Republican Voting by State",
       subtitle = "Mixed-effects random slopes per state; point size = population, color = % white",
       x = "Depression Prevalence (%)", 
       y = "Republican Vote (%)",
       color = "White %",
       size = "Population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 8))
# ###########################################################################for ri
# # Get fixed effect (common slope)
# fixefs <- fixef(model_ri_dep)
# 
# # Random intercepts by state
# ranefs <- ranef(model_ri_dep)$STNAME %>%
#   rownames_to_column(var = "STNAME") %>%
#   rename(intercept_offset = `(Intercept)`)  # only intercept, no slope now
# 
# # Build per-state intercepts (constant slope everywhere)
# coef_df <- ranefs %>%
#   mutate(
#     intercept = fixefs["(Intercept)"] + intercept_offset,
#     slope = fixefs["depression_centered"]  # same slope for all states
#   )
# # Create line data per state
# line_df <- coef_df %>%
#   rowwise() %>%
#   mutate(
#     data = list(tibble(
#       x = seq(min(df$depression_centered), max(df$depression_centered), length.out = 50),
#       y = exp(intercept + slope * x)  # same slope, different intercept per state
#     ))
#   ) %>%
#   unnest(data)
# # Plot
# ggplot(df, aes(x = depression_centered, y = per_gop)) +
#   geom_point(aes(size = TOT_POP, color = race), alpha = 0.6) +
#   geom_line(data = line_df, aes(x = x, y = y), color = "black", size = 0.7) +
#   facet_wrap(~ STNAME, scales = "free", ncol = 5) +
#   scale_color_gradient(low = "blue", high = "red") +
#   scale_size_continuous(range = c(1, 8), breaks = c(10000, 100000, 1000000)) +
#   labs(title = "Depression Rates vs. Republican Voting by State",
#        subtitle = "Random intercepts per state; fixed slope",
#        x = "Depression Prevalence (%)", 
#        y = "Republican Vote (%)",
#        color = "White %",
#        size = "Population") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         strip.text = element_text(size = 8))
# 
# sum(is.na(df))
###########################################################################
# Create a compelling visualization that shows the relationship
# accounting for state differences and population size
# ggplot(df, aes(x = Crude.Prevalence.Estimate, y = per_gop)) +
#   geom_point(aes(size = TOT_POP, color = race), alpha = 0.5) +
#   geom_smooth(method = "lm", color = "black", se = TRUE) +
#   facet_wrap(~ STNAME, scales = "free", ncol = 5) +
#   scale_color_gradient(low = "blue", high = "red") +
#   scale_size_continuous(range = c(1, 8), breaks = c(10000, 100000, 1000000)) +
#   labs(title = "Depression Rates vs. Republican Voting by State",
#        subtitle = "Point size indicates county population, color shows racial composition",
#        x = "Depression Prevalence (%)", 
#        y = "Republican Vote (%)",
#        color = "White %",
#        size = "Population") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         strip.text = element_text(size = 8))
# 
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# # 1. Get residual standard deviation from the model (on log scale)
# resid_sd <- sigma(model_rs)  # For example ~0.16 if you check your summary(model_rs)
# 
# # 2. Build prediction dataframe
# x_seq <- seq(min(df$depression_centered), max(df$depression_centered), length.out = 50)
# 
# line_df <- coef_df %>%
#   rowwise() %>%
#   mutate(
#     data = list(tibble(
#       x = x_seq,
#       log_y = intercept + slope * x_seq,
#       log_ymin = (intercept + slope * x_seq) - 1.96 * resid_sd,
#       log_ymax = (intercept + slope * x_seq) + 1.96 * resid_sd,
#       y = exp(log_y),        # back-transform to per_gop
#       ymin = exp(log_ymin),
#       ymax = exp(log_ymax)
#     ))
#   ) %>%
#   unnest(data)
# 
# # 3. Plot with ribbon
# ggplot(df, aes(x = depression_centered, y = per_gop)) +
#   geom_point(aes(size = TOT_POP, color = race), alpha = 0.6) +
#   geom_ribbon(data = line_df, aes(x = x, ymin = ymin, ymax = ymax), fill = "grey70", alpha = 0.4) +
#   geom_line(data = line_df, aes(x = x, y = y), color = "black", size = 0.7) +
#   facet_wrap(~ STNAME, scales = "free", ncol = 5) +
#   scale_color_gradient(low = "blue", high = "red") +
#   scale_size_continuous(range = c(1, 8), breaks = c(10000, 100000, 1000000)) +
#   labs(title = "Depression Rates vs. Republican Voting by State",
#        subtitle = "Mixed-effects random slopes per state; log-transformed modeling with 95% CI shading",
#        x = "Depression Prevalence (Centered)", 
#        y = "Republican Vote (%)",
#        color = "White %",
#        size = "Population") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         strip.text = element_text(size = 8))


###############3 USING LOGIT


# 3. Mixed-effects model with random intercepts for states
lmodel_ri <- lmer(logit_gop ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+ 
                   (1|STNAME), data = df, weights = sqrt(TOT_POP))
summary(lmodel_ri)
plot(lmodel_ri)
r2(lmodel_ri)

lmodel_ri_dep <- lmer(logit_gop ~ depression_centered  +
                       (1|STNAME), data = df, weights = sqrt(TOT_POP))
summary(lmodel_ri_dep)


# 4. Mixed-effects model with random slopes for depression effect by state
lmodel_rs_dep <- lmer(logit_gop ~ depression_centered+
                       (1 + depression_centered|STNAME), data = df, weights = sqrt(TOT_POP))
summary(lmodel_rs_dep)

lmodel_rs <- lmer(logit_gop ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+
                   (1 + depression_centered|STNAME), data = df, weights = sqrt(TOT_POP))
summary(lmodel_rs)
plot(lmodel_rs)

#compare
anova(lmodel_ri,lmodel_ri_dep)
anova(lmodel_rs,lmodel_rs_dep)

#############################################################################################3
# Extract state-specific slopes for depression effect
ranef_df <- as.data.frame(ranef(lmodel_rs)$STNAME)
ranef_df$STNAME <- rownames(ranef(lmodel_rs)$STNAME)  # safer
names(ranef_df)[names(ranef_df) == "depression_centered"] <- "Depression_Effect"

# Add a column for positive/negative effect
ranef_df <- ranef_df %>%
  mutate(effect_direction = ifelse(Depression_Effect >= 0, "Positive", "Negative"))

# Plot
ggplot(ranef_df, aes(x = reorder(STNAME, Depression_Effect), 
                     y = Depression_Effect, 
                     fill = effect_direction)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
  coord_flip() +
  labs(title = "Variation in Depression Effect by State",
       subtitle = "Positive values: stronger association between depression and Republican voting",
       x = "State", 
       y = "State-specific Deviation in Depression Effect (log scale)") +
  theme_minimal(base_size = 10)



library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Extract fixed and random effects
fixefs <- fixef(lmodel_rs)
ranefs <- ranef(lmodel_rs)$STNAME %>%
  tibble::rownames_to_column(var = "STNAME") %>%
  rename(intercept_offset = `(Intercept)`, slope_offset = depression_centered)

# 2. Build per-state coefficients
coef_df <- ranefs %>%
  mutate(
    intercept = fixefs["(Intercept)"] + intercept_offset,
    slope = fixefs["depression_centered"] + slope_offset
  )

# 3. Define inverse logit function
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

# 4. Residual standard deviation (on logit scale)
resid_sd <- sigma(lmodel_rs)

# 5. Build prediction dataframe
x_seq <- seq(min(df$depression_centered), max(df$depression_centered), length.out = 50)

line_df <- coef_df %>%
  rowwise() %>%
  mutate(
    data = list(tibble(
      x = x_seq,
      logit_y = intercept + slope * x_seq,
      logit_ymin = logit_y - 1.96 * resid_sd,
      logit_ymax = logit_y + 1.96 * resid_sd,
      y = inv_logit(logit_y) * 100,         # convert back to % Republican vote
      ymin = inv_logit(logit_ymin) * 100,
      ymax = inv_logit(logit_ymax) * 100
    ))
  ) %>%
  unnest(data)

# 6. Final Plot
ggplot(df, aes(x = depression_centered, y = per_gop)) +
  geom_point(aes(size = TOT_POP, color = race), alpha = 0.6) +
  # geom_ribbon(data = line_df, aes(x = x, ymin = ymin, ymax = ymax), fill = "grey70", alpha = 0.4) +
  geom_line(data = line_df, aes(x = x, y = y), color = "black", size = 0.8) +
  facet_wrap(~ STNAME, scales = "free", ncol = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(1, 8), breaks = c(10000, 100000, 1000000)) +
  labs(
    title = "Depression Rates vs. Republican Voting by State",
    subtitle = "Mixed-effects model with random slopes; logit-transformed response back to percentage",
    x = "Depression Prevalence (Centered)",
    y = "Republican Vote (%)",
    color = "White %",
    size = "Population"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 8))

model_rs <- lmer(log(per_gop) ~ depression_centered + race_sc+ gender_ratio+scale(log(total_votes))+
                   (1 + depression_centered|STNAME), data = df, weights = sqrt(TOT_POP))
summary(model_rs)

bmodel_rs <- lmer(log(per_gop) ~ Crude.Prevalence.Estimate + race+ gender_ratio+total_votes+
                   (1 + Crude.Prevalence.Estimate|STNAME), data = df)
summary(bmodel_rs)

install.packages("ggResidpanel")
library(ggResidpanel)
resid_panel(model_rs,
            plots = c("qq", "hist", "resid", "fitted"),
            qqbands = TRUE,
            smoother = TRUE,
            theme = "bw")
anova(model_rs,bmodel_rs)
