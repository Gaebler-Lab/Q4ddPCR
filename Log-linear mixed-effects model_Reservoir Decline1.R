library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(boot)
library(openxlsx)
library(lattice)

setwd("/directory") # directory

# Import the Excel file
data <- read_excel("../directory/datafile.xlsx", sheet = "Step_Down set to 1") # import Q4ddPCR and IPDA data from Excel file containing the following columns: Participant.ID;	IPDA;	Q4ddPCR;	Time on ART (years)

# Reshape data into long format for plotting and modeling
data_long <- data %>%
  pivot_longer(cols = c(Q4ddPCR, IPDA), names_to = "Method", values_to = "Reservoir") %>%
  filter(!is.na(Reservoir)) 

# Convert reservoir values to logarithmic values
data_clean <- data_long %>%
  mutate(Reservoir = ifelse(Reservoir <= 0, NA, Reservoir),  # Prevent log(0) issues, 0 values that were due to a reservoir size below LoD where set to 1 before
         LogReservoir = log(Reservoir)) %>%
  filter(!is.na(LogReservoir))  # Remove NA values

# Mutate Participant.ID into a factor so it is treated categorical
data_clean <- data_clean %>%
  mutate(Participant.ID = as.factor(Participant.ID))

# Mixed-Effects Model with Random Effects for Intercept and Slope

# mixed-effects model with random effects for intercept (baseline reservoir) and slope (inter-individual differences)
model_log <- lmer(LogReservoir ~ `Time on ART (years)` * Method + (1 + `Time on ART (years)` | Participant.ID), 
                  data = data_clean)

# Extract fixed effects
fixefs <- fixef(model_log)

# Slopes
slope_IPDA <- fixefs["`Time on ART (years)`"]
slope_Q4ddPCR <- slope_IPDA + fixefs["`Time on ART (years)`:MethodQ4ddPCR"]

# Compute half-lives
halflife_IPDA <- log(2) / -slope_IPDA
halflife_Q4ddPCR <- log(2) / -slope_Q4ddPCR

# Show slopes and half-life calculated with the mixed effect model 
cat("Slope IPDA (log scale):", slope_IPDA, "\n")
cat("Slope Q4ddPCR (log scale):", slope_Q4ddPCR, "\n")
cat("Half-life for IPDA:", halflife_IPDA, "years\n")
cat("Half-life for Q4ddPCR:", halflife_Q4ddPCR, "years\n")

# Extract standard errors from model
model_summary <- summary(model_log)

# Extract variance-covariance matrix
vcov_matrix <- vcov(model_log)
# Calculate variance for the sum β₁ + β₃
var_Q4ddPCR_slope <- vcov_matrix["`Time on ART (years)`", "`Time on ART (years)`"] + 
  vcov_matrix["`Time on ART (years)`:MethodQ4ddPCR", "`Time on ART (years)`:MethodQ4ddPCR"] +
  2 * vcov_matrix["`Time on ART (years)`", "`Time on ART (years)`:MethodQ4ddPCR"]

# SE slope (accounts for correlation)
se_slope_IPDA <- model_summary$coefficients["`Time on ART (years)`", "Std. Error"]
se_slope_Q4ddPCR <- sqrt(var_Q4ddPCR_slope)

# Approximate SE for half-life using delta method: SE(h(x)) ≈ |dh/dx| * SE(x)
# where h(x) = log(2) / -x
se_halflife_IPDA <- abs(log(2) / slope_IPDA^2) * se_slope_IPDA
se_halflife_Q4ddPCR <- abs(log(2) / slope_Q4ddPCR^2) * se_slope_Q4ddPCR

# Confidence intervals (Wald method)
conf <- confint(model_log, method = "Wald")
ci_slope_IPDA <- conf["`Time on ART (years)`", ]
ci_slope_Q4ddPCR <- slope_Q4ddPCR + c(-1.96, 1.96) * se_slope_Q4ddPCR
ci_halflife_IPDA <- log(2) / -rev(ci_slope_IPDA)
ci_halflife_Q4ddPCR <- log(2) / -rev(ci_slope_Q4ddPCR)

# Bootstrap confidence intervals for full mixed-effects model
pred_fun <- function(model) predict(model, re.form = NULL)
boot_results <- bootMer(model_log, pred_fun, nsim = 1000)

# Compute 95% confidence intervals
data_clean$Predicted_Lower <- exp(apply(boot_results$t, 2, function(x) quantile(x, 0.025)))
data_clean$Predicted_Upper <- exp(apply(boot_results$t, 2, function(x) quantile(x, 0.975)))

# Generate predicted values from the mixed-effects model, fixed effect predictions only ignoring interindividual variation to compare overall model trend
data_clean <- data_clean %>%
  mutate(Predicted = predict(model_log, re.form = NA))  

# Test if the difference in slopes is significant
# Full model: allows different slopes for each method
full_model <- lmer(LogReservoir ~ `Time on ART (years)` * Method + (1 + `Time on ART (years)` | Participant.ID), 
                   data = data_clean)

# Reduced model: assumes same slope for both methods (no interaction)
reduced_model <- lmer(LogReservoir ~ `Time on ART (years)` + Method + (1 + `Time on ART (years)` | Participant.ID), 
                      data = data_clean)

# Likelihood Ratio Test specifically for slope difference
lrt_slope_diff <- anova(reduced_model, full_model)
lrt_slope_p_value <- lrt_slope_diff$`Pr(>Chisq)`[2]

# Direct t-test of the interaction coefficient
interaction_coef <- summary(full_model)$coefficients["`Time on ART (years)`:MethodQ4ddPCR", ]
interaction_t_value <- interaction_coef["t value"]
interaction_p_value <- 2 * (1 - pt(abs(interaction_t_value), df = df.residual(full_model)))

# Print results
cat("=== Test for Difference in Slopes ===\n")
cat("LRT Chi-square:", lrt_slope_diff$Chisq[2], "\n")
cat("LRT p-value:", lrt_slope_p_value, "\n")
cat("Interaction coefficient:", interaction_coef["Estimate"], "\n")
cat("Interaction t-value:", interaction_t_value, "\n")
cat("Interaction p-value:", interaction_p_value, "\n")

if(lrt_slope_p_value < 0.05) {
  cat("Conclusion: The slopes are significantly different (p < 0.05)\n")
} else {
  cat("Conclusion: No significant difference in slopes (p >= 0.05)\n")
}

# Test model fitting
# 1. Model convergence check
cat("Convergence code:", model_log@optinfo$conv$lme4$code, "\n")

# 2. Complete residual diagnostics
par(mfrow = c(2, 2))
plot(fitted(model_log), resid(model_log), main = "Residuals vs Fitted")
qqnorm(resid(model_log), main = "Q-Q Plot")
qqline(resid(model_log))
plot(data_clean$`Time on ART (years)`, resid(model_log), main = "Residuals vs Time")
hist(resid(model_log), main = "Residual Distribution")

# 3. Random effects examination
print(VarCorr(model_log))  # Variance components
dotplot(ranef(model_log))  # Individual random effects

# 4. Model comparison with AIC/BIC
null_model <- lmer(LogReservoir ~ Method + (1 + `Time on ART (years)` | Participant.ID), data = data_clean)
AIC(null_model, model_log)
BIC(null_model, model_log)

# Plot for mixed effect model
mixed_effect_plot <- ggplot(data_clean, aes(x = `Time on ART (years)`, y = Reservoir, color = Method)) +
  geom_point(size = 3, alpha = 0.6) +  
  geom_line(aes(group = interaction(Participant.ID, Method)), color = "gray", size = 0.5, alpha = 0.5)+
  geom_line(aes(y = exp(Predicted)), size = 1.5) +  
  geom_ribbon(aes(ymin = Predicted_Lower, ymax = Predicted_Upper, fill = Method), alpha = 0.1) +  
  scale_y_log10(limits = c(1, 70000), breaks = c(1, 10, 100, 1000, 10000), labels = scales::label_comma(),minor_breaks = rep(1:9, times = 4) * 10^rep(0:3, each = 9),expand = c(0, 0),) +
  scale_x_continuous(limits = c(1, 4.6), expand = c(0, 0)) +
  scale_color_manual(values = c("Q4ddPCR" = "#00C000", "IPDA" = "#5757F9")) +  
  scale_fill_manual(values = c("Q4ddPCR" = "#00C000", "IPDA" = "#5757F9")) +  
  labs(
    title = "",
    x = "Time on ART (years)",
    y = expression(bold("Intact provirus/") ~ bold("10")^bold("6") ~ bold("CD4")^bold("+") ~ bold("cells")),
    color = "",
    fill = ""  
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 1, family = "Arial"),
    axis.title = element_text(face = "bold",size = 20, family = "Arial"),
    axis.title.y = element_text(margin = margin(r = 0.02)),  # Move Y-axis title closer
    axis.line = element_blank(),  # Remove default axis lines
    axis.ticks = element_line(size = 1.5),  # Add ticks on both axes
    axis.ticks.length = unit(0.4, "cm"),  # Extend the tick length
    axis.text = element_text(face = "bold", color = "black", size = 18, family = "Arial"),  # Make tick labels bold
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "right",  # Move the legend to the side
    legend.title = element_text(face = "bold", size = 18, family = "Arial"),  # Make legend title bold
    legend.text = element_text(size = 18, family = "Arial"),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  ) +
  annotation_logticks(
    sides = "l",               # Left side (y-axis)
    outside = TRUE,            # Place ticks outside the plot area
    short = unit(0.2, "cm"),
    mid   = unit(0.2, "cm"),
    long  = unit(0.2, "cm"),
    size = 1.8,                # Thicker ticks
    color = "black"
  )+
  
  # Add custom X and Y axes at the bottom and left
  geom_hline(yintercept = 1, size = 1.5, color = "black") +  # Horizontal axis at Y=1
  geom_vline(xintercept = 1, size = 1.5, color = "black") +  # Vertical axis at X=0
  coord_cartesian(clip = "off")  # Ensure ticks and labels are visible at the edges

# Display the plot in the console
print(mixed_effect_plot) 

# Save the plot with high quality
ggsave(
  filename = "reservoir_decline_plot_mixedeffect.png",  # File name
  plot = mixed_effect_plot,                             # Plot object
  device = "png",                          # File format
  dpi = 300,                               # High resolution (300 dpi)
  width = 10,                              # Width of the image in inches
  height = 7,                              # Height of the image in inches
  units = "in"                             # Specify the unit (inches)
)

print("Plot for reservoir decline_mixed model has been saved")

# ---------------- Excel Output ----------------
wb <- createWorkbook()

halflife_summary <- data.frame(
  Method = c("IPDA", "Q4ddPCR"),
  Slope = c(slope_IPDA, slope_Q4ddPCR),
  Slope_SE = c(se_slope_IPDA, se_slope_Q4ddPCR),
  Slope_CI_Lower = c(ci_slope_IPDA[1], ci_slope_Q4ddPCR[1]),
  Slope_CI_Upper = c(ci_slope_IPDA[2], ci_slope_Q4ddPCR[2]),
  HalfLife = c(halflife_IPDA, halflife_Q4ddPCR),
  HalfLife_SE = c(se_halflife_IPDA, se_halflife_Q4ddPCR),
  HalfLife_CI_Lower = c(ci_halflife_IPDA[1], ci_halflife_Q4ddPCR[1]),
  HalfLife_CI_Upper = c(ci_halflife_IPDA[2], ci_halflife_Q4ddPCR[2]),
  Slope_Difference_LRT_P = c(lrt_slope_p_value, NA),
  Interaction_P_value = c(interaction_p_value, NA)
)

addWorksheet(wb, "Mixed Effect Model")
writeData(wb, "Mixed Effect Model", halflife_summary)

saveWorkbook(wb, "ResultsTest.xlsx", overwrite = TRUE)
cat("Excel file has been exported successfully\n")