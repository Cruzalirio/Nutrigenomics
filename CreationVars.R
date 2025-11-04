library(tidyverse)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(scales)

datos = read_csv("https://raw.githubusercontent.com/Cruzalirio/Unal-Enfermeria-/main/Datos/diabetes_BRFSS2015.csv")


data <- datos %>%
  mutate(
    # 1. Inflammatory Index
    Inflammatory_Index = 0.25*BMI + 0.4*GenHlth +
      3*(1 - PhysActivity) +
      2*(1 - Veggies) + 2*(1 - Fruits) +
      rlnorm(n(), 0, 0.3),

    # 2. Insulin Resistance Score
    Insulin_Resistance_Score = 0.4*BMI + 2*HighBP + 0.5*Age -
      3*PhysActivity + rnorm(n(), 0, 3),
    
    # 3. Oxidative Stress
    Oxidative_Stress = 5*Smoker + 4*HvyAlcoholConsump +
      0.3*Age + rlnorm(n(), 0, 0.6),
    
    # 4. Antioxidant Intake
    Antioxidant_Intake = 15*Fruits + 15*Veggies +
      rexp(n(), rate = 0.2),
    
    # 5. Cardiometabolic Risk
    Cardiometabolic_Risk = 5*HighBP + 4*HighChol +
      0.2*BMI + 0.4*Age + 
      rnorm(n(), 0, 2),
    
    # 6. Sleep Quality Score
    Sleep_Quality_Score = 100 - (1.2*MentHlth + 1.1*PhysHlth + 
                                   8*GenHlth + rnorm(n(), 0, 5)),
    
    # 7. Gut Microbiome Balance
    Gut_Microbiome_Balance = 10*Fruits + 10*Veggies -
      5*HvyAlcoholConsump - 0.5*BMI +
      rnorm(n(), 0, 3),
    
    # 8. Nutritional Score
    Nutritional_Score = 0.6*Antioxidant_Intake + 0.4*Gut_Microbiome_Balance -
      0.2*BMI + rnorm(n(), 0, 4),
    
    # 9. Inflammation-to-Antioxidant Ratio
    Inflammation_to_Antioxidant_Ratio = Inflammatory_Index /
      (Antioxidant_Intake + 1),
    
    # 10. Biological Age
    Biological_Age = 20 + 0.8*Age + 0.3*Cardiometabolic_Risk +
      0.2*Inflammatory_Index -
      0.1*Antioxidant_Intake + rnorm(n(), 0, 4) )


vars <- c("Inflammatory_Index", "Insulin_Resistance_Score", "Oxidative_Stress",
          "Antioxidant_Intake", "Cardiometabolic_Risk", "Sleep_Quality_Score",
          "Gut_Microbiome_Balance", "Nutritional_Score",
          "Inflammation_to_Antioxidant_Ratio", "Biological_Age")

data <- data %>%
  mutate(across(all_of(vars),
                ~ rescale(., to = c(0, 100), na.rm = TRUE)))



for (v in vars) {
  miss_idx <- sample(1:nrow(data), size = 0.05 * nrow(data))
  data[miss_idx, v] <- NA
}


for (v in vars) {
  data[, v] <- round( data[, v],1)
}


data %>%
  select(Inflammatory_Index:Biological_Age) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribuciones de variables nutrigenómicas simuladas")



pca_data <- data %>%
  select(Inflammatory_Index:Biological_Age) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))



res.pca <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("blue", "orange", "red"))
fviz_pca_ind(res.pca, geom.ind = "point", pointshape = 21,
             fill.ind = "lightblue", alpha.ind = 0.5) +
  labs(title = "PCA de variables nutrigenómicas simuladas")


write_csv(data, file="diabetes_BRFSS2015.csv")
