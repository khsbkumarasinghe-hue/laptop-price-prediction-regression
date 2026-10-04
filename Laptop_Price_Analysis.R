# ==============================================================================
# 0. LOAD LIBRARIES & INITIAL IMPORT
# ==============================================================================
setwd("C://Users//USER//Desktop//ST3008")
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(MASS)
library(car)
library(psych)
library(corrplot)
library(brant)

# Import the original dataset
attach(laptop_price)

head(laptop_price)
dim(laptop_price)
names(laptop_price)
str(laptop_price)

# To avoid problems caused by spaces in column names:
names(laptop_price) <- make.names(names(laptop_price))
names(laptop_price)

# Preserve the original dataset
laptop_clean <- laptop_price
View(laptop_clean)

# laptop_price remains unchanged.
# laptop_clean will be cleaned.
# Later, laptop_final will be used for both models.


# ==============================================================================
# 1. INITIAL DATA AUDIT & MISSING VALUES
# ==============================================================================

# Check missing values and duplicates
colSums(is.na(laptop_clean))

# Check completely duplicated rows:
sum(duplicated(laptop_clean))

# Check the number of rows:
dim(laptop_clean)


# ==============================================================================
# 2. FEATURE CLEANING & DATA TRANSFORMATION
# ==============================================================================

# Clean the response variable
# Ensure that Price_euros is numeric:
laptop_clean$Price_euros <- as.numeric(laptop_clean$Price_euros)
summary(laptop_clean$Price_euros)

# Remove observations with missing or non-positive prices:
laptop_clean <- laptop_clean %>%
  filter(
    !is.na(Price_euros),
    Price_euros > 0
  )

unique(laptop_clean$Ram) 

# Convert RAM to a numeric variable
laptop_clean <- laptop_clean %>%
  mutate(
    Ram_GB = as.numeric(str_trim(str_remove(Ram, regex("GB", ignore_case = TRUE))))
  )

table(laptop_clean$Ram, laptop_clean$Ram_GB)
summary(laptop_clean$Ram_GB)

# Convert Weight to numeric kilograms 
laptop_clean <- laptop_clean %>%
  mutate(
    Weight_kg = as.numeric(str_trim(str_remove(Weight, regex("kg", ignore_case = TRUE))))
  ) 

summary(laptop_clean$Weight_kg)

# Clean the operating-system variable
# Combining macOS and Mac OS X is reasonable because they are versions of the same operating-system family.
unique(laptop_price$OpSys)
laptop_clean <- laptop_clean %>%
  mutate(
    OpSys_Group = case_when(
      OpSys %in% c("macOS", "Mac OS X") ~ "Mac",
      str_detect(OpSys, regex("Windows", ignore_case = TRUE)) ~ "Windows",
      str_detect(OpSys, regex("Linux", ignore_case = TRUE)) ~ "Linux",
      str_detect(OpSys, regex("Chrome", ignore_case = TRUE)) ~ "Chrome OS",
      TRUE ~ "Other"
    )
  )

# Windows 10 - 1072 , Windows 10 S - 8 , Windows 7 - 45
table(laptop_clean$OpSys, laptop_clean$OpSys_Group)
table(laptop_clean$OpSys_Group)

# Create meaningful CPU groups
# Original CPU variable has 118 levels, many appearing only once or twice. 
# Therefore, extract the meaningful processor family.
unique(laptop_clean$Cpu)

laptop_clean <- laptop_clean %>%
  mutate(
    Cpu_Group = case_when(
      # Intel Core Series
      str_detect(Cpu, "(?i)Intel Core i9") ~ "Intel Core i9",
      str_detect(Cpu, "(?i)Intel Core i7") ~ "Intel Core i7",
      str_detect(Cpu, "(?i)Intel Core i5") ~ "Intel Core i5",
      str_detect(Cpu, "(?i)Intel Core i3") ~ "Intel Core i3",
      str_detect(Cpu, "(?i)Intel Core M")  ~ "Intel Core M",
      
      # Other Intel Lines (Matches Dual/Quad/X5 variants cleanly)
      str_detect(Cpu, "(?i)Intel Celeron") ~ "Intel Celeron",
      str_detect(Cpu, "(?i)Intel Pentium") ~ "Intel Pentium",
      str_detect(Cpu, "(?i)Intel Atom")    ~ "Intel Atom",
      str_detect(Cpu, "(?i)Intel Xeon")    ~ "Intel Xeon",
      
      # AMD Series (Matches A4, A6, A8, A10, A12 series cleanly)
      str_detect(Cpu, "(?i)AMD Ryzen")     ~ "AMD Ryzen",
      str_detect(Cpu, "(?i)AMD A[0-9]+")   ~ "AMD A-Series", 
      str_detect(Cpu, "(?i)AMD E-Series|AMD E[0-9]+") ~ "AMD E-Series",
      str_detect(Cpu, "(?i)AMD FX")        ~ "AMD FX",
      
      # Catch-all fallback (Handles Samsung and anything else)
      TRUE ~ "Other"
    )
  )

# Check the new grouping:
table(laptop_clean$Cpu_Group)
unique(laptop_clean$Cpu_Group)

# Cross-check original and new values:
table(laptop_clean$Cpu_Group, useNA = "ifany")

find("select")

# View all column names to confirm Cpu_Group exists
names(laptop_clean)

# Check the mappings side-by-side to audit your regex work
laptop_clean %>%
  dplyr::select(Cpu, Cpu_Group) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Cpu_Group, Cpu)

# Create meaningful GPU groups
# Instead of using 110 individual GPU names, create meaningful GPU families.
unique(laptop_clean$Gpu)

laptop_clean <- laptop_clean %>%
  mutate(
    Gpu_Group = case_when(
      # Nvidia Sub-Groups
      str_detect(Gpu, "(?i)Nvidia.*GTX")    ~ "Nvidia GTX",
      str_detect(Gpu, "(?i)Nvidia.*MX")     ~ "Nvidia MX",
      str_detect(Gpu, "(?i)Nvidia.*Quadro") ~ "Nvidia Quadro",
      str_detect(Gpu, "(?i)Nvidia")         ~ "Nvidia Other",
      
      # AMD Sub-Groups
      str_detect(Gpu, "(?i)AMD.*FirePro")   ~ "AMD FirePro",
      # Catches "Radeon", "R4 Graphics", and "R17M..." variants safely
      str_detect(Gpu, "(?i)AMD.*(Radeon|R[0-9]+|R[0-9]+M)") ~ "AMD Radeon",
      str_detect(Gpu, "(?i)AMD")            ~ "AMD Other",
      
      # Intel Sub-Groups
      str_detect(Gpu, "(?i)Intel.*Iris")    ~ "Intel Iris",
      str_detect(Gpu, "(?i)Intel")          ~ "Intel Integrated",
      
      # Catch-all fallback (Handles ARM Mali, etc.)
      TRUE ~ "Other"
    )
  )

# Check:
table(laptop_clean$Gpu_Group)
unique(laptop_clean$Gpu_Group)

# View the mapping:
laptop_clean %>%
  dplyr::select(Gpu, Gpu_Group) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Gpu_Group, Gpu)

# Engineer storage variables
# Create storage type
unique(laptop_clean$Memory)

laptop_clean <- laptop_clean %>%
  mutate(
    Storage_Type = case_when(
      # Dual-drive combinations (Keep these at the top!)
      str_detect(Memory, "(?i)SSD") & str_detect(Memory, "(?i)HDD")    ~ "SSD + HDD",
      str_detect(Memory, "(?i)SSD") & str_detect(Memory, "(?i)Hybrid") ~ "SSD + Hybrid",
      str_detect(Memory, "(?i)Flash") & str_detect(Memory, "(?i)HDD")  ~ "Flash + HDD", # Caught row [27]!
      
      # Single-drive categories
      str_detect(Memory, "(?i)SSD")    ~ "SSD",
      str_detect(Memory, "(?i)HDD")    ~ "HDD",
      str_detect(Memory, "(?i)Hybrid") ~ "Hybrid",
      str_detect(Memory, "(?i)Flash")  ~ "Flash",
      
      # Fallback
      TRUE ~ "Other"
    )
  )

# Check
table(laptop_clean$Storage_Type)
unique(laptop_clean$Storage_Type)

# Create total storage capacity
# This function separates storage components and converts TB into GB.
get_total_storage <- function(memory_vector) {
  purrr::map_dbl(memory_vector, function(memory_value) {
    if (is.na(memory_value)) return(NA_real_)
    
    # Split the single string by "+"
    parts <- str_split(memory_value, "\\+")[[1]]
    
    capacities <- sapply(parts, function(part) {
      part <- str_trim(part)
      
      # Using standard [0-9.]+ safely extracts decimals like 1.0
      value <- as.numeric(str_extract(part, "[0-9.]+"))
      
      if (is.na(value)) return(0)
      if (str_detect(part, "(?i)TB")) value <- value * 1024
      
      return(value)
    })
    
    return(sum(capacities))
  })
}

# Now you can use it in a standard fast mutate block:
laptop_clean <- laptop_clean %>%
  mutate(Total_Storage_GB = get_total_storage(Memory))

# Check
laptop_clean %>%
  dplyr::select(Memory, Storage_Type, Total_Storage_GB) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Total_Storage_GB)

# Summary
summary(laptop_clean$Total_Storage_GB)
unique(laptop_clean$Total_Storage_GB)

# Thus:
# 256GB SSD          → 256 GB
# 1TB HDD            → 1024 GB
# 256GB SSD + 1TB HDD → 1280 GB
# This preserves the storage-capacity information instead of discarding it.

# Engineer screen-resolution variables
# Touchscreen
unique(laptop_clean$ScreenResolution)

laptop_clean <- laptop_clean %>%
  mutate(
    Touchscreen = if_else(str_detect(ScreenResolution, regex("Touchscreen", ignore_case = TRUE)), "Yes", "No")
  )

# IPS panel
laptop_clean <- laptop_clean %>%
  mutate(
    IPS = if_else(str_detect(ScreenResolution, regex("IPS", ignore_case = TRUE)), "Yes", "No")
  )

# Extract horizontal and vertical resolution
resolution_text <- str_extract(laptop_clean$ScreenResolution, "[0-9]+x[0-9]+")

laptop_clean <- laptop_clean %>%
  mutate(
    # Pull the digits explicitly flanking the 'x' anywhere in the string
    X_res = as.numeric(str_extract(ScreenResolution, "[0-9]+(?=x)")),
    Y_res = as.numeric(str_extract(ScreenResolution, "(?<=x)[0-9]+"))
  )

# Check the extracted variables
laptop_clean %>%
  dplyr::select(
    ScreenResolution,
    X_res,
    Y_res,
    Touchscreen,
    IPS
  ) %>%
  dplyr::distinct()

colSums(is.na(laptop_clean))

# Create a resolution group
laptop_clean <- laptop_clean %>%
  mutate(
    Resolution_Group = case_when(
      # Both dimensions must meet the 4K baseline
      X_res >= 3840 & Y_res >= 2160 ~ "4K",
      
      # Catches 2560x1600, 2880x1800, 2304x1440, 2400x1600, etc.
      X_res >= 2304 & Y_res >= 1400 ~ "QHD or Retina",
      
      # Catches 1920x1080 and 1920x1200 perfectly
      X_res >= 1920 & Y_res >= 1080 ~ "Full HD",
      
      # Everything else (1366x768, 1440x900, 1600x900)
      TRUE                          ~ "HD"
    )
  )

# Check
table(laptop_clean$Resolution_Group)
unique(laptop_clean$Resolution_Group)
# This produces meaningful categories without treating every text variation as a different screen type.

# Group rare companies
# Company has fewer levels than CPU or GPU, but very rare companies may still create unstable estimates.
# First check:
sort(table(laptop_clean$Company))

# Group companies with fewer than 10 observations:
laptop_clean <- laptop_clean %>%
  mutate(
    Company_Group = fct_lump_min(
      factor(Company),
      min = 5,
      other_level = "Other"  # Huawei, Chuwi, Fujitsu, Google, LG, Vero
    )
  )

# Check:
table(laptop_clean$Company_Group)

# Keep TypeName as it is
# Check it:
table(laptop_clean$TypeName)

# Convert it to a factor:
laptop_clean$TypeName <- factor(laptop_clean$TypeName)

# Check the remaining sample size:
dim(laptop_clean)
names(laptop_clean)


# ==============================================================================
# 3. CREATING & BUILDING THE FINAL UNIFIED DATASET
# ==============================================================================

# We will now select only variables that are statistically meaningful and interpretable.
laptop_final <- laptop_clean %>%
  dplyr::select(
    Price_euros,
    Company_Group,
    TypeName,
    Inches,
    Ram_GB,
    Weight_kg,
    Cpu_Group,
    Gpu_Group,
    OpSys_Group,
    Storage_Type,
    Total_Storage_GB,
    Resolution_Group,
    Touchscreen,
    IPS
  ) %>%
  tidyr::drop_na() %>%
  droplevels()

# Check the final dataset
dim(laptop_final)
str(laptop_final)
summary(laptop_final)

# Remove rows with missing values:
laptop_final <- laptop_final %>%
  filter(complete.cases(.))

# Convert categorical variables to factors:
factor_variables <- c(
  "Company_Group",
  "TypeName",
  "Cpu_Group",
  "Gpu_Group",
  "OpSys_Group",
  "Storage_Type",
  "Resolution_Group",
  "Touchscreen",
  "IPS"
)

laptop_final[factor_variables] <- lapply(
  laptop_final[factor_variables],
  factor
)

# Drop unused levels:
laptop_final <- droplevels(laptop_final)

# Check:
str(laptop_final)
dim(laptop_final)
summary(laptop_final)

# This is now the single final dataset
laptop_final <- laptop_final %>%
  mutate(
    Storage_Type = fct_collapse(Storage_Type,
                                "Hybrid"    = c("Hybrid", "SSD + Hybrid"),
                                "SSD + HDD" = c("SSD + HDD", "Flash + HDD")
    )
  )

# Remove unused levels
laptop_final <- droplevels(laptop_final)
table(laptop_final$Storage_Type)

# Names of all categorical variables
factor_variables <- c(
  "Company_Group",
  "TypeName",
  "Cpu_Group",
  "Gpu_Group",
  "OpSys_Group",
  "Storage_Type",
  "Resolution_Group",
  "Touchscreen",
  "IPS"
)

# Display frequencies of every category
lapply(
  laptop_final[factor_variables],
  table
)

# Check the number of levels:
sapply(
  laptop_final[factor_variables],
  nlevels
)

# Automatically identify levels with fewer than 10 observations:
rare_categories <- lapply(
  laptop_final[factor_variables],
  function(x) {
    frequency <- table(x)
    frequency[frequency < 10]
  }
)
rare_categories

sapply(
  laptop_final[factor_variables],
  function(x) min(table(x))
)

# Cpu:              115 original levels -> 13 grouped levels
# Gpu:              102 original levels -> 10 grouped levels
# Memory:            36 original levels -> 5 storage groups
# ScreenResolution:  40 original levels -> 4 resolution groups

str(laptop_final)

# ---------------------------------------------------------
# Lump remaining rare CPU and GPU categories
# ---------------------------------------------------------
laptop_final <- laptop_final %>%
  mutate(
    Cpu_Group = forcats::fct_lump_min(
      factor(Cpu_Group),
      min = 10,
      other_level = "Other CPU"
    ),
    
    Gpu_Group = forcats::fct_lump_min(
      factor(Gpu_Group),
      min = 10,
      other_level = "Other GPU"
    )
  ) %>%
  droplevels()

# Check revised categories
table(laptop_final$Cpu_Group)
table(laptop_final$Gpu_Group)

# Check sample size
dim(laptop_final)
View(laptop_final)
head(laptop_final, 5)


# ==============================================================================
# 4. MULTIPLE LINEAR REGRESSION MODELLING
# ==============================================================================

# ---------------------------------------------------------
# Full Model Definition
# OpSys_Group is excluded because Mac = Apple exactly
# ---------------------------------------------------------
linear_full <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    # OpSys_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen +
    IPS,
  data = laptop_final
)

summary(linear_full)

# ---------------------------------------------------------
# Check model rank
# ---------------------------------------------------------
X_linear <- model.matrix(linear_full)

ncol(X_linear)
qr(X_linear)$rank

ncol(X_linear) == qr(X_linear)$rank
any(is.na(coef(linear_full)))
alias(linear_full)

# ---------------------------------------------------------
# Check multicollinearity
# ---------------------------------------------------------
vif_values <- car::vif(linear_full)
vif_values

if (is.matrix(vif_values)) {
  adjusted_gvif <- vif_values[, "GVIF^(1/(2*Df))"]
  adjusted_gvif
}

# ---------------------------------------------------------
# Backward variable selection
# ---------------------------------------------------------
model_b1 <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen +
    IPS,
  data = laptop_final
)
summary(model_b1)
drop1(model_b1, test = "F")

model_b2 <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS,
  data = laptop_final
)
drop1(model_b2, test = "F")

model_b3 <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    IPS,
  data = laptop_final
)
drop1(model_b3, test = "F")
summary(model_b3)

table(laptop_final$Touchscreen, laptop_final$IPS)

model_b4 <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group,
  data = laptop_final
)
drop1(model_b4, test = "F")
summary(model_b4)

# ---------------------------------------------------------
# Linear-model assumptions checking
# ---------------------------------------------------------
par(mfrow = c(2, 2))
plot(model_b4)
par(mfrow = c(1, 1))

shapiro.test(residuals(model_b4))
lmtest::bptest(model_b4)

# ---------------------------------------------------------
# Forward variable selection
# ---------------------------------------------------------
model_f0 <- lm(
  log(Price_euros) ~ 1,
  data = laptop_final
)

add1(
  model_f0,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen +
    IPS,
  test = "F"
)

model_f1 <- lm(
  log(Price_euros) ~ Ram_GB,
  data = laptop_final
)

add1(
  model_f1,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
) 

model_f2 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group,
  data = laptop_final
)

add1(
  model_f2,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f3 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches,
  data = laptop_final
)

add1(
  model_f3,
  scope = ~
    Company_Group +
    TypeName + 
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f4 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName,
  data = laptop_final
)

add1(
  model_f4,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f5 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName + 
    Resolution_Group,
  data = laptop_final
)

add1(
  model_f5,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f6 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName + 
    Resolution_Group + 
    Storage_Type,
  data = laptop_final
)

add1(
  model_f6,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f7 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName + 
    Resolution_Group + 
    Storage_Type + 
    Company_Group,
  data = laptop_final
)

add1(
  model_f7,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f8 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName + 
    Resolution_Group + 
    Storage_Type + 
    Company_Group + 
    Weight_kg,
  data = laptop_final
)

add1(
  model_f8,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_f9 <- lm(
  log(Price_euros) ~
    Ram_GB +
    Cpu_Group + 
    Inches + 
    TypeName + 
    Resolution_Group + 
    Storage_Type + 
    Company_Group + 
    Weight_kg + 
    Gpu_Group,
  data = laptop_final
)

add1(
  model_f9,
  scope = ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group +
    Touchscreen +
    IPS +
    Total_Storage_GB,
  test = "F"
)

model_final <- lm(
  log(Price_euros) ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group,
  data = laptop_final
)


# ==============================================================================
# 5. EXPLORATORY DATA ANALYSIS (EDA) ON FINAL DATA
# ==============================================================================

## Dataset Structure
str(laptop_final)
dim(laptop_final)
names(laptop_final)

## Summary Statistics
summary(laptop_final)

describe(laptop_final[, c(
  "Price_euros",
  "Inches",
  "Ram_GB",
  "Weight_kg",
  "Total_Storage_GB"
)])

# Histograms

library(patchwork) 

hist_plots = function(var_name, title_override = NULL){
  
  plot_var <- if(var_name == "log_price") sym("Price_euros") else sym(var_name)
  
  p <- ggplot(laptop_final, aes(x = if(var_name == "log_price") log(!!plot_var) else !!plot_var)) + 
    geom_histogram(bins = 10, fill = "lightblue", color = "grey") +
    labs(
      x = if(!is.null(title_override)) title_override else paste("Distribution of", var_name), 
      y = "Frequency"
    ) +
    theme_minimal()
  
  return(p)
}

p1 <- hist_plots("Price_euros")
p2 <- hist_plots("log_price", title_override = "Distribution of log(Price_euros)")
p3 <- hist_plots("Ram_GB")
p4 <- hist_plots("Weight_kg")
p5 <- hist_plots("Inches")
p6 <- hist_plots("Total_Storage_GB")

(p1 | p2 | p3) / (p4 | p5 | p6)

par(mfrow = c(1, 1))

# Boxplots
par(mfrow = c(2, 2))
boxplot(laptop_final$Price_euros, main = "Price")
boxplot(laptop_final$Ram_GB, main = "RAM")
boxplot(laptop_final$Weight_kg, main = "Weight")
boxplot(laptop_final$Total_Storage_GB, main = "Storage")
par(mfrow = c(1, 1))

# Scatter Plots
library(ggplot2)
library(patchwork) # For grid layout

# 1. Define a robust scatter plot function
scatter_plots = function(x_var, x_label){
  
  # Convert the string column name into a symbol that ggplot can evaluate
  p <- ggplot(laptop_final, aes(x = !!sym(x_var), y = Price_euros)) + 
    geom_point(alpha = 0.5, color = "darkblue") +  # alpha adds transparency to see overlapping points
    labs(x = x_label, y = "Price (Euros)") +
    theme_minimal()
  
  return(p)
}

# 2. Generate the individual scatter plot objects
s1 <- scatter_plots("Ram_GB", "RAM (GB)")
s2 <- scatter_plots("Weight_kg", "Weight (kg)")
s3 <- scatter_plots("Inches", "Screen Size (Inches)")
s4 <- scatter_plots("Total_Storage_GB", "Storage (GB)")

# 3. Arrange them into a clean 2x2 grid
(s1 | s2) / (s3 | s4)

ggplot(laptop_final, aes(x = reorder(Cpu_Group, log(Price_euros), FUN = median), 
                         y = log(Price_euros), 
                         fill = Cpu_Group)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "Log Price Distribution by CPU Group",
    x = "CPU Group",
    y = "log(Price in Euros)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # <--- Rotates 90 degrees
  )
ggplot(laptop_final, aes(x = reorder(Gpu_Group, log(Price_euros), FUN = median), 
                         y = log(Price_euros), 
                         fill = Gpu_Group)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "Log Price Distribution by GPU Group",
    x = "GPU Group",
    y = "log(Price in Euros)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # <--- Rotates 90 degrees
  )

ggplot(laptop_final, aes(x = reorder(TypeName, log(Price_euros), FUN = median), 
                         y = log(Price_euros), 
                         fill = TypeName)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "Log Price Distribution by Laptop Type",
    x = "Laptop Type",
    y = "log(Price in Euros)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # <--- Rotates 90 degrees
  )


# Correlation Matrix
numeric_data <- laptop_final[, c(
  "Price_euros",
  "Inches",
  "Ram_GB",
  "Weight_kg",
  "Total_Storage_GB"
)]
cor(numeric_data)

corrplot(cor(numeric_data), method = "number")

# Frequency Tables 
table(laptop_final$Company_Group)
table(laptop_final$TypeName)
table(laptop_final$Cpu_Group)
table(laptop_final$Gpu_Group)
table(laptop_final$Storage_Type)
table(laptop_final$Resolution_Group)
table(laptop_final$Touchscreen)
table(laptop_final$IPS)

## Boxplots for Categorical Variables vs Price
boxplot(Price_euros ~ Company_Group, data = laptop_final, las = 2, main = "Price by Company")
boxplot(Price_euros ~ TypeName, data = laptop_final, las = 2, main = "Price by Laptop Type")
boxplot(Price_euros ~ Cpu_Group, data = laptop_final, las = 2, main = "Price by CPU")
boxplot(Price_euros ~ Storage_Type, data = laptop_final, las = 2, main = "Price by Storage Type")


# ==============================================================================
# 6. ORDINAL LOGISTIC REGRESSION
# ==============================================================================

# ---------------------------------------------------------
# Create the ordinal response variable
# ---------------------------------------------------------
# Find tertile cut-off values
price_cutoffs <- quantile(
  laptop_final$Price_euros,
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)
price_cutoffs

# Create Low, Medium and High price groups
laptop_final$Price_Group <- cut(
  laptop_final$Price_euros,
  breaks = price_cutoffs,
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High"),
  ordered_result = TRUE
)

# Check the number of laptops in each price group
table(laptop_final$Price_Group)

# Check that Price_Group is an ordered factor
str(laptop_final$Price_Group)
levels(laptop_final$Price_Group)

# ---------------------------------------------------------
# Remove unused factor levels
# ---------------------------------------------------------
laptop_final <- droplevels(laptop_final)

# ---------------------------------------------------------
# Fit the ordinal logistic regression model
# ---------------------------------------------------------

ordinal_null <- MASS::polr(
  Price_Group ~ 1,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)

# backward selection
ord_1 <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen +
    IPS,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)

drop1(ord_1 , test = "Chisq") # IPS
ord_2 <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)
anova(ordinal_null,ord_2) # p_val = 0, ord_2 fits well
drop1(ord_2 , test = "Chisq")  # weight_kg

ord_3 <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group +
    Touchscreen,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)
anova(ordinal_null,ord_3)# p_val = 0, ord_3 fits well
drop1(ord_3 , test = "Chisq") # Touchscreen

ord_4 <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Total_Storage_GB +
    Resolution_Group ,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)
anova(ordinal_null,ord_4)
drop1(ord_4 , test = "Chisq") #Total_Storage_GB

ord_5 <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group ,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)

anova(ordinal_null,ord_5)
drop1(ord_5 , test = "Chisq") 

ordinal_model <- MASS::polr(
  Price_Group ~
    Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group ,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)

# ---------------------------------------------------------
# Display model results
# ---------------------------------------------------------
summary(ordinal_model)

# ---------------------------------------------------------
# Calculate p-values
# ---------------------------------------------------------
ordinal_coef_table <- coef(summary(ordinal_model))

ordinal_p_values <- 2 * pnorm(
  abs(ordinal_coef_table[, "t value"]),
  lower.tail = FALSE
)

ordinal_results <- cbind(
  ordinal_coef_table,
  "p value" = ordinal_p_values
)
ordinal_results # p_values are all less than 0.05

# ---------------------------------------------------------
# Odds ratios
# ---------------------------------------------------------
ordinal_odds_ratios <- exp(coef(ordinal_model))
ordinal_odds_ratios

# ---------------------------------------------------------
# Confidence intervals for odds ratios
# ---------------------------------------------------------
# Wald confidence intervals are quicker and easier
ordinal_confint <- confint.default(ordinal_model)

ordinal_OR_CI <- exp(
  cbind(
    Odds_Ratio = coef(ordinal_model),
    ordinal_confint
  )
)

colnames(ordinal_OR_CI) <- c(
  "Odds Ratio",
  "2.5%",
  "97.5%"
)
ordinal_OR_CI

# ---------------------------------------------------------
# Model Adequacy Checking
# ---------------------------------------------------------

#LR Test
anova(ordinal_null,ordinal_model)

#Brant Test
brant(ordinal_model)

# ---------------------------------------------------------
# Create one complete results table
# ---------------------------------------------------------
predictor_results <- ordinal_results[!grepl("\\|", rownames(ordinal_results)), ]

final_ordinal_table <- cbind(
  predictor_results,
  "Odds Ratio"   = exp(predictor_results[, "Value"]),
  "Lower 95% CI" = exp(predictor_results[, "Value"] - 1.96 * predictor_results[, "Std. Error"]),
  "Upper 95% CI" = exp(predictor_results[, "Value"] + 1.96 * predictor_results[, "Std. Error"])
)
final_ordinal_table

# ---------------------------------------------------------
# Display threshold values
# ---------------------------------------------------------
ordinal_model$zeta

# ---------------------------------------------------------
# Predicted price groups
# ---------------------------------------------------------
predicted_price_group <- predict(
  ordinal_model,
  newdata = laptop_final,
  type = "class"
)
head(predicted_price_group)

# ---------------------------------------------------------
# Confusion matrix
# ---------------------------------------------------------
ordinal_confusion_matrix <- table(
  Actual = laptop_final$Price_Group,
  Predicted = predicted_price_group
)
ordinal_confusion_matrix

# ---------------------------------------------------------
# Overall classification accuracy
# ---------------------------------------------------------
ordinal_accuracy <- mean(
  as.character(predicted_price_group) == as.character(laptop_final$Price_Group)
)
ordinal_accuracy

ordinal_accuracy_percent <- ordinal_accuracy * 100
ordinal_accuracy_percent

# 1. Recreate your confusion matrix
cm <- matrix(c(372,  63,   2,
               52, 315,  65,
               0,  86, 348), 
             nrow = 3, byrow = TRUE)

# Optional: Label the dimensions for clarity
colnames(cm) <- c("Low", "Medium", "High")
rownames(cm) <- c("Low", "Medium", "High")

# 2. Generate the matrix of absolute differences
# row(cm) and col(cm) create matrices matching cm's dimensions filled with index values
abs_diff_matrix <- abs(row(cm) - col(cm))

# 3. Calculate total absolute error and divide by sample size
total_abs_error <- sum(cm * abs_diff_matrix) # Element-wise multiplication
total_count     <- sum(cm)
mae             <- total_abs_error / total_count

# Print results
cat("Total Observations:", total_count, "\n")
cat("Total Absolute Distance Error:", total_abs_error, "\n")
cat("Mean Absolute Error (MAE):", round(mae, 4), "\n")
# ---------------------------------------------------------
# Predicted probabilities
# ---------------------------------------------------------
predicted_probabilities <- predict(
  ordinal_model,
  newdata = laptop_final,
  type = "probs"
)
head(predicted_probabilities)

# ---------------------------------------------------------
# Add predicted results to the dataset
# ---------------------------------------------------------
laptop_final$Predicted_Price_Group <- predicted_price_group
laptop_final$Probability_Low       <- predicted_probabilities[, "Low"]
laptop_final$Probability_Medium    <- predicted_probabilities[, "Medium"]
laptop_final$Probability_High      <- predicted_probabilities[, "High"]

# View actual and predicted results
head(
  laptop_final[, c(
    "Price_euros",
    "Price_Group",
    "Predicted_Price_Group",
    "Probability_Low",
    "Probability_Medium",
    "Probability_High"
  )]
)

# ---------------------------------------------------------
# Compare the ordinal model with the null model
# ---------------------------------------------------------
ordinal_null <- MASS::polr(
  Price_Group ~ 1,
  data = laptop_final,
  Hess = TRUE,
  method = "logistic"
)

anova(
  ordinal_null,
  ordinal_model
)

# ---------------------------------------------------------
# AIC of the final ordinal model
# ---------------------------------------------------------
AIC(ordinal_model)

# ---------------------------------------------------------
# Check the design matrix rank
# ---------------------------------------------------------
ordinal_X <- model.matrix(
  ~ Company_Group +
    TypeName +
    Inches +
    Ram_GB +
    Weight_kg +
    Cpu_Group +
    Gpu_Group +
    Storage_Type +
    Resolution_Group,
  data = laptop_final
)

ncol(ordinal_X)
qr(ordinal_X)$rank
ncol(ordinal_X) == qr(ordinal_X)$rank

detach(laptop_price)