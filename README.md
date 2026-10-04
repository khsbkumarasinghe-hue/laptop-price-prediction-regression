# laptop-price-prediction-regression
Statistical modelling of laptop prices using Multiple Linear Regression and Ordinal Logistic Regression in R.
Dataset: Laptop Price Dataset by Muhammet Varlı, obtained from Kaggle.
The dataset contains information on 1,303 laptops, including manufacturer, processor, RAM, storage, GPU, display specifications, weight and price.
# Laptop Price Prediction Using Regression Models

## Overview

This project investigates the factors influencing laptop prices using
statistical modelling techniques.

The analysis was conducted using the Laptop Price Dataset containing
1,303 laptop records and various hardware and software specifications.

## Objectives

- Explore the distribution of laptop prices and specifications
- Identify important factors associated with laptop prices
- Develop a Multiple Linear Regression model for continuous price prediction
- Develop an Ordinal Logistic Regression model for price-category classification
- Evaluate model performance and assumptions
- Interpret the effects of important predictors

## Dataset

The dataset was obtained from the Kaggle Laptop Price Dataset.

It contains information about:

- Manufacturer
- Laptop type
- Screen size
- RAM
- CPU
- GPU
- Storage
- Screen resolution
- Operating system
- Weight
- Price

## Data Preprocessing and Feature Engineering

Several preprocessing and feature engineering steps were performed.

Examples include:

- Converting RAM and Weight into numerical variables
- Grouping detailed CPU categories
- Grouping GPU categories
- Grouping operating systems
- Separating storage type and storage capacity
- Extracting screen resolution characteristics
- Creating touchscreen and IPS indicators
- Creating broader manufacturer groups

## Statistical Models

### Multiple Linear Regression

The continuous laptop price was modelled using multiple linear regression
after appropriate transformation.

### Ordinal Logistic Regression

Laptop prices were categorized into:

- Low
- Medium
- High

An ordinal logistic regression framework was then used to model the ordered
price categories.

## Key Findings

The analysis identified several important factors associated with laptop
prices, including:

- RAM
- CPU
- GPU
- Storage type
- Screen resolution
- Manufacturer
- Screen size

The Multiple Linear Regression model explained approximately 82.8% of the
variation in log-transformed laptop prices.

The Ordinal Logistic Regression model achieved approximately 78% classification
accuracy across the three price categories.

## Repository Contents

- `Laptop_Price_Analysis.R` – Complete R analysis code
- `Report/` – Project report
- `Presentation/` – Project presentation

## Tools

- R
- RStudio
- dplyr
- ggplot2
- car
- Statistical modelling

## Project Type

University Statistical Modelling Project
