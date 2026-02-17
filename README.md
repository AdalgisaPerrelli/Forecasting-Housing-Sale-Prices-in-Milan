# Forecasting Housing Sale Prices in Milan

This project focuses on forecasting residential property sale prices in Milan
using statistical learning and data science techniques.

## Overview
The analysis is based on a real estate dataset that underwent extensive **data cleaning,
imputation, and feature engineering**, including variable selection, transformation of
categorical features and construction of engineered predictors to improve model performance.

## Methodology
The training data was split into training and validation sets (75%–25%).
Model performance was evaluated using the **Mean Absolute Error (MAE)**.

The following models were implemented and compared:
- Baseline model (median-based benchmark)
- Ordinary Least Squares (parsimonious and full models)
- Backward regression
- Penalized regression (Ridge, LASSO, Elastic Net)
- Principal Component Regression (PCR)
- Least Angle Regression (LARS)
- Generalized Additive Models (GAM)

## Results
Linear and penalized regression models achieved similar performance, while the **full GAM**
provided the best predictive accuracy on the validation set by capturing both non-linear
effects in continuous variables and complex categorical relationships.

## Files
- `code_Milan-Housing.R`: R script containing data preprocessing, feature engineering,
  model estimation and evaluation
- `report_Milan-Housing.pdf`: Detailed explanation of the methodology and results
- `Training.csv`: Training data
- `Test.csv`: Test data

## Tools
- R
- Statistical learning and regression modeling

