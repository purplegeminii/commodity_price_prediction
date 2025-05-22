# Commodity Price Prediction

This project explores machine learning and deep learning approaches for forecasting commodity prices, including cocoa, crude oil, and gold. It was developed as part of a university capstone thesis.

## Project Structure

- **Jupyter Notebooks**:  
  - `cocoa_lstm_model.ipynb`: LSTM-based modeling and forecasting for cocoa prices  
  - `crudeoil_lstm_model.ipynb`, `gold_lstm_model.ipynb`: LSTM models for crude oil and gold  
  - `automl_ver2.ipynb`, `automl_ver3.ipynb`: Automated machine learning experiments  
  - `data_preprocess.ipynb`: Data cleaning and preprocessing steps  
  - `mcf_lstm_ver1.ipynb`: Additional LSTM experiments

- **R Scripts**:  
  - `Cocoa_Arima.R`, `CrudeOil_Arima.R`, `Gold_Arima.R`: ARIMA time series models for each commodity

- **Data**:  
  - `main_data.csv`, `commodity_historical.csv`, `merged_macro_commodity.csv`, and others: Raw and processed datasets  
  - `monthly_revenue_employment_GDP_data_1991_2024.csv`: Macroeconomic indicators

- **Models & Outputs**:  
  - `autogluon_models/`: Saved AutoGluon models  
  - `saved_models/`: Saved Keras models  
  - PNG files: Forecast vs. actual plots

## Key Features

- LSTM neural networks for time series forecasting
- ARIMA models for baseline comparison
- Automated machine learning with AutoGluon
- Data preprocessing and feature engineering
- Evaluation metrics: RMSE, MAPE, and visualizations

## Requirements

- Python 3.8+
- Jupyter Notebook
- TensorFlow / Keras
- pandas, numpy, matplotlib, scikit-learn, plotly
- R (for ARIMA scripts)
- AutoGluon (for AutoML notebooks)

Install Python dependencies:
```sh
pip install -r requirements.txt
```
