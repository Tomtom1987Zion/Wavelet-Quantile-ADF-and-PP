Wavelet Quantile ADF and PP
To run the WQADF and WQPP test, first ensure that all required R packages (readxl, urca, parallel, waveslim, ggplot2, and ggthemes) are installed and loaded. 
Set the working directory to the location of your dataset and load the Excel file DATA.xlsx. 
The script decomposes each variable into different time horizons (short-term, medium-term, and long-term) using wavelet multi-resolution analysis (MRA). 
Applies the Quantile ADF and PP test across quantiles ranging from 0.05 to 0.95. 
The results for variables LEG, LFD, LIQ, LREC, and LTI are combined into a single dataframe and visualized using ggplot2
