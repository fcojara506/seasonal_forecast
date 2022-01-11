# River flow seasonal forecast
Last version of the seasonal spring-summer forecast (sep-mar) for 6 catchments in Chile. 
Using pre-processed ERA5 as meteorological driver within GR6J+Cemaneige hydrological model.
The reference version considers 2 types of predictors:
hydrological variables (snow, soil storages) and meteorological variables (precipitation, temperature)

For each year, we performed a Leave-one-out cross validation using multiple linear regressions.

To distribute the seasonal volume into monthly flows, we used kNN classification model to find the closest years to the current conditions.

The files 1.6 and 1.8 are designed to visualise in ggplot the permormance of the model from 1988-2021 and a prediction for the next months.

