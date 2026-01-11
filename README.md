# LINTUL Cassava

This is the repo for the LINTULcassava *R* package. 

The package  imlements a fast version of the LINTUL-CASSAVA crop growth simulation model that was developed by 

Ezui, K.S., P.A. Leffelaar, A.C. Franke, A. Mando, K.E. Giller (2018). Simulating drought impact and mitigation in cassava using the LINTUL model. Field Crops Research 219: 256-272.

The model was calibrated for Nigeria by

Adiele, J.G., A.G.T. Schut, K.S. Ezui, P. Pypers and K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124:126242.

The orginal R implementation was a translation by Rob van den Beuken from the FST version by Guillaume Ezui under supervision of Peter Leffelaar. Joy Adiele calibrated the model to Nigerian conditions. For this, the RUE parameter was set to the measured value and LAI was forced using measured data from trials in Edo, Nigeria, in 2016, to estimate the assimilate partitioning values for cultivar 'TME 419'. Tom Schut checked, simplified and adapted the code. 

Robert Hijmans used these scripts to create the *R* package. He added an alternative R implementation of the model, as well as a C++ implementation that is more than 1000 times faster than the original *R* implementation.

You can install the package with:

```
install.packages('LINTULcassava', repos = c('https://cropmodels.r-universe.dev', 'https://cloud.r-project.org'))
```

