# LINTUL Cassava

This is the repo for the LINTULcassava *R* package. 

This package imlements the LINTUL-CASSAVA crop growth simulation model that was developed by 

Ezui, K.S., P.A. Leffelaar, A.C. Franke, A. Mando & K.E. Giller (2018). Simulating drought impact and mitigation in cassava using the LINTUL model. Field Crops Research 219: 256-272. 
[doi:10.1016/j.fcr.2018.01.033](https://doi.org/10.1016/j.fcr.2018.01.033).

The model was calibrated with data from three locations in Nigeria by

Adiele, J.G., A.G.T. Schut, R.P.M. van den Beuken, K.S. Ezui, P. Pypers, A.O. Ano, C.N. Egesi & K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124:126242. [doi:10.1016/j.eja.2021.126242](https://doi.org/10.1016/j.eja.2021.126242).

And expanded for nutrient-limiation by 

Adiele, J.G., Schut, A.G.T., Ezui, K.S., & Giller, K.E. (2022). LINTUL-Cassava-NPK: A simulation model for nutrient-limited cassava growth. Field Crops Research, 281, 108488

The orginal *R* implementation by Rob van den Beuken was a translation from the FST implementation of the model developed by Guillaume Ezui under supervision of Peter Leffelaar. Joy Adiele calibrated the model for Nigerian conditions using cultivar 'TME 419'. Tom Schut checked, simplified and adapted the code which is available as R scripts [here]({https://models.pps.wur.nl/index.php/lintulcassavanpk) (2024-10-21).

Robert Hijmans used these scripts to create the *R* package. He added an alternative *R* implementation of the model, as well as a C++ implementation that is more than 1000 times faster than the original *R* implementation.

You can install the package with:

```
install.packages('LINTULcassava', repos = c('https://cropmodels.r-universe.dev'))
```

<a href="https://www.iita.org/" target="_blank">
<img width="183" height="78" alt="IITA-TAA-smallnew" src="https://github.com/user-attachments/assets/03892fd1-2ea4-4fc8-a540-44e79b680d00" />
</a>
