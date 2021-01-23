# MNO Eurostat

## Description

This repository serves as a reseearch repository used for the Eurostat Summer Coding Labs - MNO Project 2020. MNO stands for Mobile Network Operating Data. The main goal is to recreate, test and gain new insights about current estimation methods for **estimating present population based on MNO data**. To do this, we have developed our own toy world (simulation framework) with multiple parameters to exactly measure estimation error. The structure of this project as well as this research repository is designed in a modular way, in order to increase future flexibility. Currently, we use as a basis for geographic data and population numbers, a small part of South East Germany, which is based on Census Data. The modular structure allows that if new data is available or if one is interested in a different part of the world, one only needs to change the data source at the beginning.

The authors of this repository include: Giulia Regazzoni (UniBg, IT), Marco Ramljak* (UU, NL), Stefania Scrofani (UniBg, IT), and Tony Hung (VŠE, CZ).

## Important links

HTML version of our working notebook*(currently under development)*: https://r-ramljak.github.io/MNO_Eurostat/

For a full overview of our work, please visit our book *(currently under development)*: https://bookdown.org/tonyhung_th/bookdown/

This link provides more information regarding the topic of the MNO Project (p8-9): http://mse.ec.unipi.it/wp-content/uploads/2020/06/Eurostat-coding-lab_call_final-040620.pdf 

## Reproducibility

As this is a simulation study, no data needs to be protected and everything can be shared upon request. Currently, the only *real* data source used, which was not simulated, is the population generation which is based on German Census Data. Result objects based on the current workflow with the specified parameters used in our notebook can be provided upon request and therefore fully reproduced. As some of the modeled results objects are very large (>5gb each) they are currently uploaded in the student version of our Microsoft One drive account. As these clouds are university bound it is difficult to share them with non-university members, therefore, if objects are needed, they will be sent via compressed files.

To reproduce our results, run the numbered R-scripts, which also resemble the modules in our modular design, in [code](https://github.com/R-ramljak/MNO_Eurostat/tree/master/code) in corresponding order. Scripts that contain results of random nature (random variable, etc.) have a `set.seed()` command implemented. If the seed is changed, results will most likely differ.

\* corresponding author
