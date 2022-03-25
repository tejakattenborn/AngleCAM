# AngleCAM


### Introduction
...to be followed

### Approach
...lots of work

### Examples


![diurnal](https://github.com/tejakattenborn/AngleCAM/blob/main/result_small_mod.gif)
*Animation of the AngleCam input and output. Note that the output (leaf angle distribution) is for simplicity integrated to average leaf angles. The animation shows that during a sunny day Tilia cordata tends to oscilate its leaf angles. The estimates show a relatively high variance, mostly caused by small changes in leaf orientation due to wind. Despite considerable variation in illumination conditions, the predictions show a relatively stable course during the day.*

### Use AngleCam and how to contribute to this project

The trained models are available in ... and come with a minimalistic code. The code requires a running tensorflow and keras implementation in R (see).
The models can be either trained on individual image files in a input directory or from Brinno-AVI-files.
Please contact me if you find any bugs or have problems in getting the models running.

Current evaluations indicate the transferability of the approach across scence conditions, species and plant forms. However, we cannot eventually state how well the models perform on your datasets (which may be composed of very different species, leaf forms or cameras). Additional labels (reference data) may be helpful to tune the model towards you application scenario. A script for producing new reference data can be found here:
We would be very thankful if you would share these samples with us, so we can continuously improve the model performance and transferability. AngleCam is truly in a alpha-phase and it success also depends on your help. Contributors will ofcourse be involved in upcoming analysis and scientific output.
