# Neurosci-Conscious-2025
## Overview
This repository contains the analysis code for the manuscript "Brain-Body Interactions Associate with the Transition from Mind Wandering to Awareness of its Occurrence."

This study investigates the neurophysiological dynamics associated with the transition from mind-wandering (MW) to the awareness of its occurrence. We analyzed electroencephalography (EEG), electrocardiography (ECG), and respiration data to explore how brain-body interactions contribute to this shift in conscious experience.

## Repository Structure
```
scripts/
│   ├── EEG_preprocess.m
│   ├── Time-frequency_comp.m
│   ├── HEP_permutation.m
│   ├── time-series_analysis.stan
│   ├── LMM_analysis.r
│   ├── plot_HEP.r
│   └── Figure_generations.r
│
└── README.md           
```

## Requirements
The analyses were performed using MATLAB and R.

### Software
* **MATLAB** (R2023b or later recommended)
    * **EEGLAB** toolbox (v2023.1 or later)
    * **FieldTrip** toolbox
    * **EEGLAB Plugins & Extensions**

* **R** (version 4.3.1 or later recommended)
    * `lme4`, `lmerTest`, `emmeans`, `effectsize`: For LMM analysis.
    * `rstan`: For running the time-series model.
    * `ggplot2`, `dplyr`, `tidyr`, `data.table`, etc.: For data manipulation and plotting (see the header of each R script for details).
