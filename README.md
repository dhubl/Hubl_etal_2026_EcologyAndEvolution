## _A small carnivore with a constitutive defense, the western spotted skunk_ (<ins>Spilogale</ins> <ins>gracilis</ins>)_, shows no spatial or temporal avoidance of two larger guild members_

#### Dylan J. Hubl, Sarah J. Converse, Patti J. Happe, Betsy L. Howell, Kurt J. Jenkins, Robert Long, Paula MacKay, and Aaron J. Wirsing

#### Please contact the first author for questions about the code or data: Dylan Hubl (hubldylanATgmail.com)
-----------------------------------------------------------------------------------------------------------------------
## Abstract
Interference competition among carnivores ranges in intensity from the threat of attack to interspecific killing (IK). Carnivores are at the greatest risk of becoming victims of IK if they are 2–4 times smaller than their potential attacker or have overlapping diets. Thus, spatial distributions and activity patterns of small carnivores may be dictated by the presence of larger competitors. However, it has been hypothesized that constitutively defended species, such as skunks, face reduced risk of mortality during encounters with predators, and thus, do not respond to the threat of IK through spatial or temporal avoidance. To explore this hypothesis, we used multispecies occupancy models to test for pairwise interaction effects on the space use of western spotted skunks (_Spilogale gracilis_), coyotes (_Canis latrans_), and bobcats (_Lynx rufus_) on the Olympic Peninsula of Washington, USA. We also used timestamp data from camera detections to analyze the temporal overlap of daily activity patterns of all three species. We found no evidence of spatial or temporal avoidance among the three focal species, with the best supported occupancy model indicating that these species use space independently of one another. We also observed high levels of interspecific temporal overlap among all three species. These findings support our hypothesis, suggesting that the constitutive defense of western spotted skunks prevents IK at a great enough rate to obviate the need to avoid potential attackers. The decision to engage in IK is influenced by many risk and reward factors and constitutive defenses are likely a strong deterrent.
____________________
## Table of Contents
Folders include data, scripts, and figures. See files listed below.

## Data
### Processed Data
site_stations_with_detection_covariates.csv

Detection_event_times.csv
### Covariate Data
Elevation_sites_m.csv

Length_of_edge_m.csv

NLDC_Agri_cells_bySite.csv

NLDC_Developed_cells_bySite.csv

PRISM_sites_mm.csv

Road_cells_bySite.csv

Site_Buffer_Areas.csv

Tree_Density.csv

## Scripts
covariate_and_caphist_prep.R

Goodness_of_Fit.R

model_estimates_use_detection_probabilities.R

model_selection_and_figures.R

NIMBLE_null_interactions.R

NIMBLE_flat_interactions.R

NIMBLE_full_interactions.R

Temporal_Overlap.R

## Figures
Figure1.png

Figure2.png

Figure3.png

Figure4.png

## Required Packages
nimble (v1.4.2)

MCMCvis (v0.16.3)

coda (v0.19.4.1)

tidyr (v1.3.1)

lubridate (v1.9.4)

circular (v0.5.0)

ggplot2 (v3.5.1)

overlap (v0.3.9)
## How to use this Repository
The script "covariate and caphist prep.R" must be run prior to any other scripts. This script sets up the sample units sites and arranges all survey and site covariates in the proper order to match sample unit sites and surveys. The script requires all '.csv' files describing site covariates and "site stations with detection covariates.csv".
