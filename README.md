# bayesian-index-for-tackle-efficiency
This repository contains all working material pertinent to the BITE: Bayesian Index for Tackle Efficiency [notebook](https://www.kaggle.com/code/brunoscodari/bite-bayesian-index-for-tackle-efficiency), which was submitted to the 2024 NFL Big Data Bowl [competition](https://www.kaggle.com/competitions/nfl-big-data-bowl-2024). 

## Reproducibility
For those seeking to reproduce our work, we recommend the following: 
- Install the specific version of `R` and required packages outlined in `requirements.txt`
- Clone a remote copy of the repository to your local machine and navigate to the root directory:

```
git clone https://github.com/bscod27/bayesian-index-for-tackle-efficiency.git
cd bayesian-index-for-tackle-efficiency.git
``` 

- Download the files from the `data` folder (pulled directly from the competition website)
- Execute the scripts detailed below

## Scripts
The following scripts were used in this project: 
1. `convert_mov.py` - converts `data/example_video.mov` to `results/example_play.gif` 
2. `analysis.R` - operates on the information in `data` and produces all other material found in `results`

## Folders
Data, visuals, and other pertinent material are found inside the following folders:
- `data` - contains competition data and the example video we used to produce our animation
- `results` - contains all of the findings shown in the competition notebook
- `scripts` - contains the code used to produce all results
