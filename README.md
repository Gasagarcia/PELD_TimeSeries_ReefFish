# PELD-ILOC Reef fish time series analysis

### Project purpose

Share the workflow used to analyze the reef fish monitoring data
gathered by PELD-ILOC and associated researchers in four Brazilian
Oceanic Islands.

### About PELD-ILOC

<div>

[![](Figures/PELDILOC.svg)](https://peldiloc.sites.ufsc.br/pt/)

</div>

PELD (Programa de Pesquisas Ecológicas de Longa Duração) is a Brazilian
government funded initiative to install long term monitoring programs
across Brazilian terrestrial and aquatic ecosystems. PELD - ILOC is a
project within the PELD initiative started to monitor marine organisms
in Brazilian oceanic islands (ILOC from portuguese for Oceanic Islands).

### About the time series analysis

This project shares code and data used to describe reef fish data
collected by the PELD-ILOC project since 2013. A few more years between
2007 and 2012 were also included by gathering data sampled by associated
researchers. The dataset used here is currently under possession of the
ReefSyn working group and is available in the DarwinCore format at the
GBIF repository: https://doi.org/10.25607/rov4or.

Here I use an old, discontinued data structure for to compatibility
issues. I strongly discourage using this version as it (1) does not
account for taxonomic updates and (2) was not curated for potential
human errors. The scripts include some corrections and taxonomic
updates, but changes are restricted to species subset that fit our
abundance, persistence and minimal sampling effort criteria.

### Directories

I used three folders to organize data through all scripts: Data, Scripts
and R_Objects.  
Data folder contains files for fish abundance and traits. Scripts
contains all R scripts used in here and R_Objects contain progress
saving .rds files used through the scripts to avoid repeating
computationally demanding code chunks.

### Scripts

All analysis were written in R:

1.  01.edit_entries.R  
    Used to perform taxonomic updating, species subseting and generate
    biomass estimates using each species abundance and size.

2.  02.communities.R  
    Contains all descriptive and inferential multivariate analysis
    performed at the assemblage level.

3.  03.dynamics.R  
    Apply [CoDyn](https://github.com/NCEAS/codyn) metrics to describe
    reef fish data in a temporal perspective.

4.  04.groups.R  
    Uses mixed linear models to estimate biomass of reef fish groups
    through time.

5.  05.Species.R  
    Uses mixed linear models to estimate each reef fish species biomass
    through time

### Data

Data folder contains 3 files: The time series
(Final_data_peld_2019_11_18.csv), a currently no longer used copy of the
Atlantic reef fish traits database
(Fish_aspects_EasternPacific_Atlantic_Realms.csv) and a file containing
traits (traits_210613.ods).

### Code authors

Gabriel Garcia and Murilo Dias

### Contributors

Guilherme Longo, Carlos Eduardo Ferreira, Sérgio Floeter, Ronaldo
Francini-Filho
