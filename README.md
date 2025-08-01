This is the code for [DOI to be included]

Getting started:
- For the open data, please download from https://osf.io/829wd/
- The two folders, ./data and ./data_exp1, should be at the same level as the rest of the scripts in this repository
- Bowen Xiao is very happy to be contacted for any questions & to chat about the project (see article for contact details; use cantab.ac.uk if he cannot be reached with @cam)

1. For reproducing the statistics: run write_up_exp1_exp2_splitfigures.Rmd (final publication version, not preprint); this writes into exclu_PSEs.csv;
2. For combining the results from modelling with non-modelling: run analysis_DMprogress_grouplevel.m; this script will write into Exp2_PtpVarTable_full.csv
3. For running the fitting, analysis_fit_DMprogress.m + analysis_DMprogress_BICcomparison(false); these will write into exp2_params.mat and exp2_BICtable.csv
4. power_lme_progress.R is mainly written for simulation-based power calculations for the mixed-effect models on decision-making and ratings. Also has simpler pwr checks at the end, which are also doable via G*Power.
