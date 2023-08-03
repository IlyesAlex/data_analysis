Folder structure:

- analysis: The location of the scripts for data preprocessing and data analysis.
- data: The location of the raw data in csv and xlsx formats.
- figures: The location of all the figures that were generated from plotting.
- results: The location of processed data that is easily readable and non-redundant. All the analysis pipelines run on these files.


File index:

Main folder:
- analysis.Rproj = The main RStudio project for the data_analysis.
- .RData and all_models.RData = RData files with all the data and GLMEMs read in.

Analysis:
- data_viewer = A shiny app to observe the data of each participants individually in an interactive way.
- smst_main_analysis = The script to analyze all data in the experiment. These were used to report statistics in the paper.
- covariates_analysis = The script to analyze the phrase covariates.
- data_pipeline_main_study = The script to process raw data. It iterates through each file in the 'data' folder, and outputs clean data to the 'results' folder.