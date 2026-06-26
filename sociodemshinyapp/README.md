**What**: 
This repository contains code and data to create an interactive Shiny dashboard, currently hosted at https://livedataoxford.shinyapps.io/sociodemshinyapp/. The tool has been designed and developed by Sebastiano Caleffi and Marco Springmann at the Institute for Global Health, University College London.

**Description**:
The dashboard allows users to explore data on food-related environmental footprints in 2020, for 175 countries, across four socio-demographic characteristics (age, sex, education level, urban/rural residency),
and four environmental dimensions (GHG, land use, freshwater use, eutrophication potential). 

The dashboard accompanies a scientific paper that has been submitted for review, and which contains detailed information on the methodology and data used. The paper will be linked to the dashboard once it has been published. After publication, the dashboard will be published online on a freely accessible platform, hosted courtesy of Oxford University.
For any questions or comments, please contact Sebastiano Caleffi at [s.caleffi@ucl.ac.uk](mailto:s.caleffi@ucl.ac.uk) 

**Instructions for off-line use**:

The dashboard is live and hosted at (https://livedataoxford.shinyapps.io/sociodemshinyapp/), but if you wish to work on it offline follow these steps.

1. Ensure you have R and RStudio installed on your environment.
2. Download a copy of the repository onto your local environment.
3. Open the sociodemshinyapp folder in the repository, and then open the sociodemapp.Rproj file.
4. From inside the RStudio environment, click on the 'files' tab to see the list of files contained in the project folder, and click on the app.R file to open it.
5. Once you have opened the app.R file, click on the "run app" button to launch the dashboard; you can also select all the code in the file and run it manually to launch the dashboard.

Please make sure that you are opening the .Rproj file first, and then the app.R file from within the project environment. If you do so, the .Rproj file will take care of establishing the correct
relative paths so that the data files can be loaded without you having to set a working directory pointing to your local environment. If you directly open the app.r script without loading it from the 
Project environment, the dashboard will not work unless you manually set the correct local path to the data files.

You may need to install a few packages the first time you run the app.
