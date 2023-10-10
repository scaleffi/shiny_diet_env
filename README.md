# shiny_diet_env
The folder ‘app in a single file’ contains scripts that can run the whole dashboard from a single file. UI, server, and app - the three components of a shiny dashboard - are all contained in the same file. V 2.3 is the latest version, can be launched simply using the Run App button if the paths to the data files are correctly set.

The other files (app.r, loaddata.r, ui.r, server.r) generate the same dashboard as in the Dashboard V 2.3.R file, but the code is split across four scripts for easier debugging and to make it more modular. The dashboard can be launched from the app.R file, which sources the other three as needed. NOTE: the dashboard starts up correctly from the app.R file if the code in it is executed manually, or if the loaddata.R file is manually loaded and then the Run App button is used. For some reason, trying to start the dashboard from the app.file using directly the Run App button leads to an error message which I have not been able to solve. There must be some conflict in how the reactive expression are called, or in the order in which R goes through the different steps, when using the Run App button.

Below is a description of what each version added to the previous one, referring to the files in the ‘app in a single file’ folder.

V 1.0

Working dashboard, with tidy code. The dashboard uses estimates on daily intakes and associated environmental footprints put together by Marco Springmann to produce plots and visualisations that allow users to create several comparisons.

- Environmental footprints + consumption data from cons_compare.

V2.0

- Adjusted colors by food group.

V 2.1

- Added facet-wrapping where appropriate.
- Added value labels in plots where appropriate.

V 2.2

- Changed sidebar menu structure in UI to group consumption data and environmental Footprints data in two separate items.
- Assigned full labels to facet plots where possible/relevant

V 2.3

- Implemented option for user to visualise data in table format, as well as plot, and to download the data as table in .csv (IN PROGRESS, NOT COMPLETED FOR ALL SECTIONS. Completed for sexage and eduurb)
