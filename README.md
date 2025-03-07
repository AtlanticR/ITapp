# Introductions and Transfers Tool

This tool is designed to facilitate the process of reviewing applications for introductions and transfers of live aquaculture stock. 

Currently, the app is operational for DFO Maritimes and Gulf region and will generate suggested responses based on the species being transferred (intended species or possible hitchhikers) as well as the origin and destination sites.

# Installation

- Users must first install "R" and "R Studio" (on DFO computers, you can do that from the Software Center)
- [Download](https://github.com/remi-daigle/ITapp/archive/master.zip) and unzip this repository or otherwise clone the code onto your computer.
- RStudio based usage: Open `ITapp.Rproj` in RStudio, open the `app.R` file, then click on the `Run App` button
- Shortcut based usage:  Create a shortcut to `ITapp.bat` in a convenient location (e.g. your desktop), double click on your shorcut. This method assumes you have R v.4.1.1 installed in a specific location (e.g. `C:\Program Files\R\R-4.1.1\bin\R.exe`)

The first time you run it, it will take a **very** long time to install, but this is a one time thing. This is necessary to avoid including sensitive data in this repository. Feel free to create a shortcut to the `ITapp.bat`file on your desktop.

# Operations

- Ensure that preferences are set correctly in the 'Settings' tab. The user can set the threshold year for which to ignore older biofouling monitoring records or incidental observation records. The user can also set the number of consecutive non-detections from the biofouling monitoring required to consider an AIS as "failed to establish". Currently, a failure to establish **does not** affect the tool's recommended response, but the invasion history information is provided in Table 3 in the "Supportive Tables" section of the Summary tab.
- On the Origin tab, select the origin site's province and lease identifier. By default, after lease selection, the app will display the 3 nearest biofouling monitoring sites, and the 5 nearest incidental observations, but it will only select the nearest biofouling monitoring site for consideration in the Summary tab. The user may want to increase the number of visible sites or select additional sites. Only sites which are selected will be considered in the Summary tab.
- Repeat the above for the destination site on the Destination tab.
- On the Summary tab, select the "Applicable Product Description" (i.e. the product being moved intentionally), and select relevant "Other Considerations"
- Review the Supportive Tables and Suggested Response on the Summary Tab. The user can return to the other tabs to modify any of the inputs if desired.
-Due to the multitude of combined AIS at each site and the potential impacts of AIS mitigation treatments to impact product (e.g. oysters), Table 2 now lists all possible mitigation treatments.Assessors using the app must select the treatment option from Table 2. 

#Tabs
-All Introduction & Transfer decisions are recommended to use the biofouling and incidental report. These are data collected and verified by DFO. 
-The Public Reports and eDNA tab can be used to inform decisions when more robust data is unavailable. 

# Troubleshoouting
- Any issues can be posted on the [GitHub](https://github.com/remi-daigle/ITapp/issues) repository

#References
-AIS mitigation treatments were derived from Masse-Beaulne et al. (2025) and only includes options with moderate to high certainty of effectiveness, unless stated otherwise.
-Massé-Beaulne, V., Simard, N., Bernier, R.Y., Pearce, C.M. and Therriault, T.W. 2025. Mitigation Measures to Reduce the Risk of Introduction and Spread of Aquatic Invasive Species through Shellfish and Macroalgal Movements. DFO Can. Sci. Advis. Sec. Res. Doc. 2025/011. viii + 150 p.
-CSAS English Research Document available at: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2025/2025_011-eng.pdf
-All raw data for the Maritimes Biofouling Program can be accessed here: https://open.canada.ca/data/en/dataset/8d87f574-0661-40a0-822f-e9eabc35780d