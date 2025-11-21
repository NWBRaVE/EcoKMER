# EcoKMER

EcoKMER is a Shiny web application for ecological data analysis and visualization, featuring interactive maps, data processing tools, and Python integration for advanced analytics.

## Key Features:

- **Federated Metadata Exploration**: Query DataFed repositories to harmonize multi-lab metadata, filter interactively, then download only matching files via Globus
- **Spatio-Temporal Visualization**: Interactive maps encode sample collection dates through color gradients, with user selected spatial filtering for identifying geographic and temporal patterns
- **Dual-Mode Operation**: Online mode connects to live DataFed repositories; offline mode enables exploration of local metadata snapshots without network access

## Use Cases:

- **Selective Sample Retrieval**: Identify candidate samples from large-scale metagenomic collections by filtering metadata before downloading the selected sequence files
- **Multi-Site Campaign Analysis**: Visualize spatial distribution and temporal trends across sampling sites, export filtered subsets for targeted analysis
- **Offline Field Exploration**: Work with metadata snapshots at remote field stations, perform exploratory analyses without connectivity, synchronize with online repositories when returning to network access

### Prerequisites

- **R** (version 4.4.2 or later)
- **Git** for cloning the repository
- **Python** (optional, for reticulate and DataFed functionality)

### Installation and Setup

1. **Clone the repository**
   ```bash
   git clone [https://github.com/NWBRaVE/EcoKMER/ecokmer.git
   cd ecokmer
   ```

2. **Open R in the project directory**
   ```bash
   R
   ```

3. **Install and activate renv (if not already installed)**
   ```r
   if (!requireNamespace("renv", quietly = TRUE)) {
     install.packages("renv")
   }
   renv::restore()
   ```
   
   This will install all required R packages listed in the `renv.lock` file.

4. **Run the Shiny app**
   ```r
   shiny::runApp()
   ```

### Package Dependencies

The application uses the following main R packages:
- **Shiny ecosystem**: `shiny`, `shinyWidgets`, `shinydashboard`, `DT`
- **Data processing**: `dplyr`, `readxl`, `purrr`, `stringr`
- **Visualization**: `plotly`, `leaflet`, `ggplot2`
- **Python integration**: `reticulate`

All dependencies are managed via `renv` for reproducible environments.

## Python Environment (Required for DataFed Features)

If you plan to use DataFed / Python-integrated features:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

Start R after activating the environment (or set `RETICULATE_PYTHON` to the venv Python path) so `reticulate` can find the modules. 

The app prints a summary line like:
`[python-env] Python deps: datafed=OK, pandas=OK, tqdm=MISSING, openpyxl=OK (critical: OK)`

## How To:

### Online (DataFed) mode:
1. Launch the app: `shiny::runApp()`
2. Choose "Online" in the data mode selector (home pane).
3. Enter DataFed username & password, then click **Authenticate** (credentials stored only in the running R session env vars `DF_USER`/`DF_PASS`).
4. Click **Fetch Records** – this queries DataFed collections (using the Globus endpoint + context from config) and writes raw CSVs into `./Data/downloads/`.
5. Click **Load / Pull Records** (the load button) to merge those CSVs into the harmonized in‑memory dataset used across tabs.
6. Apply filters on analysis tabs, visualize the data, and optionally **Process Filtered Records to Download** followed by **Download from DataFed** to retrieve filtered artifacts.

### Offline mode:
1. Choose "Offline" mode.
2. (Optional) Browse and select a local harmonized CSV; if you skip this the app falls back to `Data/harmonized.csv`.
3. Use analysis tabs (map, plots, tables) exactly as in online mode (no DataFed interaction).

### Configuration
The Datafed configuration lives in: `www/DataFedSettings.cfg`

See `DEV_NOTES.md` for more details. 

## Contributing

For people who want to make changes to this project:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test that the Shiny app still runs: `shiny::runApp()`
5. Update `renv.lock` if you add new packages: `renv::snapshot()`
6. Submit a pull request


## Testing

One-liner (runs all tests):
```bash
R -q -e 'testthat::test_dir("tests/testthat")'
```

## License

EcoKMER is made freely available under the terms of Simplified BSD license.
Copyright 2025 Battelle Memorial Institute
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This material was prepared as an account of work sponsored by an agency of the United States Government.  Neither the United States Government nor the United States Department of Energy, nor Battelle, nor any of their employees, nor any jurisdiction or organization that has cooperated in the development of these materials, makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness or any information, apparatus, product, software, or process disclosed, or represents that its use would not infringe privately owned rights.
Reference herein to any specific commercial product, process, or service by trade name, trademark, manufacturer, or otherwise does not necessarily constitute or imply its endorsement, recommendation, or favoring by the United States Government or any agency thereof, or Battelle Memorial Institute. The views and opinions of authors expressed herein do not necessarily state or reflect those of the United States Government or any agency thereof.

<div align="center">

PACIFIC NORTHWEST NATIONAL LABORATORY  
operated by  
BATTELLE  
for the  
UNITED STATES DEPARTMENT OF ENERGY  
under Contract DE-AC05-76RL01830  

</div>


## Acknowledgments

This work was supported by the NW-BRaVE for Biopreparedness project funded by the U. S. Department of Energy (DOE), Office of Science, Office of Biological and Environmental Research, under FWP 81832. A portion of this research was performed on a project award (Enhancing biopreparedness through a model system to understand the molecular mechanisms that lead to pathogenesis and disease transmission) from the Environmental Molecular Sciences Laboratory, a DOE Office of Science User Facility sponsored by the Biological and Environmental Research program under Contract No. DE-AC05-76RL01830. Pacific Northwest National Laboratory is a multi-program national laboratory operated by Battelle for the DOE under Contract DE-AC05-76RL01830. A portion of this paper was supported by the University of Colorado School of Medicine.
