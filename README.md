#### Analysis of Colony Images
_For understanding the biophysics of subclone dissemination in lung cancer_

This is a combination of projects that allow analysis of colonies of tumor cells bearing fluorescent subclones. All information derived can be accessed through this folder.

##### Project Organization

    ./analysis/
    ├── jamming-and-size-distribution
    │   ├── data
    │   ├── environments
    │   ├── reports
    │   │   └── figures
    │   ├── scheduling
    │   │   ├── errors
    │   │   └── reports
    │   └── src
    ├── mixing-analysis
    │   ├── data
    │   ├── environments
    │   ├── reports
    │   │   └── figures
    │   ├── scheduling
    │   │   ├── errors
    │   │   └── reports
    │   └── src
    └── therapy-resistant-subclones
        ├── data
        ├── environments
        ├── reports
        │   └── figures
        ├── scheduling
        │   ├── errors
        │   └── reports
        └── src
            ├── FIJI-scripts
            ├── R-analysis-scripts
            ├── R-plotting
            ├── R-post-processing
            ├── R-tools
            ├── bash-tools
            └── python-tools
    ./shared-assets/
    ├── image-pairs
    ├── processed-acquisitions
    ├── r-pipeline-outputs
    └── raw-acquisitions


##### To Do List
_20/04/20_

1. Spatial distribution of all subclones in the following manner:
    * Subclone closest point to the boundary is to be noted and then plotted as a function of position from boundary. The idea is to describe the spatial positioning in the presence and absence of mixing.

_06/01/20_

1. Completed Size relationship, now need to process day 15/18 data to get recombination potential and a short graph for the distance between subclones and the boundary. Then move on to movie data. I believe that the PIV data should be skipped unless more control movies are found.
