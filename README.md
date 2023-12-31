# Python Fortran Bridges for SuperdropNet



[![DOI](https://zenodo.org/badge/687300663.svg)](https://zenodo.org/badge/latestdoi/687300663)



Coupling code for integrating SuperdropNet in ICON 2.6.5

Developed at Helmholtz AI, Hereon and German Climate Computing Center DKRZ, 2022-2023.

Developers:
- Caroline Arnold (arnold@dkrz.de) [@crlna16] (https://www.github.com/crlna16)
- Shivani Sharma (shivani.sharma@hereon.de) [@shivanigauniyal] (https://www.github.com/shivanigauniyal)
- Tobias Weigel (weigel@dkrz.de) [@TobiasWeigel] (https://www.github.com/TobiasWeigel)

Contents:
- cffi_interface
  - Code for coupling with embedded Python
  - Code for SuperdropNet
- pipes_interface
  - Code for coupling with pipes 
- yac_interface
  - Code for coupling with YAC
- icon_runscripts: Experiment definitions
  - exp.warm_bubble_fortran
  - exp.warm_bubble_cffi
  - exp.warm_bubble_pipes
  - exp.warm_bubble_yac
  - exp.cold_bubble_fortran
  - exp.cold_bubble_cffi
- fortran_coupling:
  - fortran side of coupling
  - pseudo code for integrating into ESM
- notebooks:
  - Analysis.ipynb: General analysis notebook to compare and analyze experiments
  - PaperPlots.ipynb: Contains scripts to reproduce the paper plots
  - GPU_Inference.ipynb: Measure SuperdropNet GPU inference time
  - Performance.ipynb: Measure SuperdropNet CPU inference time
