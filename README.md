
# rSCHNEL

rSCHNEL is an R package implementing the SCHNEL algorithm.

## Installation

In order to ensure the successful installation of the package, download two libraries that the wrapped C++ HSNE dimensionality reduction implementation requires. 
In the terminal, run:
```
$ sudo apt-get update
$ sudo apt-get install liblz4-dev
$ sudo apt-get install libflann-dev
```

To support the use of the clustering algorithm, please install python modules for the leiden algorithm.
In case of any problems refer to the official website [here](https://cran.r-project.org/web/packages/leiden/vignettes/run_leiden.html).

Install the required packages with:
- `pip install scipy` / `pip3 install scipy`
- `pip install python-igraph` / `pip3 install python-graph`
- `pip install leidenalg` / `pip3 install leidenalg`

To download the rSCHNEL package, first install `devtools` utilities package
```
install.packages("devtools")
```

Then, download the package from GitHub.
```
devtools::install_github("biovault/SCHNELr")
```

Followed by:
```
library(rschnel)
```

Now download the python source file which is [here](https://github.com/biovault/SCHNELr/blob/parser_in_python/py/src_clustering_HSNE_parser.py). 
And finally:
```
reticulate::source_python(__path_to_python_source_file__)
```

The vignette `installation_guide` includes the description of packages required to support different input formats. 

## Example
An example of clustering a dataset with rSCHNEL is described in the projects vignette 