# Oblique Decision Tree and its Boosting and Random Forest
This project is the replication script for "Consistency of Oblique Decision Tree and its Boosting and Random Forest," submitted for consideration for publication in **Bernoulli**. The replication script includes the exact code used to reproduce the calculations presented in the manuscript. The methods proposed in our paper are ODT, ODRF, and ODBT, which are implemented using our R package `ODRF`. For detailed usage, please refer to https://github.com/liuyu-star/.

## Scripts Used in the Manuscript

The instructions for using the R scripts are as follows:

- **`Ensemble_methods_for_regression.R`**: This script corresponds to Table 2 in the manuscript, utilizing Axis-aligned, Oblique, and Boosting methods for regression on 20 datasets with continuous responses, comparing their predictive accuracy.

- **`Ensemble_methods_for_classification.R`**: This is the replication script for Table 3 in the manuscript. It employs the same three ensemble methods to perform classification on 20 datasets with binary categorical responses, comparing the classification error rates of each method.

- **`tree_methods_for_regression.R`**: This is the replication script for Table 4 in the manuscript. It utilizes tree methods to perform regression on 20 datasets with continuous responses, comparing the predictive accuracy and computation time of different tree methods.

- **`tree_methods_for_classification.R`**: This is the replication script for Table 5 in the manuscript. It applies tree methods to perform classification on 20 datasets with binary categorical responses, comparing the classification error rates and computation time of various tree methods.

Additionally, the **`supportingFiles`** folder contains offline installation packages for some methods. All computations were conducted in R version 4.2.2 on a Windows 11 operating system with an AMD Ryzen 7 5800H@3.2GHz. Multi-threading was used for the computations in Tables 2 and 3 of the manuscript, while for Tables 4 and 5, multi-threading was not used to ensure fair comparison of computation times.

## Data Download

You can access the datasets used in the manuscript by downloading the **`Datasets`** folder from [https://github.com/liuyu-star/ODBT](https://github.com/liuyu-star/ODBT). To do this, make sure you are in the current working directory of R, and then proceed with the download. Alternatively, you can also follow our method to retrieve the data automatically without manual downloading.

## Attention

Before running the manuscript's replication scripts, confirm that all required R packages are installed and that the **`Datasets`** folder is placed in the R workspace (`getwd()`).
