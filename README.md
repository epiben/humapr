With `humapr` we seek to provide a simple tool to visualise human topographic data as choropleths. humapr is still simple but fully functional, and handles both tidy data (one row = one observation) and aggregated data (one row = one summary statistic, e.g. mean or sum). 

The package originated in the area of forensic medicine and injury research, but we hope to get feedback from (potential) users to extend its functionalities and make it a useful resource in other disciplines as well. Thus, we invite you to go ahead and put it to the test and are very keen to get feedback and bug reports from useRs. If you have ideas for other geoms, feel free to throw us an email or added it under issues here on the GitHub page. 

`humapr` isn't on CRAN, so you need to run `devtools::install_github("benskov/humapr")` to install it. The introductory vignette has more examples of how to use `humapr`, but we include here a simple example of a (simulated) two-way tabulation of number of lesions by type of force and sex:

<p align="center"><img src="https://github.com/benskov/humapr/tree/master/public_figures/example_grid_trauma_gender.png"></p>

### Funding
We started developing `humapr` a while ago and didn't receive funding directly for this project. However, the following entities have provided funding indirectly to its creation: Institute of Forensic Medicine, Aarhus University; NNF Centre for Protein Research (NNF14CC0001); Innovation Fund Denmark (5153-00002B).
