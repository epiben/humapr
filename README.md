<p align="center"><img src="https://raw.githubusercontent.com/benskov/humapr/master/public_figures/logo_name.png" height="350"></p>

With `humapr` we seek to provide a simple tool to visualise human topographic data as choropleths. It's still simple but fully functional, and handles both tidy data (one row = one observation) and aggregated data (one row = one summary statistic, e.g. mean or sum). 

The package originated in the area of forensic medicine and injury research, but we hope to get feedback from (potential) users to extend its functionalities and make it useful in other disciplines as well. Thus, we invite you to go ahead and put it to the test and are very keen to get feedback and bug reports from useRs. If you have ideas for other geoms, feel free to throw us an email or added it under issues here on the GitHub page. 

`humapr` isn't on CRAN, so you need to run the following to install it:

```
devtools::install_github("benskov/humapr")
``` 

The introductory vignette that comes with `humapr` when installed ([or click here](http://htmlpreview.github.io/?https://github.com/benskov/humapr/blob/master/inst/doc/intro_to_humapr.html)) has more examples of how to use it, but we include here a simple example of a (simulated) two-way tabulation of number of lesions by type of force and sex:

<p align="center"><img src="https://raw.githubusercontent.com/benskov/humapr/master/public_figures/example_grid_trauma_gender.png"></p>



### Funding
We started developing `humapr` a while ago and didn't receive funding directly for this project. However, the following entities have provided funding indirectly to its creation: Institute of Forensic Medicine, Aarhus University; NNF Centre for Protein Research (NNF14CC0001); Innovation Fund Denmark (5153-00002B).
