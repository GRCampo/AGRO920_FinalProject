This project was developed as part of the AGRO 920 – Predictive Modeling for Plant Breeding course at Kansas State University. The goal wasn’t to reinvent the wheel, but to show that I can work with complex biological data and apply machine learning models in a meaningful way. In short: build a working pipeline, survive the process, and (hopefully) get useful results.

The project combines plant genomics, transcriptomics, and predictive modeling to find patterns in RNA-seq data related to abiotic stress in wheat (*Triticum aestivum*). Instead of using a reference genome, I followed a reference free approach: breaking raw reads into k-mers and using them directly as input for machine learning models. It’s a bit unconventional, but sometimes going around the map is faster than sticking to the official roads.

All scripts, data processing steps, and model evaluations are clearly documented — partly for reproducibility, and partly so future-me doesn’t hate past-me when trying to understand what I did.

The raw data and intermediate files are stored in the **AGRO920 folder of my Beocat user directory**. If you'd like access, feel free to ask, I’ll gladly give permission. The `results` and `scripts` folders are also available for anyone interested in the pipeline or plots without digging through a cluster.
