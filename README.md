# Context-Diversity

This repository includes scripts and data for the manuscript, When is a Word in Good Company for Learning? published in _Developmental Science_.

The published manuscript can be found here: https://doi.org/10.1111/desc.13510

The goal of the analyses is to disentangle predictors of early word learning: (1) the frequency of words in children's language input, versus (2) the diversity of different linguistic contexts in which words appear in children's language input. 

The analyses implement multiple measures of context diversity that have been developed in prior research, which can be found in the script "Context Diversity - Degree & Divergence.R". 

A key target of these analyses is to capture relationships between variables that are nonlinear but consistently in one direction, e.g., always positive or always negative. The analyses therefore use Generalized Additive Models implemented in the extremely useful scam package in R ( https://doi.org/10.1007%2Fs11222-013-9448-7 ). These analyses are in the script, "Context Diversity - Predict AoA.R".

The scripts have been extensively commented and are intended to be run in this order using the contents of the subfolders:
(1) Context Diversity - Degree & Divergence.R
(2) Context Diversity - Predict AoA.R
(3) Context Diversity - Manuscript Graphs & Tables.R
