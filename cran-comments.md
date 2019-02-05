---
title: "cran-comments.md"
author: "Mira Kattwinkel"
date: "Jan 07, 2019"
output: html_document
---

## Submission review comments

## Test environments
* Linux Mint 19.0 Tara, R 3.5.1
* Windows 7, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:
* checking installed package size ...
     installed size is  9.3Mb
     sub-directories of 1Mb or more:
       extdata   9.0Mb

The external data consists of GIS data file (shapes and raster) to enable helpful examples. 

Some examples (within donttest) take quite a while to run (> 5s) because they all contain the full
workflow of loading and processing GIS files to be stand-alone examples.

## Downstream dependencies
All downstream dependencies have been checked.
