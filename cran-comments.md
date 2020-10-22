---
title: "cran-comments.md"
author: "Mira Kattwinkel"
date: "Oct 15, 2020"
output: html_document
---

## Submission review comments

## Test environments
* Linux Mint 19.0 Tara, R 3.6.3, grass 7.4
* Linux Mint 19.0 Tara, R 4.0.2, grass 7.8.4
* Windows 10, R 4.0, grass 7.6.1
* win-builder (devel, release, oldrelease)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:
* checking installed package size ...
     installed size is  9.4Mb
     sub-directories of 1Mb or more:
       extdata   9.0Mb

The external data consists of GIS data file (shapes and raster) to enable helpful examples. 

Some examples (within donttest) take quite a while to run (> 5s) because they all contain the full
workflow of loading and processing GIS files to be stand-alone examples.

## Downstream dependencies
There are no downstream dependencies