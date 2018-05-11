---
title: "cran-comments.md"
author: "Mira Kattwinkel"
date: "May 11, 2018"
output: html_document
---

## Submission review comments
This is the first submission of this version.

## Test environments
* Linux Mint 18.2 Sonya, R 3.4.1
* Windows 7, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:
* checking examples ... NOTE
Examples with CPU or elapsed time > 5s

Some examples take quite a while to run (> 5s) because they all contain the full
workflow of loading and processing GIS files to be stand-alone examples.

## Downstream dependencies
All downstream dependencies have been checked.
