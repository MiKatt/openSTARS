---
title: "cran-comments.md"
author: "Mira Kattwinkel"
date: "May 11, 2018"
output: html_document
---

## Submission review comments
All comments were taken into account in particular:
"Please explain the acronym DEM in your description."

This is done in the description right before giving the abbreviation; 
now starting with a capital letter:
"A Digital Elevation Model (DEM) is used to derive stream networks"

## Test environments
* Linux Mint 18.2 Sonya, R 3.4.1
* Windows 7, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE (on win-builder):
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mira Kattwinkel <mira.kattwinkel@gmx.net>'

This is a stable e-mail address and the spam filter set to let through e-mails 
from the domain r-project.org.

Some examples take quite a while to run (> 5s) because they all contain the full
workflow of loading and processing GIS files to be stand-alone examples.

## Downstream dependencies
All downstream dependencies have been checked.


