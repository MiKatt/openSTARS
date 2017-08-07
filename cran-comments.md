---
title: "cran-comments.md"
author: "Mira Kattwinkel"
date: "August 2, 2017"
output: html_document
---

## Test environments
* Linux Mint 18.2 Sonya, R 3.4.1
* Windows 7, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE (on win-builder):
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mira Kattwinkel <mira.kattwinkel@gmx.net>'

This is a stable e-mail address and the spam filter set to let through e-mails 
from the domain r-project.org.

Notes on 'no visible binding for global variable' for (fixed) data.table column
names are prevented by including them as utils::globalVariables.

Some examples take quite a while to run (> 5s) because they all contain the full
workflow of loading and processing GIS files to be stand-alone examples.

## Downstream dependencies
There are no downstream dependencies as this is the first release of the package.