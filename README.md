# WOMP (What's On My Plate)

A tool to calculate the macro- and micro-nutrient content in your meal recipes

# How it works

`womp` queries nutrition data from the USDA's FoodData Central (FDC)
[database](https://fdc.nal.usda.gov/). This data is then combined based on a
schedule for the meals you plan to eat to produce a summary of all the nutrients
you will be consuming throughout a given time period.

The granularity of summarized nutrients depends on the specific foods queried
from the FDC, but generally includes:

* energy (calories)
* macros (water, fats, protein, carbs)
* vitamins
* minerals
* amino acids
* lipid composition (DHA, DPA, EPA, etc)
* sugar composition (glucose, fructose, etc)
* fiber
* other organics such as phytosterols, organic acids, cholines, etc

Summary data can be output as plain text, JSON format, or tsv format. The latter
two are designed for downstream processing by other tools if one wants to
calculate additional metrics for their meal plans (ie in R or python).

# Who would want to use this?

Anyone who wants to understand what they are consuming in their diet would
benefit from `womp`. In general, this will be easiest for those who have
predictable meal plans. While `womp` may be used as a "meal tracker," it is
designed to be and likely best leveraged as a "meal planner and calculator".

Currently this is a command-line only application that works on Linux. Porting
to other desktops is planned, as well as the addition of a web-based GUI.

# Usage

TODO

get an apikey and such, type stuff and watch text fly across your screen

# Installation

TODO

# Planned Roadmap

* [ ] port to windows and mac
* [ ] add a web-based GUI
* [ ] add in-app search function for FDC
* [ ] add uncertainty brackets to output
