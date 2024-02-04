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

Summary data can be output as YAML, JSON, or TSV format. The first is meant to
be "human readable" and all are suitable for downstream processing by other
tools if one wants to calculate additional metrics (ie in R or python).

# Who would want to use this?

Anyone who wants to understand what they are consuming in their diet would
benefit from `womp`. In general, this will be easiest for those who have
predictable meal plans. While `womp` may be used as a "meal tracker," it is
designed to be and likely best leveraged as a "meal planner and calculator".

Currently this is a command-line only application that works on Linux. Porting
to other desktops is planned, as well as the addition of a web-based GUI.

# Quick start

First get an API key for USDA's FoodData Central database
[here](https://fdc.nal.usda.gov/api-key-signup.html). Replace `<APIKEY>` in
all examples below with the key they give you.

See `test/exmples` for a starting point to create a meal plan. The `.dhall`
and `.yml` files are equivalent and should produce the same output. Note that
yml does not currently support default values so will be slightly more verbose.

### Examples

Print a TSV list of ingredients, including the header (`-H`) and their masses
for the next week (`-d 7`) starting on Feb 3, 2024 (`-s 2024-02-03`):

```
womp summary -k <APIKEY> -s 2024-02-03 -d 7 -H -c ./test/examples/yogurt.yml
```


Print a list of all nutrients to be consumed for the next week:

```
womp table -k <APIKEY> -s 2024-02-03 -d 7 -I 1 -D -H -c ./test/examples/yogurt.dhall
```

Same as above but print a tree in JSON format:

```
womp tree -k <APIKEY> -s 2024-02-03 -d 7 -I 1 -D -j -c ./test/examples/yogurt.dhall
```

The `-I` flag will specify the aggregation interval in days, and `-D` will 
group all nutrient masses by date range (in this case one day).

The following will print the same as above but grouped by day:

```
womp table -k <APIKEY> -s 2024-02-03 -d 7 -I 1 -D -H -c ./test/examples/yogurt.dhall
```

Print a list of all nutrients for the next week, group by ingredient (`-G`)
and sort by nutrient and value within each ingredient in ascending order (`-S
+nutrient,+value`), then filter for the nutrient you want.

This will rank all nutrients by absolute potassium content:

```
womp table -k <APIKEY> -s 2024-02-03 -d 7 -G -S +nutrient,+value -H \
  -c ./test/examples/yogurt.dhall | \
  grep Potassium
```

Dump the json blob of a given nutrient (in this case a banana) as reported by
FDC:

```
womp dump -k <APIKEY> -i 1105073
```

Simply pre-download a json blob (which will be stored in `XDG_CACHE_HOME/womp`):

```
womp fetch -k <APIKEY> -i 1105073
```

Show all help:

```
womp --help
```

Additional documentation can be found in `dhall/Types.dhall` which provides
an overview of the configuration structure.

# Installation

Clone this repo and build, currently only tested on Linux:

```
git clone <url>
cd womp
stack install
```

# Planned Roadmap

* [ ] add other databases (ie NCCDB)
* [ ] port to windows and mac
* [ ] add in-app search function for FDC
* [ ] add uncertainty brackets to output
* [ ] add a web-based GUI
