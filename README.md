# Biogeography_datadiv
Global project of assessing biodiversity in French Polynesia

The main purpose of this code is to get filtered data from a GBIF metadata-oriented
set of records (e.g. occurrences), be aware that, because of storage space limitations,
the code can't run without your record file. And some modifications are subject 
to change, as some names have been filtered by hand.

Only external data has been attached to the project at data/external; there are
shapefiles of FP atolls and islands, airports, cities over 1000 inhabitants,
rivers and roads.
Raw and processed data are available at SEANOE db repo, see the linked article :
http://dx.doi.org/10.22541/au.173640768.88486210/v1

I've added a way to read the starting file, which you can download
in ".txt". You can also read.csv or use the rgbif::occ_download() to get 
what you want. (see the arguments of the function)
