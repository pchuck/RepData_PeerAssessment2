# reproducible research
#   course project #2
#   patrick charles
#
# makefile to conveniently invoke targets for processing/rendering r markdown
#

RMD=stormdata
DATA=StormData

# fetch the dataset
fetch:
	wget https://d396qusza40orc.cloudfront.net/repdata/data/$(DATA).csv.bz2

# use knitr to convert rmd to html and render
render:
	./rmdToHtml.R $(RMD)

# remove generated files
clean:
	rm -f $(SRC).csv
	rm -f $(RMD).html $(RMD).md 
	rm -rf figure/
	rm -f $(DATA).csv $(DATA).csv.bz2
