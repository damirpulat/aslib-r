#!/bin/bash

for dir in */; do
	cd $dir;
	#remove last two lines
	head -n -2 algos.html > algos-add.html;

	#add interative graph
	echo "<b2>" >> algos-add.html;
	echo "<h2> Interactive algorithm runs and runstatus </h2>" >> algos-add.html;
	echo "" >> algos-add.html;
	echo "</pre></div>" >> algos-add.html;
	echo "  </div><div class=\"rimage default\">" >> algos-add.html;
	echo "  <body style=\"margin: 0 auto;\">" >> algos-add.html;
	echo "      <iframe src=\"https://www.cs.uwyo.edu/~dpulatov/aslib-interactive/${dir%/}_runs.html\"" >> algos-add.html;
	echo "           frameborder=\"0\"" >> algos-add.html;
	echo "            style=\"display:block; position: absolute; height: 100%; width: 100%\">" >> algos-add.html;
	echo " " >> algos-add.html;
	echo " </iframe></div></div>" >> algos-add.html;
	echo "    </body>" >> algos-add.html;
	echo " <br>" >> algos-add.html;
	echo "</body>" >> algos-add.html;
	echo "</html>" >> algos-add.html;
	
	#rename file
	mv algos-add.html algos.html
	cd ..;

done
