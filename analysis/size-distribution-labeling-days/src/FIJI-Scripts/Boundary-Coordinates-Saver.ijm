function coordinate_extractor_colony(label) {
	name_array = split(label,"-");
	nR = nResults;
	Roi.getCoordinates(x, y);
	for (i=0; i<x.length; i++) {
		setResult("X", i+nR, x[i]);
		setResult("Y", i+nR, y[i]);
		updateResults();
	}
}

colony_roi_name = Roi.getName;

coordinate_extractor_colony(colony_roi_name);

title = getTitle();
file_title = substring(title, 0, indexOf(title, "."));
saveAs("Results", "/Users/bhargaa/Desktop/Boundary-Coordinates/" + file_title + "-Boundary-Coordinates.csv");
run("Clear Results");
run("Close All");
close("Results");


n = roiManager("count");
for (i=0; i<n; i++){
	roiManager("select", i);
	name = Roi.getName;
	coordinate_extractor(name);
}
