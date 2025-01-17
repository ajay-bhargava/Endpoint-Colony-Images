// EXP123 - Colony Information Extractor
// By: Ajay Bhargava
// 25/01/20
// Function: Takes an input directory, returns the list of coordinates for each subclone and colony, as well as EdU points

input = getDirectory("Choose a Directory");
output = "/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/shared-assets/processed-acquisitions-2/";

suffix = ".oir";

setBatchMode(false);

process_folder(input);

function FilterList(input, string){
	returnedList = newArray(0);
	for (i = 0; i < input.length; i++){
		if (indexOf(input[i], string) != -1)
			returnedList = Array.concat(returnedList,input[i]);
	}
	return returnedList;
}

function coordinate_extractor(label, color) {
	name_array = split(label,"-");
    color_property = color;
	nR = nResults;
	Roi.getCoordinates(x, y);
	for (i=0; i<x.length; i++) {
	  	setResult("Color", i+nR, color_property);
	  	setResult("Number", i+nR, name_array[0]);
		setResult("X", i+nR, x[i]);
		setResult("Y", i+nR, y[i]);
		updateResults();
	}
}

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

function process_folder(input){
	file_list = getFileList(input);
	file_list = Array.sort(file_list);
	images_list = FilterList(file_list, suffix);
	for (i = 0; i < images_list.length; i++){
		if(File.isDirectory(input + File.separator + images_list[i]))
			process_folder(input + File.separator + images_list[i]);
		if(endsWith(images_list[i], suffix))
			process_files(input, output, images_list[i]);
		}
}

function process_files(input, output, file){
	// Colony File
	run("Set Measurements...", "  redirect=None decimal=2");
	path = input + file;
	file_title = substring(file, 0, indexOf(file, "."));
	main_folder = output + file_title + File.separator;
	File.makeDirectory(main_folder);
	run("Bio-Formats Macro Extensions");
	Ext.openImagePlus(path);
	Stack.setChannel(4);
	run("Yellow");
	run("Duplicate...", "title=DAPI-Stack duplicate channels=1");
	selectWindow("DAPI-Stack");
	run("Gaussian Blur...", "sigma=30 scaled");
	//run("Threshold...");	
	setThreshold(83, 65535);
	run("Convert to Mask");
	run("Analyze Particles...", "size=1000000-Infinity add");
	close("DAPI-Stack");
	run("Duplicate...", "title=Working-Stack duplicate channels=2-4");
	selectWindow("Working-Stack");
	run("Scale Bar...", "width=500 height=16 font=12 color=White background=None location=[Lower Right] bold hide overlay label");
	Stack.setDisplayMode("composite");
	roiManager("Select", 0);
	roiManager("Set Color", "cyan");
	roiManager("Set Line Width", 5);
	run("Flatten");
	selectWindow("Working-Stack (RGB)");
	run("Flatten");
	selectWindow("Working-Stack (RGB)-1");
	saveAs("jpeg", main_folder + file_title);
	close("Working-Stack (RGB)-1");
	selectWindow(file);
	close("\\Others");
	roiManager("Select", 0);
	setBackgroundColor(0, 0, 0);
	run("Clear Outside", "stack");
	roiManager("delete");
	close("\\Others");
	selectWindow(file);
	saveAs("tiff", main_folder + file_title);
	close("*");
	
	// Colony Coordinates
	colony_coordinates_folder = main_folder + file_title + "-Colony-Data" + File.separator;
	File.makeDirectory(colony_coordinates_folder);
	open(main_folder + file_title + ".tif");
	roiManager("Show All");
	roiManager("Show None");
	run("Duplicate...", "title=Working-Stack-C1 duplicate channels=1");
	selectWindow("Working-Stack-C1");
	run("Gaussian Blur...", "sigma=30 scaled");
	//run("Threshold...");
	setThreshold(83, 65535);
	run("Convert to Mask");
	run("Analyze Particles...", "size=100000-Infinity add");
	roiManager("Select", 0);
	colony_roi_name = Roi.getName;
	roiManager("Save", colony_coordinates_folder + file_title + "-" + "Colony-Coordinates-ROI" + ".zip");
	coordinate_extractor_colony(colony_roi_name);
	saveAs("Results", colony_coordinates_folder + file_title + "-" + "Colony-Coordinates" + ".csv");
	run("Clear Results");
	close("Working-Stack-C1");
	roiManager("reset");
	
	// yPET Boundary Files
	yPET_coordinates_folder = main_folder + file_title + "-yPET-Clones" + File.separator;
	File.makeDirectory(yPET_coordinates_folder);
	run("Duplicate...", "title=Working-Stack-C2 duplicate channels=4");
	selectWindow("Working-Stack-C2");
	run("Gaussian Blur...", "sigma=2 scaled");
	//run("Threshold...");
	setThreshold(100, 65535);
	run("Convert to Mask");
	setOption("BlackBackground", false);
	run("Erode");
	roiManager("reset");
	run("Analyze Particles...", "size=1000-Infinity add");
	n = roiManager("count");
	for (i=0; i<n; i++){
	roiManager("select", i);
	name = Roi.getName;
	color = "yPET";
	coordinate_extractor(name, color);
	}
	saveAs("Results", yPET_coordinates_folder + file_title + "-yPET-Clones-Coordinates" + ".csv");
	run("Clear Results");
	roiManager("save", yPET_coordinates_folder + file_title + "-yPET-Clones-ROI-List" + ".zip");
	roiManager("reset");
	close("Working-Stack-C2");
	
	//dTomato Analysis
	dTomato_coordinates_folder = main_folder + file_title + "-dTomato-Clones" + File.separator;
	File.makeDirectory(dTomato_coordinates_folder);
	run("Duplicate...", "title=Working-Stack-C3 duplicate channels=2");
	selectWindow("Working-Stack-C3");
	run("Gaussian Blur...", "sigma=5 scaled");
	//run("Threshold...");
	setThreshold(132, 65535);
	run("Convert to Mask");
	roiManager("reset");
	setOption("BlackBackground", false);
	run("Erode");
	run("Analyze Particles...", "size=100-Infinity add");
	n = roiManager("count");
	for (i=0; i<n; i++){
	roiManager("select", i);
	name = Roi.getName;
	color = "dTomato";
	coordinate_extractor(name, color);
	}
	saveAs("Results", dTomato_coordinates_folder + file_title + "-dTomato-Clones-Coordinates" + ".csv");
	run("Clear Results");
	roiManager("save", dTomato_coordinates_folder + file_title + "-dTomato-Clones-ROI-List" + ".zip");
	roiManager("reset");
	close("Working-Stack-C3");
	
	// EdU Coordinates
	EdU_coordinates_folder = main_folder + file_title + "-EdU-Coordinates" + File.separator;
	File.makeDirectory(EdU_coordinates_folder);
	run("Duplicate...", "title=Working-Stack-C4 duplicate channels=3");
	selectWindow("Working-Stack-C4");
	run("Find Maxima...", "prominence=50 strict exclude output=[Single Points]");
	selectWindow("Working-Stack-C4 Maxima"); 
	run("Analyze Particles...", "add");
	n = roiManager("count");
	for (i=0; i<n; i++){
	roiManager("select", i);
	name = Roi.getName;
	color = "EdU";
	coordinate_extractor(name, color);
	}
	saveAs("Results", EdU_coordinates_folder + file_title + "-EdU-Maxima" + ".csv");
	run("Clear Results");
	roiManager("save", EdU_coordinates_folder + file_title + "-EdU-ROI-List" + ".zip");
	close("Working-Stack-C4 Maxima");
	roiManager("reset");

	// Retrieve coordinates of dTomato and yPET segmented objects. Load them, run the ROI numberizer and save the data as a flattened JPEG. 
	roiManager("open", dTomato_coordinates_folder + file_title + "-dTomato-Clones-ROI-List" + ".zip");
	roiManager("open", yPET_coordinates_folder + file_title + "-yPET-Clones-ROI-List" + ".zip");
	n = roiManager("count");
	for (i=0; i<n; i++){
		roiManager("select", i);
		name = Roi.getName;
		setResult("Number", i, i+1);
	}
	run("ROI Color Coder", "measurement=Number lut=[glasbey inverted] width=0 opacity=100 label=microns^2 range=Min-Max n.=5 decimal=0 ramp=[256 pixels] font=SansSerif font_size=14 draw");
	selectWindow(file_title + ".tif");
	close("\\Others");
	roiManager("Show All");
	run("Flatten", "slice");
	selectWindow(file_title + "-1" + ".tif");
	saveAs("jpeg", main_folder + file_title + "-Color-Code");
	
	// Cleanup for next image 
	run("Clear Results");
	roiManager("reset");
	close("*");
}

print("Done!");
