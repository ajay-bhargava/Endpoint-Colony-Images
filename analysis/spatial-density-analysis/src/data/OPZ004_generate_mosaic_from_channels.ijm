input = getDirectory("input"); 
output = getDirectory("output");
suffix = ".oir";
setBatchMode(true);
processFolder(input);

// function to scan folders/subfolders/files to find files with correct suffix
function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	print("Processing: " + input);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + list[i]));
			processFolder(input + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) {
	setBatchMode("hide");
	path = input + file;	
	well_id = substring(file, 0, indexOf(file,"."));
	image_id = substring(file, 0, indexOf(file,"."));
	run("Bio-Formats Macro Extensions");
	Ext.openImagePlus(path);
	makeOval(398, 397, 2523, 2523);
	setBackgroundColor(0, 0, 0);
	run("Clear Outside", "stack");
	run("Crop");
	run("Split Channels");
	names = newArray(nImages); 
	ids = newArray(nImages); 
		for (i=0; i<ids.length; i++){ 
			selectImage(i+1);
			ids[i] = getImageID();
			names[i] = getTitle();
			if (startsWith(names[i], "C1")){
				  saveAs("tiff", output + well_id);
			}
		}
	close("*"); 
}
print("Done!");