input = getDirectory("input");
output = getDirectory("output");
suffix = ".oir";

setBatchMode(true);
processFolder(input);

// function to scan folders/subfolders/files to find files with correct suffix
function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i]))
			processFolder(input + File.separator + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) {
	path = input + file;
	run("Bio-Formats Macro Extensions");
	Ext.openImagePlus(path);
	makeOval(348, 474, 2688, 2688);
	setBackgroundColor(0, 0, 0);
	run("Clear Outside", "stack");
	run("Crop");
	current_title = getTitle();
    save_title = substring(current_title, 0, indexOf(current_title,"."));
    saveAs("tiff", output + save_title);
	close("*");
}
