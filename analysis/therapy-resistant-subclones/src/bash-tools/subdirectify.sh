# Subdirectify
for f in *.tif; do
  dir="${f%.*}"
  mkdir $dir
  mv "$f" "./$dir"
done
