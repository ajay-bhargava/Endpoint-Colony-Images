# Subdirectify 
for f in *.oir; do
  dir="${f%.*}"
  mkdir $dir
  mv "$f" "./$dir"
done
