#!/bin/bash

# Directory containing .Rd files
DIR="../man"

# Loop through each .Rd file in the specified directory
for file in "$DIR"/*.Rd; do
  # Get the base name of the file without extension
  base_name=$(basename "$file" .Rd)
  
  # Convert to HTML
  R CMD Rdconv -t html -o "$DIR/$base_name.html" "$file"
  # pandoc -s "$file" -o "$DIR/$base_name.html"
  
  # Convert to PDF (using Rd2pdf command)
  # R CMD Rd2pdf -o "$DIR/$base_name.pdf" "$file"
done

echo "Conversion complete."
