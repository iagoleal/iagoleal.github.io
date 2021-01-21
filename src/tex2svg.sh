inname="$1"
outname="$2"

cp "$inname" '/tmp/out.tex'
latex -output-directory '/tmp/' "/tmp/out.tex"
dvisvgm --scale=2 --optimize=all --font-format=woff -o "/tmp/out.svg" "/tmp/out.dvi"
cp "/tmp/out.svg" "$outname"
