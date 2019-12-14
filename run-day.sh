#!env sh
sol="day$1/solution.ml"
if [ -f $sol ]; then
	dune exec day"$1"/solution.exe --root .
else
	echo "please provide a valid day number as argument."
fi


