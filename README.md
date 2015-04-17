This Common Lisp package takes a collection of `.TextGrid` files and
`formant_data.txt` files and parses them to produce a `.csv` file
suitable for import into a data processing environment such as R.

The file expects a set of TextGrid files and a set (potentially a set
of one) of files called `formant_data.txt`, which consist of lines
having the following format:

````
<name of .wav file> <timepoint of formant measurement> <F1> <F2>
````

The script parses the TextGrid files and finds the formant
measurements from the `formant_data.txt` files.

This is not a full parser for TextGrid files at this stage, but it
does support IntervalTiers. The mark-up scheme is obviously specific
to my project, but it shouldn't be too difficult to tweak it (look at
the `parse-file` function).

This package is free software, released under the
[Community Research and Academic Programming License](http://matt.might.net/articles/crapl/) (CRAPL).
