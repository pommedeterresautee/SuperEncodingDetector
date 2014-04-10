## Super Encodign Detector ##

**Super Encodign Detector** is a full-featured tool to determine the encoding of a text file, and play with it!

### Features of the tool ###

The tool will provide you with features to play with encoding text file:

- **determine the encoding of a text file** (`--encoding`, `-e`)
- Convert a text file to **UTF-8** or **ISO-8859** (`-I`, `--ISO8859convert`, `-U`, `--UTF8convert`)
- merge text files with the same encoding, even if they have a BOM (`--merge`)

### Determine the encoding of a text file ###

Sometimes, you may wonder how to find the encoding of a text file. This tool is here for that purpose. It will first check the existence of a BOM (insert link), and if there are not, will conduct a Heuristic analyze based on the frequence of 

### Convert text encoding ###

Ok, now you know the encoding of this text file. Unfortunately, you want to import it in some tool you have, and it doesn't support the encoding (usually it happens with Unicode files).

What can you do?

Super Encoding Detector can help you here by converting your file to this other format!

You don't need to provide the program with the current encoding of the file, just choose what you need: UTF-8 or ISO-8859?

These two encoding should cover 95% of the cases, so we have not made it more complex than necessary, just choose between the two and execute the program. 

### Merge text file ###

You may thing that merging text file is as easy as `cat something >> somethingElse`?

Sometimes it will work, other times not!

Why? Because of the **[BOM](http://en.wikipedia.org/wiki/Byte_order_mark "Byte Order Mark explained by the great Encyclopedia!")**!

The **B**yte **O**rder **M**ark represents the first bytes of some Unicode files. It's used to recognize the format of a Unicode file without having to guess it through heuristic analyze.

The BOM is usefull, but sometimes it is also a bit dangerous, in particular when you try to merge several files. Why? because the BOM from the first file will be at the beginning of the file as expected, but the BOM of the second file will be in the middle of the final file. Because of that you may corrupt the entire file which is not acceptable for a so simple operation as merging text file!

This program will merge two or more files in one by removing the BOM of all files (but the first). So now you will be able to merge Unicode files as easily than merging any codepage text file.

### Help screen (to update) ###

SuperEncodingDetector will help you to manage text files in different encoding format.
This application is good for working with the different Unicode version and ASCII character set but not to manage national specific code pages.

* Encoding detection is based on the Byte Order Mark (BOM) of the file if it is available (UTF-8, UTF-16 BE/LE and the two versions of  UTF-32 BE/LE).
* Encoding detection is based on a full scan of the text file if no BOM is available (UTF-8 and ASCII).
* Conversion from Unicode to ASCII is done by replacing special characters by their ASCII equivalents if possible.
* Merge different files encoded in a format including a BOM. The final file will include only one BOM.

Example: java -jar SuperEncodingDetector.jar --input 