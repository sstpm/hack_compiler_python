# hack_compiler_python
Nand2Tetris Python hack compiler

Originally written in 2021 for the course on Coursera, moved to github for safe keeping.

To use: ```python3 ./JackCompiler.py <jack file | directory>```

If given a valid Jack named ``x.jack``, this will output both ``x.vm`` and ``x.xml``.
This is a VM file in the intermediate language defined in the course as well as an XML file of the parser/tokenizer's output.
Pass the file to a working assembler to see the assembled file, but if I recall correctly the emulation tools the course uses can directly use the VM file as is.
