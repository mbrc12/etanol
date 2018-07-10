# Etanol - a purity and nullability analysis tool for Java

[![CircleCI](https://circleci.com/gh/mbrc12/etanol.svg?style=svg)](https://circleci.com/gh/mbrc12/etanol)

## Installation

You can install `etanol` as follows:

```
$ git clone --recursive https://github.com/mbrc12/etanol
$ cd etanol
$ stack install 
<Builds and installs etanolx>   
$ etanolx 
```

`etanolx` is the name of the executable. You can try running `etanolx --help` for further help.

## Example

Suppose you have a project in a directory `foo/`, and it depends on some library `bar.jar`. You wish to analyse `foo/` for purity and nullability using etanol. One way is as follows:

```
$ etanolx -a bar.jar -s "rt.db" -o bar.db
    # This assumes that the standard rt.jar for the JRE you're using has been
    # analysed previously and stored in rt.db
$ etanolx -a foo/ -s "rt.db bar.db" -o foo.db
    # Note that the two dependency analyses (rt.db, bar.db) are separated by
    # space inside the quotes
```

The analysis outputs a `foo.db` file which contains the analysis results for your project. You are also given a summary like this (on the standard output):

```
Analysis results:

Fields:
  Normal: 444
  Basic: 162
  FinalStatic: 1174
Methods:
  Pure: 360
  Impure: 2034
  Local: 15
  StrongImpure: 2
  UnanalyzableMethod: 1975
Field Nullability:
  NullableField: 444
  NonNullableField: 1336
Method Nullability:
  NullableMethod: 541
  NonNullableMethod: 2042
  UnanalyzableNullMethod: 1803
```

## Usage

Analysis in `etanolx` requires 3 arguments. First is the name of the directory containing the class files (which will be searched
recursively) or the path to the `.jar` file. Second is the sources, which are outputs of previous analyses, and the third is the
output destination of the current analysis. All 3 are expanded on below.

### Analysis target

Analysis in `etanol` proceeds by first loading the classfiles for analysis. Currently, you can either specify a directory or a `.jar`
file. However you can also control how etanol works with them (both of which are externally similar, except, possibly, for performance
improvements). 

Etanol can work in two different ways at present.

1. Directory Backend, in which all its
accesses are from a directory. In this mode, directories are kept as is, but jar files are _expanded_ to temporary directories for 
analysis.

2. JAR Backend, in which the exact opposite is performed. Jar files are treated as is, but directories
are _compressed_ into JAR files before analysis. Although `DirectoryBackend` is default, you can change that in `~/.etanol/config`.
The default `~/.etanol/config` is documented well, so doing that should be pretty easy.

### Sources

It is often the case that the analysis target code depends on other libraries. You may have already analysed them previously, and want
to pass the results of those analyses in for use during the current analysis. This can be done via the sources option. All analyses
produce an output file containing the results of the analysis. You can pass these files in separated by space enclosing them in a string
as follows 

> `... -s "<source 1> <source 2> <source 3> ..." ..`.

### Output 

As mentioned in the previous section, each analysis has an output file containing the results. This option allows you to specify 
the the output file name. It is used like 

> `... -o <output file name> ...`.

### The global config

`etanolx` maintains a global config file in `~/.etanol/config`, which contains two options at present: 

1. `Backend` :  Described above. 
2. `Verbosity` : This controls the amount of, well, verbosity, you want. There are 4 levels: 
    * `DebugLevel`
    * `InfoLevel`
    * `SeriousLevel`
    * `QuietLevel`
    
`DebugLevel` outputs a lot of data, and is suited only for debugging purposes (most of this output doesn't make sense at first sight, so its not very useful for the user).`InfoLevel` outputs only information messages.
`SeriousLevel` is for only serious errors/problems, and finally `QuietLevel` ensures that there is no verbosity (just the final summary).
Currently, `SeriousLevel` itself suppresses all output, as no `SeriousLevel` error messages are present in the code. The default
is `InfoLevel` which is probably what you want if you're just using the tool. But if
you want to develop the tool further, or want complete silence (other than the summmary) you can just change the `.etanol/config` file.


Thanks to [Nikita Tchayka](https://github.com/NickSeagull) for the cool name and to [Lorde](https://en.wikipedia.org/wiki/Lorde) for [_Melodrama_](https://en.wikipedia.org/wiki/Melodrama_(Lorde_album))!
