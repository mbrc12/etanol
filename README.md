# Etanol - a purity and nullability analysis tool for Java

[![CircleCI](https://circleci.com/gh/mbrc12/etanol.svg?style=svg)](https://circleci.com/gh/mbrc12/etanol)

_The rationale behind Etanol is explained in [this](https://mbrc12.github.io/blog/posts/2018-08-06-2.html) blog post. Please refer to it for a less technical discussion._

The first part of this document describes the command line tool. The second part describes the API.

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

To test whether is etanol is working as expected, run

```
$ sh test.sh
```

and check if there are any errors.

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

To know more about what each annotation means, please refer to the [Wiki](https://github.com/mbrc12/etanol/wiki). More information about each specific annotation will be added soon.


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

**N.B.** : Note that if you don't want to pass any sources, you don't need to add any `-s ..` option.

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
3. `AbortOnAbsence` : This controls the setting of whether the analysis should abort if an Unanalyzable method/field is encountered due to its absence in the current analysis scope. The two options are
    * `Abort` : Abort on absence, this is the default setting.
    * `DoNotAbort` : The opposite of the above. Continue analysis ignoring absence of class files.

For `Verbosity` : `DebugLevel` outputs a lot of data, and is suited only for debugging purposes (most of this output doesn't make sense at first sight, so its not very useful for the user).`InfoLevel` outputs only information messages.
`SeriousLevel` is for only serious errors/problems, and finally `QuietLevel` ensures that there is no verbosity (just the final summary).
Currently, `SeriousLevel` itself suppresses all output, as no `SeriousLevel` error messages are present in the code. The default
is `InfoLevel` which is probably what you want if you're just using the tool. But if
you want to develop the tool further, or want complete silence (other than the summmary) you can just change the `.etanol/config` file.

### The Etanol API

Etanol can also work in an API mode, for integration into applications. The main entry point of the API is a function called `analysis` located in `Etanol.API`. This function takes an `AnalysisInputType` record and returns a `AnalysisOutputType` record (Not exactly; it returns a `Either [ClassName] AnalysisOutputType`, it is described below). 

The `AnalysisInputType` is 
```
data AnalysisInputType 
    = AnalysisInputType 
    { classes :: [ClassName]
    , targets :: [AnyID] 
    , classProvider :: ClassProvider
    , sourceClasses :: S.Set ClassName
    , sourceFieldDB :: FieldDB
    , sourceMethodDB :: MethodDB
    , sourceFieldNullabilityDB :: FieldNullabilityDB
    , sourceMethodNullabilityDB :: MethodNullabilityDB
    } 
```

The fields are detailed below:
* `classes` : The classes you want to analyse.
* `targets` : A list of `AnyID`s (which is basically a tagged union of `MethodID` and `FieldID`s), to target for analysis.
* `classProvider` : A function from `ClassName` to `Maybe ByteString`, a function that provide `ByteString`s of classes on demand (or indicate that it couldn't be found through `Nothing`. 
* `sourceClasses`: The classes that are present in the dependencies you want to pass in. The following four fields hold information about their analysis properties. Most of the time, this comes from the output of a previous analysis.
* `sourceFieldDB, sourceMethodDB, sourceFieldNullabilityDB, sourceMethodNullabilityDB` : Information about the purity/nullability of methods/fields present in the dependency classes (`sourceClasses`).

The type synonyms used above and below are as follows:

```
type ClassName = Text 	-- from text

data AnyID
    = EFieldID { fieldID :: !FieldID }
    | EMethodID { methodID :: !MethodID }

type FieldID = (FieldName, FieldDescriptor)

type MethodID = (MethodName, MethodDescriptor)

type FieldDescriptor = Text -- of the form like I or LPair;

type MethodDescriptor = Text -- of the form like (I)I or (LPair;)Z

type FieldName = Text -- like a.b.c.FIELD

type MethodName = Text -- like a.b.c.d.methodName

type FieldDB = Map FieldID FieldType 		-- standard map from haskell Data.Map

type MethodDB = Map MethodID MethodType

type FieldNullabilityDB = Map FieldID FieldNullabilityType

type MethodNullabilityDB = Map MethodID MethodNullabilityType

data FieldType
    = Normal
    | Basic
    | FinalStatic
    | UnanalyzableField

data FieldNullabilityType
    = NullableField
    | NonNullableField
    | UndecidedField
    | UnanalyzableNullField

data MethodType
    = Pure
    | Impure
    | Local
    | StrongImpure
    | UnanalyzableMethod

data MethodNullabilityType
    = NullableMethod
    | NonNullableMethod
    | UndecidedMethod
    | UnanalyzableNullMethod
    
```

As mentioned above, the output for `analysis` is `Either [ClassName] AnalysisOutputType`. The `Either` is due to the fact that you can choose to abort the analysis when classes are missing, by modifying `.etanol/config` as mentioned above.

If `DoNotAbort` is the option selected, you can be sure (upto bugs) that `Left ..` will never be the result. However, if `Abort` is selected, the API will try to collect as many missing classes as possible and return a `Left [ClassName]`. Otherwise `Right AnalysisOutputType` is returned.

`AnalysisOutputType` has the following definition:

```
data AnalysisOutputType
    = AnalysisOutputType
    { fieldPurity :: FieldID -> Maybe FieldType
    , methodPurity :: MethodID -> Maybe MethodType
    , fieldNullability :: FieldID -> Maybe FieldNullabilityType
    , methodNullability :: MethodID -> Maybe MethodNullabilityType
    , fieldsAnalyzed :: [FieldID]
    , methodsAnalyzed :: [MethodID]
    } 
```
The fields are detailed below:

* `fieldPurity` : A function that returns the `FieldType` (wrapped in Maybe in case the field is not found).
* `methodPurity, fieldNullability, methodNullability` : Analogous to the above.
* `fieldsAnalyzed, methodAnalyzed` : Lists of fields and methods analysed by analysis, useful for debugging.

It is not immediately clear how to use this API given that the output and input types are so different, so here is an explanation of how you might use it for a usecase.

The testsuite for etanol uses the API in the following way:

First we define a `TestUnit` as follows:

```
data TestUnit 
    = TestUnit 
    { className :: T.Text 
    , units   :: [Unit]
    , sources :: [FilePath]
    } deriving (Show)
```

and `Unit` as:

```
data Unit
    = UField
    { field :: FieldID
    , fpurity :: FieldType
    , fnullability :: FieldNullabilityType
    } 
    | UMethod
    { method :: MethodID
    , mpurity :: MethodType
    , mnullability :: MethodNullabilityType
    } deriving (Show)
```

For example, right off the etanol tests, 
```
TestUnit { className = "T1"
         , sources = ["java.db"] 
         , units = [ UField 
                     { field = ("T1.x", "I")
                     , fpurity = FinalStatic
                     , fnullability = NonNullableField
                     }
                   , UMethod 
                     { method = ("T1.f", "(I)I")
                     , mpurity = Pure
                     , mnullability = NonNullableMethod
                     }
                   ]
         }
```

Now the verifier for the test suite works as follows:

* First it loads everything in `sources`, and merges all the maps and sets inside it (See the definition of `AllDB` is `Etanol.Types`.
* Next it creates a `classProvider` using the `Etanol.Crawler` library's `classesOnDemandBS` function, that can detect and convert all classes inside a directory, exposing a function that is a `ClassProvider`.
* Then it calls analysis with the targets listed in `units`, and just matches the output against the asserted purity/nullability results. 

You can read `~/test/Spec.hs` for the full code.


Thanks to [Nikita Tchayka](https://github.com/NickSeagull) for the cool name and to [Lorde](https://en.wikipedia.org/wiki/Lorde) for [_Melodrama_](https://en.wikipedia.org/wiki/Melodrama_(Lorde_album))!
<Paste>
