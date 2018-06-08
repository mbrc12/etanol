# Etanol - a purity and nullability analysis tool for Java

The Wiki will contain more details once the implementation is somewhat stabilized.

For installation, after you clone the repository, do

```
$ cd etanol
$ stack build
<Should build>
$ stack install
<Should install>
$ etanolx
```

`etanolx` is the name of the executable. Running that will have further instructions for use.

**Note:** Before you start analysing your desired code-base, it is recommended that you first extract the `rt.jar` jar file which contains the entire Java standard library, since most of your code would probably use `java.*` libraries (your code almost surely depends on `java.lang.Object`). However, an entire analysis of the `rt.jar` can take a long time, so you can do that by parts (but be sure to do that in proper order, that is, for example, do not analyse `javax.*` before `java.*` etc.)

In its current version, etanol can  at least properly analyse the `rt.jar` and Google's Guava libraries (of course, I mean to say that no errors were encountered during the processing, not that all the judgements were accurate!).

**Debugging/Error reporting note:** During debugging etanol, I found two main classes of bugs: Bugs that were during analysing the file itself, and could be reproduced even with etanol loading and analysing only that single file, and bugs that occurred only when an entire library was being analysed. So if you find a bug, please report it with the offending class, if you happen to know it from the output at the time. But if isolating that class and analysing it does not reproduce the error, then please also provide (a link to) the context (i.e. the library) being analysed at the time, for the sake of reproducibility.

Thanks to [Nikita Tchayka](https://github.com/NickSeagull) for the cool name and to [Lorde](https://en.wikipedia.org/wiki/Lorde) for [_Melodrama_](https://en.wikipedia.org/wiki/Melodrama_(Lorde_album))!
