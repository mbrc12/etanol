# Etanol - a purity and nullability analysis tool for Java

Currently the project is in its initial stages, and almost nothing is implemented. The specification/algorithm/implementation details are being written - please check out the Wiki.

You can test the project now, although its highly likely that the code will fail for your test-case right now, as a lot of debugging needs to be done. Anyway, after you clone the repository, do

```
$ cd etanol
$ stack ghci
<Should compile all files>
>> startpoint "<path/to/directory/containing/classes>"
```

`startpoint` is exported by `Etanol.Driver`, and is a convenience function to the Analysis interface.

Thanks to [Nikita Tchayka](https://github.com/NickSeagull) for the cool name and to [Lorde](https://en.wikipedia.org/wiki/Lorde) for [_Melodrama_](https://en.wikipedia.org/wiki/Melodrama_(Lorde_album))!
