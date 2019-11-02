# Scame
Scame is a [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) interpreter (currently being) written in Scala. It supports running source code either on the REPL, or from a source file. Support for more scheme features will be added to the interpreter soon.

**Technologies Used:** [Scala](https://www.scala-lang.org/) by Martin Odersky et al, as the main programming language; [FastParse](https://www.lihaoyi.com/fastparse/) by Lihaoyi et al, as the parser combinator library; and [ZIO](https://zio.dev/) by John De Goes et al, to handle IO and deal with _the configuration problem_ (which I suspect will become more apparent in the future).

## Running the Program
Since Scame is written in Scala, you can run it in the same ways you run any Scala programs (e.g. through SBT or Java's `java -jar` command). To activate the REPL, just run the program without passing any command-line arguments. To run a source file, pass in the source file's filename as an argument.
