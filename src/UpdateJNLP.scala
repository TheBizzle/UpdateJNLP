import
  java.io.{ File, FileOutputStream }

import
  scala.io.{ BufferedSource, Source }

object UpdateJNLP extends App {

  val (inDir, outDir) = ("models", "newModels")

  val root   = new File(s"/home/jason/Desktop/$inDir")
  val files  = recursiveListFiles(root)
  val nlogos = files filter (_.getName.contains(".nlogo"))

  val JavaRegex  = """(.*props:get\s+"java.*)""".r
  val QuoteRegex = """(.*)(props:get\s+")(.*)""".r
  val VarRegex   = """(.*)(props:get\s+)(\w+)(.*)""".r

  nlogos foreach {
    file =>

      val newText = usingSource(_.fromFile(file)) {
        _.getLines() map {
          case JavaRegex(str)                     => str
          case QuoteRegex(start, boiler, end)     => s"$start${boiler + "jnlp."}$end"
          case VarRegex(start, boiler, term, end) => s"$start$boiler${s"""(word "jnlp." $term)"""}$end"
          case str                                => str
        } mkString "\n"
      }

      val FilenameRegex = s"(.*/)$inDir(/.*)".r
      val FilenameRegex(start, end) = file.getAbsolutePath

      val outFile = new File(s"$start$outDir$end")
      outFile.getParentFile().mkdirs()

      using(new FileOutputStream(outFile)) {
        _.write(newText.getBytes)
      }

  }


  private def using[A <: { def close() }, B](closeable: A)(f: A => B): B =
    try f(closeable) finally closeable.close()

  private def usingSource[T](sourceBuildFunc: (Source.type) => BufferedSource)(sourceProcessFunc: (BufferedSource) => T): T =
    using(sourceBuildFunc(Source))(sourceProcessFunc)

  // Courtesy of Rex Kerr (http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala)
  private def recursiveListFiles(f: File): Array[File] = {
    val files = f.listFiles
    files ++ (files filter (_.isDirectory) flatMap recursiveListFiles)
  }

}
