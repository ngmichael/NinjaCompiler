import scala.io.Source

object Main {

  var inputFilePath: String = "/home/noah/dev/NinjaCompiler/target/scala-2.12/classes/input.nj"
  var input: Iterator[String] = Iterator.empty

  def main(args: Array[String]): Unit = {
    input = Source.fromFile(inputFilePath).getLines()
    while(input.hasNext) {
      println(input.next())
    }
  }
}
