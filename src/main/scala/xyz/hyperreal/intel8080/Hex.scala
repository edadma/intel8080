package xyz.hyperreal.intel8080

import scala.collection.immutable.LazyList._

object Hex {

  private val hexDigit = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') toSet

  def apply(mem: Memory, src: io.Source): Unit = apply(mem, src.getLines)

  def apply(mem: Memory, src: String): Unit = apply(mem, io.Source.fromString(src))

  def apply(mem: Memory, src: Iterator[String]): Unit = {
    @scala.annotation.tailrec
    def skipline(s: LazyList[Char]): LazyList[Char] =
      s match {
        case LazyList() => s
        case '\n' #:: t => t
        case _ #:: t    => skipline(t)
      }

    var addr = -1

    for ((line, i) <- src zipWithIndex) {
      @scala.annotation.tailrec
      def chars(s: LazyList[Char]): Unit =
        s match {
          case LazyList()         =>
          case (' ' | '\t') #:: t => chars(t)
          case d1 #:: d2 #:: d3 #:: d4 #:: ':' #:: t if hexDigit(d1) && hexDigit(d2) && hexDigit(d3) && hexDigit(d4) =>
            addr = Integer.parseInt(d1.toString + d2 + d3 + d4, 16)
            chars(t)
          case '/' #:: '/' #:: t => chars(skipline(t))
          case d1 #:: d2 #:: t if hexDigit(d1) && hexDigit(d2) =>
            if (addr == -1)
              sys.error(s"no address on line ${i + 1}: $line")

            mem.programByte(addr, Integer.parseInt(s"$d1$d2", 16).asInstanceOf[Byte])
            chars(t)
          case _ => sys.error(s"error on line ${i + 1}: $line")
        }

      chars(LazyList from line)
    }
  }

}
