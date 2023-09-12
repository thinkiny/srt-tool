import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

class SrtBuilder:
  private var num = 0
  private var buffer = ListBuffer[SrtItem]()
  private val sub = new StringBuilder
  private var time: Array[String] = null

  def addLine(l: String): SrtBuilder =
    if num > 2 && l == "" then
      if sub.nonEmpty then
        buffer += SrtItem(time(0), time(1), sub.toString())
        sub.clear()
      num = 0
    else
      num match
        case 1            => time = l.split("-->").map(_.trim())
        case 2            => sub ++= l.trim()
        case _ if num > 2 => sub.append(" ").append(l.trim())
        case _            =>
      num += 1
    this

  def getResult(): List[SrtItem] = buffer.toList

  def mergeResult(): List[SrtItem] =
    val res = ListBuffer[SrtItem]()

    @tailrec
    def merge(item: SrtItem): Unit =
      if res.isEmpty then res += item
      else
        res.last.mergeLater(item) match
          case MergeResult.DropPrev(item) =>
            res.dropRightInPlace(1); merge(item)
          case MergeResult.Modify(item) => merge(item)
          case MergeResult.Stop         => res += item
          case MergeResult.Skip         =>

    buffer.filter(_.sub.nonEmpty).foreach(merge)
    res.toList
