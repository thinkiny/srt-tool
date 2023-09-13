import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

enum MergeAction:
  case DropPrev(item: SrtItem) extends MergeAction
  case Modify(item: SrtItem) extends MergeAction
  case DropCurrent extends MergeAction
  case AddCurrent extends MergeAction

case class SrtItem(
    startTime: LocalTime,
    endTime: LocalTime,
    sub: List[String]
)(using ord: Ordering[LocalTime]):
  def diffSeconds(item: SrtItem): Long =
    ChronoUnit.SECONDS.between(
      ord.min(startTime, item.startTime),
      ord.max(endTime, item.endTime)
    )

  def mergeLater(o: SrtItem): MergeAction =
    ((sub.size min o.sub.size) to 1 by -1).find(i =>
      sub.takeRight(i) == o.sub.take(i)
    ) match
      case Some(i) => {
        if (i == sub.size) then
          MergeAction.DropPrev(SrtItem(startTime, o.endTime, o.sub))
        else if o.sub.size == i then MergeAction.DropCurrent
        else
          MergeAction.Modify(
            SrtItem(o.startTime, o.endTime, o.sub.drop(i))
          )
      }
      case _ => MergeAction.AddCurrent

  def toSrtString(i: Int): String =
    val sTime = SrtItem.formatTime(startTime)
    val eTime = SrtItem.formatTime(endTime)
    s"${i}\n${sTime} --> ${eTime}\n${sub.mkString(" ")}\n\n"

object SrtItem:
  private val formatter = DateTimeFormatter.ofPattern("HH:mm:ss,SSS")
  private def parseTime(s: String): LocalTime =
    LocalTime.parse(s, formatter)

  private def formatTime(s: LocalTime): String =
    s.format(formatter)

  def apply(startTime: String, endTime: String, sub: String): SrtItem =
    SrtItem(
      parseTime(startTime),
      parseTime(endTime),
      "[\\w'-]+".r.findAllIn(sub).map(_.trim()).filter(_.nonEmpty).toList
    )
