package icfpc2022

import scala.collection.mutable

class Timers {
  val timers = mutable.Map.empty[String, (Long, Long)]

  def time[T](label: String)(op: => T): T = {
    val ts = System.nanoTime() / 1000
    val res = op
    val diff = System.nanoTime() / 1000 - ts
    timers.updateWith(label) {
      case None         => Some((diff, 1L))
      case Some((t, c)) => Some((diff + t, c + 1L))
    }
    res
  }

  def output(label: String): Unit = {
    val (diff, cnt) = timers.getOrElse(label, (0L, 0L))
    val avg = if (cnt == 0L) 0L else diff / cnt
    println(f"$label%25s - ${avg}µs")
  }

  def outputAll: Unit =
    timers.keySet.toList.sorted.foreach(output)

  def reset(): Unit =
    timers.clear()
}
