package icfpc2022

import java.io.File

trait Solver {
  def solve(target: File): (Program, Long)
}
