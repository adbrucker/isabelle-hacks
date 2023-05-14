#!/bin/sh
exec isabelle scala "$0" "$@"
!#

import isabelle._
import scala.io.Source
import scala.math.Ordering.Implicits._

def print_times (sessions:List[String], sort:String):Unit = {
  val user_output_dir: Path = Path.explode("$ISABELLE_HEAPS/$ML_IDENTIFIER")
  val store = Sessions.store(Options.init())
  for (session <- sessions) {
    val db_path = user_output_dir+store.database(session)
    if (db_path.file.exists()) {
      val db = SQLite.open_database(db_path)
      val command_timings = store.read_command_timings(db, session)
      val theory_timings = store.read_theory_timings(db, session)
      if (!(theory_timings.isEmpty)) {
        println()
        println(f" +---------------------------------------------------------+-------------+------------+------------+")
        println(f" | Theory                                                  | elapsed [s] | cpu [s]    | gc [s]     |")
        println(f" +---------------------------------------------------------+-------------+------------+------------+")        
        val timings = sort match {
          case "n" => theory_timings.sortBy(p => p.filter(e => e._1 == "name").head)
          case "e" => theory_timings.sortBy(p => p.filter(e => e._1 == "elapsed").head._2.toDouble).reverse
          case "c" => theory_timings.sortBy(p => p.filter(e => e._1 == "cpu").head._2.toDouble).reverse
          case "g" => theory_timings.sortBy(p => p.filter(e => e._1 == "gc").head._2.toDouble).reverse
          case  _  => theory_timings
        }
        var telapsed = 0.0
        var tcpu = 0.0
        var tgc = 0.0
        timings.foreach { p => {
          val (_, theory) = p.filter (e => e._1 == "name").head
          val (_, elapsed) = p.filter (e => e._1 == "elapsed").head
          val (_, cpu) = p.filter (e => e._1 == "cpu").head
          val (_, gc) = p.filter (e => e._1 == "gc").head
          telapsed = telapsed + elapsed.toDouble
          tcpu = tcpu + cpu.toDouble
          tgc = tgc + gc.toDouble
          println(f" | $theory%-55s | $elapsed%11s | $cpu%10s | $gc%10s |")}
        }
        println(f" +---------------------------------------------------------+-------------+------------+------------+")
        println(f" | Total                                                   | $telapsed%11.3f | $tcpu%10.3f | $tgc%10.3f |")
        println(f" +---------------------------------------------------------+-------------+------------+------------+")
        println()
      }else{
        System.err.println("Warning: No theory timings found!")
      }
    }
  }
 } 
def main(args: Array[String]):Unit = {
val usage = """
    Usage: isabelle scala extract_timing_as_csv.scala [--help] SESSION_NAME

           --sort n|c|e|g sort by [n]ame, [c]pu, [e]lapsed, [g]c
           --help       prints this help message
  """
  val arglist = args.toList
  type OptionMap = Map[Symbol, Any]
  Map(scala.Symbol("sort") -> "u")
  def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
    def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--help" :: tail      => println(usage) 
                                      System.exit(0)
                                      map
        case "--sort" :: o :: tail => o match {
                                        case "n" => nextOption(map ++ Map(scala.Symbol("sort") -> "n"), tail)
                                        case "c" => nextOption(map ++ Map(scala.Symbol("sort") -> "c"), tail)
                                        case "e" => nextOption(map ++ Map(scala.Symbol("sort") -> "e"), tail)
                                        case "g" => nextOption(map ++ Map(scala.Symbol("sort") -> "g"), tail)
        }
        case string :: Nil         => nextOption(map ++ Map(scala.Symbol("session") -> string), list.tail)
        case option :: tail        => println("Unknown option "+option) 
                                      System.exit(1)
                                      map
      }
  }
  val options = nextOption(Map(),arglist)

  val sessions = List(options(scala.Symbol("session")).asInstanceOf[String])
  print_times (sessions, options(scala.Symbol("sort")).asInstanceOf[String])

}

