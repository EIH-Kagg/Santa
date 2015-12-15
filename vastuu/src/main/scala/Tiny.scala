/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package vastuu

import oscar.linprog.modeling._
import oscar.algebra._

import scala.math.{asin,cos,pow,sin,sqrt}
import scala.io.Source

// TODO Rearrange
object MIPVarUpgrades {
  implicit class MIPVarToString(val v: MIPIntVar) {
    def display: String = s"${v}: ${v.value.getOrElse('x')}"
    def displayBinIf(cond: Double => Boolean): Option[String] = {
      v.value.flatMap { x => if (cond(x)) Some(v.toString) else None }
    }
    def displayStrIfTrue(str: String): Option[String] = {
      v.value.flatMap { x => if (x == 1.0) Some(str) else None }
    }
  }
}

/**
  * Formulation for 20-50 gifts.
  * A possible MIP formulation (of problem size O( Locations^2 * sledges * gifts )
  * - model each gifts path using binary variables, per edge, and per sledge
  * - model each sledge's path using binary variables
  * - assign gifts to sledges taking capacity into account
  * - the gifts' path should be superseded by the sledge's path that is assigned to it
  * - note: travel time * weight can be priced individually 
    
  * The number of sledges is not limited, so we can assume a sledge cannot go back to
  * the Pole to pick up a second round as such a round can be served by another sledge.
  * 
  */
object Tiny extends MIPModelGLPK with App {

  // Variables
  //
  // gt_g,s,i,j: binary, whether gift g travelled btwn locations i and j on sledge s.
  // st_s,i,j: binary, whether sledge s travelled btwn locations i and j.

  // Constraints
  //
  // CAP_s: sum of gifts (ever) travelling on sledge s must not exceed capacity.
  // T_g: gt_g,s,i,j => st_s,i,j (gift travelling link on sledge requires sledge doing so.)
  //
  // (Sledge only visits North Pole once--otherwise really just a new sledge.)

  // Objective
  //
  // Minimize [sum of all gt_g,s,i,j * distance(i,j) * weight(g) ] +
  //          [sum of all st_s,i,j * 10]
  //
  // (Count the trip back to pole.)

  /////////////////////////////
  // CONSTANTS AND UTILITIES //
  /////////////////////////////
  val SledgeCapacity: Double = 1000.0
  val SledgeWeight: Double = 10.0
  val EarthRadius: Int = 100  // arbitrary radius
  val PoleLatitude: Double = 90.0
  val PoleLongitude: Double = 0.0

  def haversine(lat1: Double, long1: Double, lat2: Double, long2: Double): Double = {
    2.0 * EarthRadius *
    asin( sqrt( pow(sin((lat2 - lat1) / 2), 2) +
                cos(lat1) * cos(lat2) * pow(sin((long2 - long1) / 2), 2) ) )
  }

  //////////////////////
  // PROBLEM INSTANCE //
  //////////////////////
  case class ProblemInstance(
    maxSledges: Int,
    inputPath: String = "/Users/ehsu/Kaggle/santa/provided/",
    inputFilename: String = "gifts-nano.csv",
    timeLimitInMin: Int = 10
  ) {

    def parseData: Seq[(Int,Double,Double,Double)] = {
      val inputFile: String = inputPath + inputFilename
      println(s"Parsing ${inputFile}.")
      val src = Source.fromFile(inputFile)
      val it = src.getLines().drop(1).map(_.split(","))
      it.map { r =>
        // GiftId,Latitude,Longitude,Weight
        (r(0).toInt,r(1).toDouble,r(2).toDouble,r(3).toDouble)
      }.toSeq
    }

    def solve: Unit = {
      val rows = parseData
      val giftIds: Vector[Int] = rows.map(_._1).toVector
      val numGifts: Int = giftIds.size
      val giftsRange: Range = 0 until numGifts

      val coords: Vector[(Double,Double)] =
        rows.map { r => (r._2,r._3) }.toVector
      val weights: Vector[Double] = 
        rows.map { r => r._4 }.toVector

      println("giftIds: " + giftIds)
      println("coords: " + coords)
      println("weights: " + weights)

      println(s"Processing ${numGifts} gifts.")

      // Locations: North pole is last location, index is numLocs - 1.
      // Every other location has the same index as its corresponding gift.
      val locs: Vector[(Double,Double)] = coords :+ (PoleLatitude,PoleLongitude)
      val numLocs: Int = locs.size
      val poleIndex: Int = numLocs - 1
      val locsRange: Range = 0 until numLocs

      val sledges = 0 until maxSledges
      val numSledges = maxSledges

      // Destinations: Mapping from gift indices to location indices
      // (Really just a 1-to-1 mapping here.)
      val dest: Map[Int,Int] = (0 until numGifts).map(i => (i,i)).toMap

      // Gift traversal variables: gift g passes between locations i and j on sledge s.
      val gts = Array.tabulate(numGifts, numSledges, numLocs, numLocs) { (g,s,i,j) =>
        MIPIntVar(s"gt:${g},${s},${i},${j}", 0 to 1)
      }

      // Gift-Sledge assignments: gift g rides on sledge s.
      // TODO: Speedup if we get rid of this and generate answer from soln manually?
      val gss = Array.tabulate(numGifts, numSledges) { (g,s) =>
        MIPIntVar(s"gs:${g},${s}", 0 to 1)
      }

      // Sledge traversal variables: sledge s passes between locations i and j.
      val sts = Array.tabulate(numSledges, numLocs, numLocs) { (s,i,j) =>
        MIPIntVar(s"st:${s},${i},${j}", 0 to 1)
      }

      // Sledge final location variables: last stop before Pole; or, just Pole.
      val sfls = Array.tabulate(numSledges, numLocs) { (s,i) =>
        MIPIntVar(s"sfl:${s},${i}", 0 to 1)
      }

      // Calculate pairwise distances.
      val distances: Map[(Int,Int),Double] = (
        for ( i <- locsRange; j <- locsRange )
        yield {
          val (lat1,long1) = locs(i)
          val (lat2,long2) = locs(j)
          ((i,j), haversine(lat1, long1, lat2, long2))
        }).toMap

      ///////////////////////////////
      // Constraints on Gift Paths //
      ///////////////////////////////

      /* Gifts shouldn't traverse self-loops. */
      for (g <- giftsRange; s <- sledges; i <- locsRange )
        add { gts(g)(s)(i)(i) == 0 }

      /* Gifts must travel out from North Pole (and not come back.) */
      for (g <- giftsRange) {
        add( sum(sledges, locsRange){ (s,i) => gts(g)(s)(poleIndex)(i) } == 1 )
        add( sum(sledges, locsRange){ (s,i) => gts(g)(s)(i)(poleIndex) } == 0 )
      }

      /* Gifts must at some point travel into their destination (and never travel out.) */
      for (g <- giftsRange) {
        add( sum(sledges, locsRange){ (s,i) => gts(g)(s)(i)(dest(g)) } == 1 )
        add( sum(sledges, locsRange){ (s,i) => gts(g)(s)(dest(g))(i) } == 0 )
      }

      /* Gift shouldn't enter/exit the same location twice. */
      /* (Objective prevents creation of loops in optimal solution.) */
      /* (TODO: explicitly enforced here, but not in reference.  Remove?) */
      /* (Kind of superseded by similar contraint on sledges.) */
      //for (g <- giftsRange; s <- sledges; i <- locsRange) {
      //  add( sum(locsRange){ (j) => gts(g)(s)(j)(i) } <= 1 )
      //  add( sum(locsRange){ (j) => gts(g)(s)(i)(j) } <= 1 )
      //}

      /* Gift enters location (except Pole and final destination) iff it also exits. */
      for (g <- giftsRange; s <- sledges; i <- locsRange
           if i != poleIndex; if i != dest(g))
        add( sum(locsRange){ (j) => gts(g)(s)(i)(j) } ==
             sum(locsRange){ (j) => gts(g)(s)(j)(i) } )

      /////////////////////////////////
      // Constraints on Sledge Paths //
      /////////////////////////////////

      /* Sledges shouldn't traverse self-loops. */
      for (s <- sledges; i <- locsRange)
        add { sts(s)(i)(i) == 0 }

      /* All sledges have exactly one final location. */
      for (s <- sledges)
        add( sum(locsRange){ (i) => sfls(s)(i) } == 1 )

      /* Sledge enters location (except Pole) iff it also exits, exception: final loc. */
      for (s <- sledges; i <- locsRange
            if i != poleIndex)
         add( sum(locsRange){ (j) => sts(s)(j)(i) } ==
              sum(locsRange){ (j) => sts(s)(i)(j) } + sfls(s)(i) )

      /* Sledge can only leave each location once (otherwise it could make depots). */
      for (s <- sledges; i <- locsRange) {
        add( sum(locsRange){ (j) => sts(s)(i)(j) } <= 1 )
        add( sum(locsRange){ (j) => sts(s)(j)(i) } <= 1 )
      }

      /* Sledges don't explicitly come home.  Helps search bcs otherwise search might end
         with unused sledges coming home; and, should also elp pre-solver.  (???) */
      for (s <- sledges; i <- locsRange)
        add( sts(s)(i)(poleIndex) == 0 )

      ///////////////////////////////////////////
      // Constraints on Gift-Sledge Assigments //
      ///////////////////////////////////////////

      /* Every gift needs an assigned sledge. */
      for (g <- giftsRange)
        add( sum(sledges){ (s) => gss(g)(s) } == 1)

      /* Every gift only rides on its assigned sledge. */
      for (g <- giftsRange; s <- sledges; i <- locsRange; j <- locsRange)
        add( gts(g)(s)(i)(j) <= gss(g)(s) )

      /* Every sledge has a capacity. */
      for (s <- sledges)
        add( sum(giftsRange){ (g) => gss(g)(s) * weights(g) } <= SledgeCapacity )

      /* Whenever a gift traverses an edge, so too must the specified sledge. */
      for (g <- giftsRange; s <- sledges; i <- locsRange; j <- locsRange)
        add( gts(g)(s)(i)(j) <= sts(s)(i)(j) )

      ////////////////////////
      // OBJECTIVE FUNCTION //
      ////////////////////////

      /* 
       * Sum of weights of gifts as they are hauled over their traversals, plus
       * baseline weight of sledges as they go over their traversals, plus
       * weight of empty sledge on returning to North Pole from final location.
       */
      minimize(
        sum(giftsRange, sledges, locsRange, locsRange){ (g,s,i,j) =>
          gts(g)(s)(i)(j) * (weights(g) * distances(i, j)) } +
        sum(sledges, locsRange, locsRange){ (s,i,j) =>
          sts(s)(i)(j) * (SledgeWeight * distances(i, j)) } +
        sum(sledges, locsRange){ (s,i) =>
          sfls(s)(i) * (SledgeWeight * distances(i, poleIndex)) }
      )

      start()
      println("objective: " + objectiveValue)

      val pToG2Dist = haversine(90.0,0.0,12.494749307,28.6263955635)
      val g2ToG1Dist = haversine(12.494749307,28.6263955635,16.3457688674,6.30354512503)
      val g1ToPDist = haversine(16.3457688674,6.30354512503,90.0,0.0)

      println("p to g2: " + pToG2Dist)
      println("g2 to g1: " + g2ToG1Dist)
      println("g1 to p: " + g1ToPDist)

      println("distances: " + distances)

      val gCost = pToG2Dist * (15.5244795726 + 1.0) + g2ToG1Dist * (1.0)
      val sCost = (pToG2Dist + g2ToG1Dist) * SledgeWeight
      val rCost = g1ToPDist * SledgeWeight

      println("(Hand-Calculated) Gifts Cost:   " + gCost)
      println("(Hand-Calculated) Sledges Cost: " + sCost)
      println("(Hand-Calculated) Returns Cost: " + rCost)
      println("(Hand-Calculated) Total: " + (gCost + sCost + rCost))

      val giftsCost: Double = sum(giftsRange, sledges, locsRange, locsRange){ (g,s,i,j) =>
        gts(g)(s)(i)(j) * (weights(g) * distances(i, j)) }.value.getOrElse(0.0)

      val sledgeCost: Double = sum(sledges, locsRange, locsRange){ (s,i,j) =>
        sts(s)(i)(j) * (SledgeWeight * distances(i, j)) }.value.getOrElse(0.0)

      val returnCost: Double = sum(sledges, locsRange){ (s,i) =>
        sfls(s)(i) * (SledgeWeight * distances(i, poleIndex)) }.value.getOrElse(-1.0)

      println("Gifts Cost:   " + giftsCost)
      println("Sledges Cost: " + sledgeCost)
      println("Returns Cost: " + returnCost)
      println("Total: " + (giftsCost + sledgeCost + returnCost))

      import vastuu.MIPVarUpgrades._
      val gs = for (s <- sledges; g <- giftsRange) yield gss(g)(s).displayStrIfTrue(s"$g-$s")
      println("Gift-Sledge assignments: " + gs.flatten.mkString(" "))

      val g = for (g <- giftsRange; s <- sledges; i <- locsRange; j <- locsRange) yield
        gts(g)(s)(i)(j).displayStrIfTrue(s"$g-$s($i,$j)")
      println("Gift traversals: " + g.flatten.mkString(" "))

      val s = for (s <- sledges; i <- locsRange; j <- locsRange) yield
        sts(s)(i)(j).displayStrIfTrue(s"$s($i,$j)")
      println("Sledge traversals: " + s.flatten.mkString(" "))

      val f = for (s <- sledges; i <- locsRange) yield
        sfls(s)(i).displayStrIfTrue(s"$s($i)")
      println("Sledge final locations: " + f.flatten.mkString(" "))

      // for (s <- sledges; i <- locsRange) {
      //   println(sfls(s)(i).display)
      // }

      release()

      // TODO: Try MPBinaryVar https://jenkins.info.ucl.ac.be:8080/job/oscar-dev/javadoc/index.html#oscar.linprog.modeling.MPBinaryVar$

    }
  }

  val prob = new ProblemInstance(3)
  prob.solve

  /*
  val n = 4

  val Numbers = 1 to n * n
  val Lines = 0 until n
  val Columns = 0 until n

  val x = Array.tabulate(n, n, n * n)((l, c, N) => MIPIntVar("x" + (l, c, N), 0 to 1))
  val s = MIPIntVar("s", 0 to 10000000)

  for (l <- Lines; c <- Columns)
    add(sum(Numbers)((n) => x(l)(c)(n - 1)) == 1)

  for (n <- Numbers)
    add(sum(Lines, Columns)((l, c) => x(l)(c)(n - 1)) == 1)

  for (l <- Lines)
    add(sum(Columns, Numbers)((c, n) => x(l)(c)(n - 1) * (n)) == s)

  for (c <- Columns)
    add(sum(Lines, Numbers)((l, n) => x(l)(c)(n - 1) * (n)) == s)

  add(sum(Lines, Numbers)((l, n) => x(l)(l)(n - 1) * (n)) == s)

  //mip.add(sum(Lines,Numbers)((l,n) => x(l)(n-l-1)(n-1)*(n))==s) // TODO: fix this constraint

  maximize(s) 
  start()
  println("objective: " + objectiveValue)

  for (l <- Lines) {
    println(Columns.map(c => Numbers.filter(n => x(l)(c)(n - 1).value.get == 1)).mkString(","))
  }
  release()

   */
}
