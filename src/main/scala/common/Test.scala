package common

import scala.collection.GenMap
import scala.collection.GenSeq
import kmeans._

object Test {
  def main(args: Array[String]) {
       
    println("Means:================================:")
    val means = Range(4, 16).toList.map(x => new Point(random(1, 9) + (random(1, 9) / 10), x.toFloat + (random(1, 9) / 10), random(1, 9) + (random(1, 9) / 10)))
    //means.foreach(println)

    println("Points:================================:")
    val points = Range(10, 20).toList.map(x => new Point(random(10, 50) + (random(1, 9) / 10), x.toFloat + (random(1, 9) / 10), random(10, 50) + (random(1, 9) / 10)))
    //points.foreach(println)
    
    val rawcluster = points.map(x => (findClosest(x, means), x))
    println("RawCluster: ================================:")
    //rawcluster.foreach(println)
    
    val groupd = rawcluster.groupBy(_._1)
    println("Mapped: ================================:")
    groupd.foreach(println)
    
   
    
    val temp = groupd.mapValues(f => f.map(_._2))
    
    println("????: ================================:")
    temp.foreach(println)
    //val temp = groupd.ma

//    println("================================:")
//    val zipped = for (p <- points; k <- kmeans) yield {
//      (k, p)
//    }

//    zipped.foreach(println)

    //val clusters = zipped.map(point => List[Point]().+:(point._2))
  
  }

  def random(start: Int, end: Int): Float = {
    val rnd = new scala.util.Random
    (start + rnd.nextInt((end - start) + 1)).toFloat
  }
  
  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  //  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
  //    ???
  //  }
}