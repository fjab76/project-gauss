package fjab.misc

import java.util.concurrent.{CountDownLatch, ForkJoinTask, RecursiveAction, _}

import fjab.euler

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object OneDimensionalStencil {

  /**
    * Sequential version
    * @param iter Number of iterations
    * @param arr Array with fixed boundary values. The other values are calculated as the average value of their neighbours
    * @return Array with the final values after completing all iterations
    */
  def seq(iter: Int, arr: Array[Double]): Array[Double] = {
    //defensive copy to keep arr from being mutated outside this method
    var (myVal,myNew) = (arr.clone(),arr.clone())

    for(_ <- 1 to iter) {
      for (i <- 1 until arr.length - 1) {
        myNew(i) = (myVal(i - 1) + myVal(i + 1)) / 2.0
      }
      val temp = myNew
      myNew = myVal
      myVal = temp
    }
    myVal
  }


  /**
    * Parallel version using ForkJoin
    */
  def parWithForkJoin(iter: Int, arr: Array[Double]) = {
    //defensive copy to stop arr from being mutated outside this method
    var (myVal,myNew) = (arr.clone(),arr.clone())

    for(_ <- 1 to iter) {
      val tasks = for (i <- 1 until arr.length - 1) yield new RecursiveAction {
        override def compute(): Unit = myNew(i) = (myVal(i - 1) + myVal(i + 1)) / 2.0
      }
      ForkJoinTask.invokeAll(tasks.asJavaCollection)
      val temp = myNew
      myNew = myVal
      myVal = temp
    }
    myVal
  }

  /**
    * Parallel version using CountDownLatch
    */
  def parWithLatch(iter: Int, arr: Array[Double]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    //defensive copy to stop arr from being mutated outside this method
    var (myVal,myNew) = (arr.clone(),arr.clone())

    for(_ <- 1 to iter) {
      val latch = new CountDownLatch(arr.length-2)
      for (i <- 1 until arr.length - 1) yield Future {
        myNew(i) = (myVal(i - 1) + myVal(i + 1)) / 2.0
        latch.countDown()
      }
      latch.await()

      val temp = myNew
      myNew = myVal
      myVal = temp
    }
    myVal
  }

  /**
    * Parallel version using Cyclic Barrier
    */
  def parWithCyclicBarrier(iter: Int, arr: Array[Double]) = {
    //import scala.concurrent.ExecutionContext.Implicits.global
    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(arr.length-2))
    //defensive copy to stop arr from being mutated outside this method
    var (myVal,myNew) = (arr.clone(),arr.clone())

    val barrier = new CyclicBarrier(arr.length-2)
    Await.ready(Future.sequence(
      for (i <- 1 until arr.length - 1) yield
        Future {
          var (localVal,localNew) = (myVal,myNew)

          for (_ <- 1 to iter) {
            localNew(i) = (localVal(i - 1) + localVal(i + 1)) / 2.0

            barrier.await()

            val temp = localNew
            localNew = localVal
            localVal = temp
          }
        }), 10 seconds)

    myVal
  }

  /**
    * Parallel version using Phasers
    */
  def parWithPhasers(iter: Int, arr: Array[Double]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    //implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(arr.length-2))
    //defensive copy to stop arr from being mutated outside this method
    val (myVal,myNew) = (arr.clone(),arr.clone())
    val n = arr.length

    val ph: Array[Phaser] = Array.ofDim[Phaser](n) //array of phasers
    for (i <- ph.indices) ph(i) = new Phaser(1)

    Await.ready(Future.sequence(
      for (i <- 1 until n - 1) yield
        Future {
          var (localVal,localNew) = (myVal,myNew)

          for (_ <- 1 to iter) {
            localNew(i) = (localVal(i - 1) + localVal(i + 1)) / 2.0

            val phase = ph(i).arrive()
            if(i > 1) ph(i-1).awaitAdvance(phase)
            if(i < n-2) ph(i+1).awaitAdvance(phase)

            val temp = localNew
            localNew = localVal
            localVal = temp
          }
        }), 10 seconds)

    myVal
  }

  /**
    * Parallel version using Cyclic Barrier and grouping the elements of the array
    *
    * @param iter Number of iterations
    * @param g Number of groups in which the array is to be divided
    * @param arr Array with fixed boundary values. The other values are calculated as the average value of their neighbours
    * @return
    */
  def parWithCyclicBarrierAndGroups(iter: Int, g: Int, arr: Array[Double]) = {
    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(g))
    //defensive copy to stop arr from being mutated outside this method
    val (myVal,myNew) = (arr.clone(),arr.clone())

    val n = arr.length
    val barrier = new CyclicBarrier(g)

    Await.ready(Future.sequence(

      for ((_,leftMostElement,rightMostElement) <- euler.groups(n,g)) yield
        Future {
          var (localVal,localNew) = (myVal,myNew)

          for (_ <- 1 to iter) {
            for (i <- leftMostElement to rightMostElement) {
              if(i > 0 && i < n-1)
                localNew(i) = (localVal(i - 1) + localVal(i + 1)) / 2.0
            }
            barrier.await()

            val temp = localNew
            localNew = localVal
            localVal = temp
          }
        }), 10 seconds)

    myVal
  }


  /**
    * Parallel version using Phasers and grouping the elements of the array
    */
  def parWithPhasersAndGroups(iter: Int, g: Int, arr: Array[Double]) = {
    //import scala.concurrent.ExecutionContext.Implicits.global
    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(g))
    //defensive copy to stop arr from being mutated outside this method
    val (myVal,myNew) = (arr.clone(),arr.clone())
    val n = arr.length

    val ph: Array[Phaser] = Array.ofDim[Phaser](g) //array of phasers
    for (i <- ph.indices) ph(i) = new Phaser(1)

    Await.ready(Future.sequence(
      for ((j,leftMostElement,rightMostElement) <- euler.groups(n,g)) yield
        Future {
          var (localVal,localNew) = (myVal,myNew)

          for (_ <- 1 to iter) {
            for (i <- leftMostElement to rightMostElement) {
              if(i > 0 && i < n-1)
                localNew(i) = (localVal(i - 1) + localVal(i + 1)) / 2.0
            }

            val phase = ph(j).arrive()

            if(j > 0) ph(j-1).awaitAdvance(phase)
            if(j < g - 1) ph(j+1).awaitAdvance(phase)

            val temp = localNew
            localNew = localVal
            localVal = temp
          }
        }), 10 seconds)

    myVal
  }

  /**
    * This version is optimised with the use of fuzzy barriers.
    */
  def parWithFuzzyBarrier(iter: Int, g: Int, arr: Array[Double]) = {
    //import scala.concurrent.ExecutionContext.Implicits.global
    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(g))
    //defensive copy to stop arr from being mutated outside this method
    val (myVal,myNew) = (arr.clone(),arr.clone())
    val n = arr.length

    val ph: Array[Phaser] = Array.ofDim[Phaser](g) //array of phasers
    for (i <- ph.indices) ph(i) = new Phaser(1)

    Await.ready(Future.sequence(
      for ((j,leftMostElement,rightMostElement) <- euler.groups(n,g)) yield
        Future {
          var (localVal,localNew) = (myVal,myNew)

          for (_ <- 1 to iter) {

            if(leftMostElement > 0 && leftMostElement < n-1) localNew(leftMostElement) =
              (localVal(leftMostElement - 1) + localVal(leftMostElement + 1)) / 2.0
            if(rightMostElement > 0 && rightMostElement < n - 1) localNew(rightMostElement) =
              (localVal(rightMostElement - 1) + localVal(rightMostElement + 1)) / 2.0

            val phase = ph(j).arrive()

            for (i <- leftMostElement + 1 to rightMostElement - 1) {
              if(i < n-1)
                localNew(i) = (localVal(i - 1) + localVal(i + 1)) / 2.0
            }

            if(j > 0) ph(j-1).awaitAdvance(phase)
            if(j < g - 1) ph(j+1).awaitAdvance(phase)

            val temp = localNew
            localNew = localVal
            localVal = temp
          }
        }), 10 seconds)

    myVal
  }


  def twoDimAvg(iter: Int, arr: Array[Array[Double]]) = {
    var (myVal, myNew) = (arr.map(_.clone()),arr.map(_.clone()))

    for(_ <- 1 to iter) {
      for (
        i <- 1 until arr.length - 1;
        j <- 1 until arr(i).length - 1
      ) myNew(i)(j) = (myVal(i-1)(j) + myVal(i+1)(j) + myVal(i)(j-1) + myVal(i)(j+1))/4

      val temp = myNew
      myNew = myVal
      myVal = temp
      println(temp.map(_.mkString(",")).mkString("\n"))
      println("\n")
    }
    myVal
  }


}
