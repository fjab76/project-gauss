package fjab.euler.graphs.api

import fjab.euler.graphs.GraphTraversal

import scala.collection.mutable.ListBuffer

/**
  *
  */
trait MutableGraphTraversal[T] extends GraphTraversal[T]{

  /**
    * This method traverses the graph and as it does so, builds all possible paths until finding the solution 
    * as per the definition of the method 'isSolution'.
    *
    * The algorithm to traverse the graph, depth-first or breadth-first, is determined by the method 'addNeighbours'.
    *
    * The process to calculate the neighbours of a given vertex is specific for every type of graph and is implemented
    * by the method 'neighbours'. The method 'isVertexEligibleForPath' filters the neighbours that are eligible to be
    * included in the present path (for instance, in most cases it is desirable not to visit twice through the same vertex).
    *
    * It is worth noticing that the present algorithm to traverse the graph is very generic and therefore
    * sub-optimal for problems like finding the path between 2 vertices as the
    * different paths do not share information about the vertices visited, resulting in different paths visiting vertices
    * that are already known not to be part of the solution.
    *
    *
    * @param seed Initial vertices used to calculate the possible paths
    * @return Path The searched path or Nil if the desired path does not exist
    */
  override def findPath(seed: Seq[Path]): Path = {

    val paths: ListBuffer[Path] = ListBuffer() ++= seed

    def next(): Path = paths.headOption match{
      case None => Nil
      case Some(currentVertexPath) =>
        if(isSolution(currentVertexPath)) currentVertexPath.reverse
        else {
          val currentVertex = currentVertexPath.head
          val neighbourVertices = neighbours(currentVertex).filter(isVertexEligibleForPath(_, currentVertexPath))
          val pathsToNeighbourVertices = neighbourVertices.map(_ :: currentVertexPath)
          paths.remove(0)
          addNeighbours(paths, pathsToNeighbourVertices)
          next()
        }
    }

    next()

  }

  /**
    * This method adds the neighbours of the current vertex to the list of remaining vertices to explore.
    * Depending on whether the new paths are added in front of the list or at the end, the resulting traversing
    * algorithm will be depth-first or breadth-first respectively.
    *
    * Breadth-first algorithms are necessary to find the shortest path between 2 vertices or when looking
    * for paths in an infinite graph
    *
    */
  def addNeighbours(verticesToExplore: ListBuffer[Path], neighbours: List[Path]): Unit

}

