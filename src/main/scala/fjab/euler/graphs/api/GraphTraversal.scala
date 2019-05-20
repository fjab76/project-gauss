package fjab.euler.graphs


trait GraphTraversal[T] {

  type Vertex = T
  //Path to a Vertex
  type Path = List[Vertex]

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
  def findPath(seed: Seq[Path]): Path

  /**
    * Calculate the neighbours of the given vertex.
    * Implementations of this method depend on the nature of the graph: moves allowed from one vertex to another, 
    * constraints, etc.
    */
  def neighbours(vertex: Vertex): List[Vertex]


  /**
    * Condition to be met by the path. The condition will be different for each particular problem, e.g.:
    * - path that passes through an specific vertex (condition used to find a path connecting 2 vertices)
    * - path that passes through all the vertices (condition used to find the solution of problems like the Knight's tour)
    */
  def isSolution(path: Path): Boolean

  /**
    * This method determines if the given vertex is to be added to the path. The implementation will depend on the
    * characteristics of the problem to solve.
    *
    * In most cases, it will be necessary to discard a vertex that is already included in the present path.
    */
  def isVertexEligibleForPath(vertex: Vertex, path: Path): Boolean

}

