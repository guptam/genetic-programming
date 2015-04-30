import scala.math._

object genetic
{

  // generic mathematical function wrapper
  class FunctionWrapper (val myFunctionBody: List[Any]=>Any, val myInputParamCount: Int, val myFunctionName: String) {
    
  }


  // Functions for GP we will use as of now
  val addition    		= new FunctionWrapper((p: List[Any]) =>p(0).asInstanceOf[Int] + p(1).asInstanceOf[Int], 2, "+")
  val subtraction   	= new FunctionWrapper((p: List[Any]) =>p(0).asInstanceOf[Int] - p(1).asInstanceOf[Int], 2, "-")
  val multiplication  	= new FunctionWrapper((p: List[Any]) =>p(0).asInstanceOf[Int] * p(1).asInstanceOf[Int], 2, "*")
  val division    		= new FunctionWrapper((p: List[Any]) => if (p(1).asInstanceOf[Int] == 0) 0 else p(0).asInstanceOf[Int] / p(1).asInstanceOf[Int], 2, "/")
  val ifX       		= new FunctionWrapper((p: List[Any]) => if (p(0).asInstanceOf[Int] > 0) p(1).asInstanceOf[Int] else p(2).asInstanceOf[Int], 3, "if")
  val isGreater   		= new FunctionWrapper((p: List[Any]) => if (p(0).asInstanceOf[Int] > p(1).asInstanceOf[Int]) 1 else 0, 2, ">")
  val isEqual     		= new FunctionWrapper((p: List[Any]) => if (p(0).asInstanceOf[Int] == p(1).asInstanceOf[Int]) 1 else 0, 2, "==")


  // List of possible functions
  val functionList = List (addition, subtraction, multiplication, ifX, isGreater, isEqual)


  // Abstract because we will be creating a recursive structure on Child Class FunctionNode
  case class Node() 
  {
    val spacer = " "
    val noder =   "\\"
    val stemmer = " |"

    def printToString(paramlist: List[Any], indent: String=" ") 
    {
      print(indent)
    }
    def display(indent: Int=0) 
    {
      print(spacer*indent)
    }
    def flat() 
    {
      print("")
    }
    def evaluate(paramlist: List[Any]): Any = {0}
  }


  // Main class for Function Nodes
  class FunctionNode(val func: FunctionWrapper, var children: List[Node]) extends Node() 
  {
    val name = func.myFunctionName
    val function = func.myFunctionBody

    override def evaluate(paramlist: List[Any]): Any =
      function(
        for (child <- children)
          yield child.asInstanceOf[Node].evaluate(paramlist)
             )

    // print without evaluate
    override def display(indent: Int=0) {
      println(spacer*indent + "" + name)     
      for (child <- children.dropRight(1)) 
      {
        child.display(indent+1)
      }
      children.last.display(indent+1)
    }

    override def flat() 
    {
      print(name)
      for (child <- children.dropRight(1)) 
      {
        child.flat()
      }
      children.last.flat()
    }

     // print (with evaluated values)
    override def printToString(paramlist: List[Any], indent: String=" ") {
      super.printToString(paramlist, indent)
      println(noder + name + "=" + evaluate(paramlist))
      for (child <- children.dropRight(1)) child.printToString(paramlist, indent + spacer * 2 + stemmer)
      children.last.printToString(paramlist, indent + spacer * 4)
    }

  }


  class ParamNode(paramid: Int) extends Node() {

   override def evaluate(paramlist: List[Any]): Any =
      paramlist(paramid).asInstanceOf[Any]


    override def display(indent: Int=0) {
      println(spacer*indent  + "" + paramToString(paramid))
    }

    override def flat() {
      print(paramToString(paramid))
    }

    override def printToString(paramlist: List[Any], indent: String=" ") {
      super.printToString(paramlist, indent)
      println(noder + paramToString(paramid) + "=" + evaluate(paramlist))
    }
    def paramToString(id: Int): String = {
      "p[" + id + "]"
      }
  }


  class ConstantNode(value: Any) extends Node() {

   override def evaluate(paramlist: List[Any]): Any =
      value

    override def display(indent: Int=0) {
      println(spacer*indent  + "" + value)
    }

    override def flat() {
      print(value)
    }

    override def printToString(paramlist: List[Any], indent: String=" ") {
      super.printToString(paramlist, indent)
      println(noder + value)
    }
  }


  def selectAny[A](stuff: List[A]): A = {
    stuff(util.Random.nextInt(stuff.length))

  }



  class SyntaxTree
  (
    val numParam: Int,                    					// maximum list index to go upto
    val funcList: List[FunctionWrapper] = functionList,   	// predefined set of functions to use
    val maxDepth: Int = 3,                  				// initial tree maximum depth
    val prFunc: Float = 0.5f,                 				// Prob of getting a function node
    val prParam: Float = 0.6f,                				// Prob of getting a terminal node
    val constFunc: ()=>Any=()=>util.Random.nextInt(10),   	// set of constants (between 0 and 100)
    var root: Node = null                   				// starting
  ) 
  {

     // this is all constructor code
    // by default, return a random syntax tree
      if (root==null) {
        root = get_random_tree()
      }

      // create a random syntax tree comprising of functions and constants and params
      def get_random_tree(depth: Int=0, isThisRoot: Boolean=true): Node = {

        // get a random number (used for stochastic calculations) 
        val roll = util.Random.nextFloat()

        if (isThisRoot || ((roll < prFunc) && (depth < maxDepth))) {

            // get a random function from funcList
            val newfunc = selectAny(funcList)

            // Recusively create children subtrees.
            var children = for (i <- 1 to newfunc.myInputParamCount)
                              yield get_random_tree(depth+1,false)

            // Wrap it up in an FunctionNode and return.
            return new FunctionNode(newfunc, children.toList.asInstanceOf[List[Node]])

          } else if (roll < prParam) {
            // Make a parameter node.
            new ParamNode(util.Random.nextInt(numParam)) // any number between 0 and numParam-1

          } else {
            // Make a constant node.
            new ConstantNode(constFunc()) // random number between 0 and 1
          }
      }

      // -----------------------------------------------------------------------------------------------------------
      def mutate(probMut: Float=0.1f): SyntaxTree = {
        // start checking for mutation at root
        val newroot = subMutate(root, probMut)

        // new Syntax tree for the mutated child
        new SyntaxTree(numParam, funcList, maxDepth, prFunc, prParam, constFunc, newroot) //.clone.asInstanceOf[SyntaxTree]

      }

      // start traversing the tree from root to leaves. At each node, run a roulett wheel and check if it should 
      // be mutated. If you hit the jackpot, then return a random tree at that particular node.
      // We are mutating the master copy of tree (cloning is not taken care of in mutation function but at some
      // higher level function call)
    // ***** Function can mutate multiple children nodes ******
    // as of now the mutation is dumb as does not consider arity of the function etc.
      def subMutate(subtree: Node, probMut: Float=0.1f, depth: Int=0): Node = {
      	val t = subtree.copy
        if (util.Random.nextFloat() < probMut) {
          // Return a brand new subtree.
          get_random_tree(depth)
        } else 
        {
          // If this is a function node:
          // because: onction nodes have children
          if (t.isInstanceOf[FunctionNode]) 
          {
            // Mutate its children (with a probability):
            t.asInstanceOf[FunctionNode].children = for (child <- t.asInstanceOf[FunctionNode].children)
                                              yield (subMutate(child, probMut, depth+1))
          }
          // Return the current subtree, mutated or not.
          subtree
        }

      }
      // -----------------------------------------------------------------------------------------------------------



      // -----------------------------------------------------------------------------------------------------------
      // crossing create a new child (1 child for 2 parents)
      // By traversing both trees at once, the crossover happens at approximately the same level on each tree
      def crossbreed(otherroot: Node, probCross: Float=0.7f): SyntaxTree = {

        val newroot = subCrossBreed(root, otherroot, probCross)

        // new Syntax tree for the child
        new SyntaxTree(numParam, funcList, maxDepth, prFunc, prParam, constFunc, newroot) // .clone.asInstanceOf[SyntaxTree] 

      }

      def subCrossBreed(thisroot: Node, otherroot: Node, probCross: Float=0.7f, atroot: Boolean=true): Node = {
      	val t1 = thisroot.copy
      	val t2 = otherroot.copy
      	
        if ((!atroot) && (util.Random.nextFloat() < probCross)) {
          t2
        } 
        else {
          // See about crossing the childrens, if any:
          if (t1.isInstanceOf[FunctionNode] && t2.isInstanceOf[FunctionNode]) {
            // Randomly replace this node's children with the other node's children.
            t1.asInstanceOf[FunctionNode].children = for (child <- t1.asInstanceOf[FunctionNode].children)
                       yield (subCrossBreed(child, selectAny(t2.asInstanceOf[FunctionNode].children),probCross,false))
          }
        // Return the current root, whether crossed or not.
        t1
        }
      }
      // -----------------------------------------------------------------------------------------------------------



      def scoreAgainstData(data: List[List[Any]]): Int = {
        val scores = for (v <- data) yield score(v)
        (scores.sum/data.length).toInt
      }

      def score(v: List[Any]): Int= {
        val y: Int = v.last.asInstanceOf[Int]
        val s: Int = evaluate(v.dropRight(1)).asInstanceOf[Int] - y
        if (s < 0) -s else s
      }

      def evaluate(paramlist: List[Any]): Any = {
        root.evaluate(paramlist)
      }

      // print the tree (without evaluation)
      // start from root
      def display() {
        println("----------------------------") 
        root.display(1)
        println("----------------------------") 
      }


      def flat() {
        root.flat()
      }

        // print the tree (with evaluation)
      def printToString(paramlist: List[Any] = List()) {
        root.printToString(paramlist)
      }

  }


  // make population of random Syntax trees 
  // used in GenerateGeneration
  def generateOneRandomTree
  (
    numParam: Int,
    funcList: List[FunctionWrapper] = functionList,
    maxDepth: Int = 4,
    prFunc:   Float = 0.5f,
    prParam:  Float = 0.6f,
    constFunc: ()=>Any=()=>util.Random.nextInt(10)
    ): SyntaxTree = {
      new SyntaxTree(numParam, funcList, maxDepth, prFunc, prParam, constFunc)
    }


  // make population of random Syntax trees 
  def generateFirstPopulation
  (
    numTrees: Int,
    numParam: Int,
    funcList: List[FunctionWrapper] = functionList,
    maxDepth: Int = 4,
    prFunc:   Float = 0.5f,
    prParam:  Float = 0.6f,
    constFunc: ()=>Any=()=>util.Random.nextInt(10)
    ): List[SyntaxTree] = {
      for (i <- (0 to numTrees-1).toList) yield new SyntaxTree(numParam, funcList, maxDepth, prFunc, prParam, constFunc)
    }

  // calculate fitness of each tree in the population
  def scorePopulation(population: List[SyntaxTree], data: List[List[Int]]): List[(SyntaxTree,Int)] = 
  {
    population.map((tree)=> (tree, tree.scoreAgainstData(data))).sortBy(_._2)
  }


  // Returns a random number, tending towards lower numbers. The lower pexp
  // is, more lower numbers you will get
  def selectindex(pexp: Float=0.7f, populationSize: Int): Int = {
    val x = (log(util.Random.nextFloat())/log(pexp)).toInt
    if (x > populationSize-1) populationSize-1 
    else if (x < 0) return 0 else x
  }


  // For Integer Overflow shit
  def newAbs(i: Int): Int = {
  	if (i < 0 ) Integer.MAX_VALUE else i
  }


  /**
   * Given a population of trees and some data, make a new generation of this population by:
   * 1 - scoring each tree against this data
   * 2 - removing a proportion, p, of the population
   * 3 - crossbreeding the remaining trees randomly to create a new population
   */
  def generateGeneration(scoredPopulation: List[(SyntaxTree,Int)], paramCount: Int, populationSize: Int,
    probexp: Float=0.7f, 
      // The rate of decline in the probability of selecting lower-ranked programs. A
      // higher value makes the selection process more stringent, choosing only programs
      // with the best ranks to replicate.
    probnew: Float=0.1f,
      // The probability when building the new population that a completely new, random
      // program is introduced. probexp and probnew will be discussed further in the
      // upcoming section “The Importance of Diversity.”
    mutationrate: Float=0.3f, // The probability of a mutation, passed on to mutate.
    breedingrate: Float=0.1f // The probability of crossover, passed on to crossover.
    ): List[SyntaxTree] = 
  {


    // Make a list of trees sorted by increasing score.
    val sortedTreeScores = scoredPopulation.sortBy(x=>newAbs(x._2.asInstanceOf[Int])).map(_._1.asInstanceOf[SyntaxTree])

    // Population to be returned
    val newPopulation = new Array[SyntaxTree](populationSize) // scala.collection.mutable.MutableList[SyntaxTree]

	newPopulation(0) =  sortedTreeScores(0) //.asInstanceOf[SyntaxTree].clone().asInstanceOf[SyntaxTree]
	newPopulation(1) =  sortedTreeScores(1)// .asInstanceOf[SyntaxTree].clone().asInstanceOf[SyntaxTree]
	newPopulation(2) =  sortedTreeScores(2)// .asInstanceOf[SyntaxTree].clone().asInstanceOf[SyntaxTree]

    var i: Int = 3

    // Elitism: Top 2 performers always make it to the new generation
    //if (newPopulation.isEmpty) newPopulation ++= sortedTreeScores.take(2)

    // println("\nAfter Elite")
    // newPopulation.map(_.flat)
    while (i < populationSize) 
    {
        // rotating the roulette wheel for new tree
        if (util.Random.nextFloat() < probnew)
        {
          newPopulation(i) = generateOneRandomTree(paramCount)
        }
        else
        {
          val tree1 = sortedTreeScores(selectindex(probexp, populationSize))
          val tree2 = sortedTreeScores(selectindex(probexp, populationSize))
          
          // evolutionary roulette wheel to crossover & mutate
          val nextTree = tree1.crossbreed(tree2.root, breedingrate).mutate(mutationrate)
          newPopulation(i) = nextTree
       }

       i = i + 1
    }

    //println("\nFinal")
    //newPopulation.map(_.flat)

    // return the new population  
    newPopulation.toList.asInstanceOf[List[SyntaxTree]]

  }




  // start making generation after generations starting from initial population
  def evolve
  (
    population    	: List[SyntaxTree],
    populationSize  : Int,
    paramCount    	: Int,
    numgen      	: Int,
    data      		: List[List[Int]],
    gen       		: Int=1
  ):(SyntaxTree,Int)= {

    val score = scorePopulation(population, data).sortBy(x=>newAbs(x._2.asInstanceOf[Int]))
    val thisBest = score.head

    println(gen + "\t:\t" + thisBest._2)
    
    // if we have reached our goal 
    // Objectives : Either fitness reaches 0 or all generations are exhausted
    if ((thisBest._2==0) | (numgen <= 1)) (thisBest)
    else // no, we have not reached our goal
    {
      val newgen = generateGeneration(score, paramCount, populationSize)
      evolve(newgen, populationSize, paramCount, numgen-1, data, gen+1)
    }
  }



  /*****************************************************************
  -- Just a random data set generator
  *****************************************************************/
  def hiddenFunction(): List[Int] = {
      val x = util.Random.nextInt(21)
      val y = util.Random.nextInt(21)
      val z = x + y*y
      List(x,y,z)
  } 

  def buildRandomDataset(numRows:Int): List[List[Int]] = {
    val data = for (i <- (0 to numRows-1).toList) 
          yield hiddenFunction()
    data.asInstanceOf[List[List[Int]]]
    
  }
  /*****************************************************************/

  def main(args: Array[String]) 
  {
    // data to be tested upon
    val testdata = buildRandomDataset(50)

    val populationSize = 200
    val paramCount = 2
    
    // first population
    val firstPopulation = generateFirstPopulation(populationSize,paramCount)
      
      // start evolving (upto 50 generations)
    val finalPair =  evolve(firstPopulation, populationSize, paramCount, 50000, testdata)

    println("Best Tree Ever with Score of " + finalPair._2)
    finalPair._1.display()

  }
}
