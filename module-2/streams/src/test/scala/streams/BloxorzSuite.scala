package streams

import Bloxorz.*

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal, s"illegal block ${block}") // The solution must always lead to legal blocks
        move match
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
    }

  trait Level1 extends SolutionChecker:
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)


  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }


  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }

  test("is standing") {
    new Level1:
      assert(Block(Pos(1,1), Pos(1,1)).isStanding)
      assert(!Block(Pos(1,1), Pos(1,2)).isStanding)
  }

  test("is legal") {
    new Level1:
      assert(Block(Pos(0,0), Pos(0,1)).isLegal, s"legal block(${Block(Pos(2,0), Pos(2,0))} position failed")
      assert(Block(Pos(2,0), Pos(2,0)).isLegal, s"legal block(${Block(Pos(2,0), Pos(2,0))} position failed")
      assert(!Block(Pos(-1,0), Pos(0,1)).isLegal, s"illegal block(${Block(Pos(-1,0), Pos(0,1))} position passed")
      assert(!Block(Pos(2,0), Pos(3,0)).isLegal, s"illegal block(${Block(Pos(2,0), Pos(3,0))} position passed")
  }

  test("start block") {
    new Level1:
      assertEquals(startBlock, Block(Pos(1,1), Pos(1,1)), "start block not in correct position")
  }

  test("neighbors of block") {
    new Level1:
      private val b: Block = Block(Pos(1,1), Pos(1,1))
      assertEquals(b.neighbors, List((b.left, Move.Left), (b.right, Move.Right), (b.up, Move.Up), (b.down, Move.Down)))
      assertEquals(b.legalNeighbors, List((b.right, Move.Right), (b.down, Move.Down)))
  }

  test("done") {
    new Level1:
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
  }

  test("neighbors with history") {
    new Level1:
      import Move.*
      private val exp: Set[(Block, List[Move])] = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assertEquals(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left, Up)).to(Set), exp)
  }

  test("new neighbors only") {
    new Level1:
      import Move.*
      private val exp: Set[(Block, List[Move])] = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      private val allNeighbors = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left, Up))
      private val explored = Set(Block(Pos(1,2),Pos(1,3)))
      assertEquals(newNeighborsOnly(allNeighbors, explored).to(Set), exp)
  }
  
  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
