package streams

import munit.FunSuite
import streams.Bloxorz.Level0.Pos
import streams.Bloxorz.Level0

class StringParserTerrainTest extends FunSuite:
  test("terrain for level 0") {
    assert(Level0.terrain(Pos(1, 2)))
    assert(Level0.terrain(Pos(2, 2)))
    assert(!Level0.terrain(Pos(0, 0)))
    assert(!Level0.terrain(Pos(1, 0)))
    assert(!Level0.terrain(Pos(4, 5)))
  }

  test("start pos for level 0") {
    assertEquals(Pos(1, 2), Level0.startPos)
  }

  test("goal pos for level 0") {
    assertEquals(Pos(1, 3), Level0.goal)
  }
