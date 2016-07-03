package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = null
    val l1 = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val l2 = l1 ::: List('a', 'n', 'd', 't', 'h', 'i', 's', 'i', 's', 'l', 'o', 'n', 'g', 'e', 'r')
    val l3 = List()
    val s1 = "huffmanestcoolandthisislonger"
  }


  test("weight of a smaller tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("weight of an empty tree") {
    new TestTrees {
      assert(weight(t3) === 0)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("singleton is false non empty List") {
    new TestTrees {
      def x: List[CodeTree] = List(t1, t2)

      assert(singleton(x) == false)
    }
  }

  test("singleton is false null List") {
    new TestTrees {
      assert(singleton(null) == false)
    }
  }

  test("singleton is true single List") {
    new TestTrees {
      def x: List[CodeTree] = List(t3)

      assert(singleton(x) == true)
    }
  }

  test("times(\"hello, world\")") {
    assert(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) === List(('e', 1), (' ', 1), (',', 1), ('l', 3), ('h', 1), ('r', 1), ('w', 1), ('o', 2), ('d', 1))
    )
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of singleton leaf list") {
    val leaflist = List(Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4)))
  }

  test("combine of Nil") {
    val leafList = Nil
    assert(combine(leafList) === Nil)

  }

  test("until of code tree") {
    new TestTrees {
      val leaflist = List(t1, t2)
      assert(until(singleton, combine)(leaflist).size == 1)
    }
  }


  test("make code tree of arbitary chars") {
    assert(createCodeTree(List('h', 'e', 'l', 'l', 'o')) === Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('e', 1),
      Leaf('h', 1), List('e', 'h'), 2), List('o', 'e', 'h'), 3), List('l', 'o', 'e', 'h'), 5)
    )
  }

  test("decode secret") {
    new TestTrees {
      assert(decode(frenchCode, secret) === l1)
    }

  }

  test("encode secret") {
    new TestTrees {
      assert(encode(frenchCode)(l1) === secret)
    }

  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick encode secret") {
    new TestTrees {
      assert(quickEncode(frenchCode)(l1) === secret)
    }

  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode long list") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)(l2)) === l2)
    }
  }

  test("quick encode long list") {
    new TestTrees {
      assert(decode(frenchCode, quickEncode(frenchCode)(l2)) === l2)
    }
  }

  test("encode empty list") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)(l3)) === l3)
    }
  }
  test("quick encode empty list") {
    new TestTrees {
      assert(decode(frenchCode, quickEncode(frenchCode)(l3)) === l3)
    }
  }

}
