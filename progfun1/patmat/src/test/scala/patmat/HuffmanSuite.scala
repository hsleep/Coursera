package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val c1 = List('a' -> List(0), 'b' -> List(1))
    val c2 = List('a' -> List(0, 0), 'b' -> List(0, 1), 'd' -> List(1))
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('y', 5))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('y', 5)))
  }


  test("createCodeTree optimal") {
    val t = Fork(
      Leaf('a',8),
      Fork(
        Fork(
          Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),
          Fork(Leaf('g',1),Leaf('c',1),List('g', 'c'),2),
          List('e', 'f', 'g', 'c'),
          4
        ),
        Fork(
          Fork(Leaf('h',1),Leaf('d',1),List('h', 'd'),2),
          Leaf('b',3),
          List('h', 'd', 'b'),
          5
        ),
        List('e', 'f', 'g', 'c', 'h', 'd', 'b'),
        9
      ),
      List('a', 'e', 'f', 'g', 'c', 'h', 'd', 'b'),
      17
    )
    assert(createCodeTree(string2Chars("aaaaaaaabbbcdefgh")) === t)
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("encode") {
    val chars = string2Chars("huffmanestcool")
    assert(encode(frenchCode)(chars) === secret)
    val newCodeTree = createCodeTree(chars)
    assert(decode(newCodeTree, encode(newCodeTree)(chars)) === string2Chars("huffmanestcool"))
  }


  test("decodedSecret is huffmanestcool") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }


  test("convert t1") {
    new TestTrees {
      assert(convert(t1) === c1)
    }
  }


  test("convert t2") {
    new TestTrees {
      assert(convert(t2) === c2)
    }
  }


  test("quick encode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("quick encode huffmanestcool") {
    assert(secret === quickEncode(frenchCode)(string2Chars("huffmanestcool")))
  }
}
