package org.springscala.ruzzle.solve

import java.io.InputStream

import scala.io.Source

object RuzzleSolver {

  // Alphabet size
  val SIZE = 26
  val M = 4
  val N = 4
  val classLoader: ClassLoader = getClass.getClassLoader
  val stream : InputStream = getClass.getResourceAsStream("/words.txt")
  val dictionary = Source.fromInputStream(stream).getLines.toArray
  var stringCombination = ""

  val n = dictionary.length
  val root = new TrieNode
  for (i <- 0 until n)
    insert(root, dictionary(i))

  // If not present, inserts a key into the trie
  // If the key is a prefix of trie node, just marks leaf node
  def insert(root: TrieNode, Key: String): Unit = {
    val n = Key.length
    var pChild = root

    for (i <- 0 until n) {
      val index = Key.charAt(i) - 'A'
      if (pChild.Child(index) == null)
        pChild.Child(index) = new TrieNode
      pChild = pChild.Child(index)
    }

    // make last node as leaf node
    pChild.leaf = true
  }

  // function to check that current location (i and j) is in matrix range
  def isSafe(i: Int, j: Int, visited: Array[Array[Boolean]]): Boolean = {
    (i >=0 && i < M && j >=0 &&  j < N && !visited(i)(j))
  }

  // A recursive function to print all words present on boggle
  def searchWord(root: TrieNode, boggle: Array[Array[Char]], i: Int, j: Int,
                 visited: Array[Array[Boolean]], str: String): Unit = {
    // if we found word in trie / dictionary
    if (root.leaf == true){
      stringCombination += str + "\n"
    }

    // If both I and j in  range and we visited that element of matrix first time
    if (isSafe(i, j, visited)) {
      // make it visited
      visited(i)(j)

      // traverse all child of current root
      for (k <- 0 until SIZE) {
        if (root.Child(k) != null) {
          // current character
          val ch = (k + 'A').asInstanceOf[Char]

          // Recursively search remaining character of word in trie for 8 adjacent cells of boggle[i][j]
          // (make it 15)
          if (isSafe(i+1,j+1,visited) && boggle(i+1)(j+1) == ch)
            searchWord(root.Child(k),boggle,i+1,j+1, visited,str+ch)
          if (isSafe(i, j+1,visited)  && boggle(i)(j+1) == ch)
            searchWord(root.Child(k),boggle,i, j+1, visited,str+ch)
          if (isSafe(i-1,j+1,visited) && boggle(i-1)(j+1) == ch)
            searchWord(root.Child(k),boggle,i-1, j+1, visited,str+ch)
          if (isSafe(i+1,j, visited) && boggle(i+1)(j) == ch)
            searchWord(root.Child(k),boggle,i+1, j, visited,str+ch)
          if (isSafe(i+1,j-1,visited) && boggle(i+1)(j-1) == ch)
            searchWord(root.Child(k),boggle,i+1, j-1, visited,str+ch)
          if (isSafe(i, j-1,visited)&& boggle(i)(j-1) == ch)
            searchWord(root.Child(k),boggle,i,j-1, visited,str+ch)
          if (isSafe(i-1,j-1,visited) && boggle(i-1)(j-1) == ch)
            searchWord(root.Child(k),boggle,i-1, j-1, visited,str+ch)
          if (isSafe(i-1, j,visited) && boggle(i-1)(j) == ch)
            searchWord(root.Child(k),boggle,i-1, j, visited,str+ch)
        }
      }
      // make current element unvisited
      visited(i)(j) = false;
    }
  }

//   Prints all words present in dictionary.
  def findWords(boggle: Array[Array[Char]], root: TrieNode): String = {
    // Mark all characters as not visited
    val visited = Array.ofDim[Boolean](M, N)
    var pChild = root
    var str = ""

    // traverse all matrix elements
    for (i <- 0 until M)
      for (j <- 0 until N)
        //we start searching for word in dictionary if we found a character which is child of Trie root
        if (pChild.Child(boggle(i)(j) - 'A') != null) {
          str = str+boggle(i)(j)
          searchWord(pChild.Child(boggle(i)(j) - 'A'), boggle, i, j, visited, str)
          str = ""
        }

    stringCombination
  }

}

class TrieNode {
  val Child = new Array[TrieNode](RuzzleSolver.SIZE)
  // isLeaf is true if the node represents end of a word
  var leaf = false
  for (i <- 0 until RuzzleSolver.SIZE)
    Child(i) = null
}
