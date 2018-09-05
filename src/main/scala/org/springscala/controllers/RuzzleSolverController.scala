
package org.springscala.controllers

import org.springframework.http.{HttpHeaders, HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._
import org.springscala.ruzzle.solve.RuzzleSolver
import RuzzleSolver.{M, N}

@RestController
@RequestMapping(path = Array("/ruzzle"))
class RuzzleSolverController() {

  @GetMapping(path = Array("/solve/{str}"))
  def solve(@PathVariable str: String): ResponseEntity[String] = {

    val inputArr = str.toUpperCase.toCharArray
    var boggle = Array.ofDim[Char](4, 4)

    if (str.length != 16)
      return new ResponseEntity("Send sixteen characters with no spaces", new HttpHeaders, HttpStatus.BAD_REQUEST)
    else
      boggle = inputArr.grouped(4).toArray

    val output = RuzzleSolver.findWords(boggle, RuzzleSolver.root)
    new ResponseEntity(output, new HttpHeaders, HttpStatus.CREATED)
  }

}