package LU_Decomposition

import chisel3._
import org.scalatest._
import chiseltest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.FixedPoint

class Matrix_Inversion_test_3stage extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "LU_decomposition"
  it should "produce right output" in {
    test(new Matrix_inversion_4stage(3,16,8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      val Matrix_stage = 3
      val width = 16
      val point = 8

      //3阶矩阵测试
      val my_matrix = Array(
        Array(Array(15, 8, 10), Array(11, 12, 2), Array(11, 2, 12)),
        Array(Array(9, 9, 12), Array(5, 3, 8), Array(9, 10, 10)),
        Array(Array(8, 14, 3), Array(4, 14, 10), Array(3, 14, 4)),
        Array(Array(6, 13, 1), Array(9, 14, 4), Array(13, 14, 5)),
        Array(Array(10, 3, 7), Array(3, 8, 13), Array(15, 5, 14)))
      //val my_matrix = Array(Array(1,2,3), Array(3,2,1), Array(1,4,2))
      //val my_matrix = Array(Array(3,4,1), Array(5,7,6), Array(2,7,3))
      //val my_matrix = Array(Array(7,5,3), Array(2,3,4), Array(1,6,5))
      //val my_matrix = Array(Array(2,1,2), Array(7,3,2), Array(5,6,3))
      //val my_matrix = Array(Array(3,2,6), Array(4,2,4), Array(1,7,7))

      for (i <- 0 until 2 ) {
        c.io.LU_state.poke(c.clear)
        c.clock.step(1)

        var count = 0
        while (c.io.re_stage.peek().litValue != c.idle.litValue) {
          if (c.io.re_stage.peek().litValue == c.load.litValue) {
            if (count < Matrix_stage) {
              val inputs = my_matrix(i)(count).map { a => FixedPoint.fromDouble(a, width.W, point.BP) }
              for (j <- 0 until Matrix_stage) c.io.in_seq(j).poke(inputs(j))
              count = count + 1
            }
            c.clock.step(1)
          }
          else {
            c.clock.step(1)
          }
        }
      }
    }

  }
}