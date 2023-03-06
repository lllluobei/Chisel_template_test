package LU_Decomposition

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Matrix_Inversion_test_4stage extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "LU_decomposition"
  it should "produce right output" in {
    test(new Matrix_inversion_4stage(4,16,8)).withAnnotations(Seq(WriteVcdAnnotation)){ c =>

      val Matrix_stage = 4
      val width = 16
      val point = 8

      //4阶矩阵测试
      //val my_matrix = Array(Array(5,1,4,3), Array(2,5,7,5), Array(1,4,4,5),Array(2,3,3,4))
      val my_matrix = Array(
        Array(Array(4,3,2,2), Array(7,6,7,6), Array(4,4,6,2),Array(5,6,5,7)),
        Array(Array(2,5,3,5), Array(4,1,4,2), Array(5,5,6,5),Array(7,1,2,5)),
        Array(Array(1,2,1,3), Array(7,5,7,4), Array(1,7,1,5),Array(5,6,6,3)),
        Array(Array(1,6,5,3), Array(4,6,2,2), Array(6,5,1,1),Array(2,5,6,6)),
        Array(Array(5,5,3,2), Array(5,5,7,7), Array(3,3,2,2),Array(3,5,1,3)))
      /*
      val my_matrix_2 = Array(Array(2,5,3,5), Array(4,1,4,2), Array(5,5,6,5),Array(7,1,2,5))
      val my_matrix_3 = Array(Array(1,2,1,3), Array(7,5,7,4), Array(1,7,1,5),Array(5,6,6,3))
      val my_matrix_4 = Array(Array(1,6,5,3), Array(4,6,2,2), Array(6,5,1,1),Array(2,5,6,6))
      val my_matrix_5 = Array(Array(5,5,3,2), Array(5,5,7,7), Array(3,3,2,2),Array(3,5,1,3))
       */

      for(i <- 0 until 5) {
        var count = 0
        c.io.LU_state.poke(c.clear)
        c.clock.step(1)
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