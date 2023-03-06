package LU_Decomposition

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Matrix_Inversion_test_39stage extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "LU_decomposition"
  it should "produce right output" in {
    test(new Matrix_inversion_4stage(39,16,8)).withAnnotations(Seq(WriteVcdAnnotation)){ c =>

      val Matrix_stage = 39
      val width = 16
      val point = 8

      //39阶矩阵测试
      val my_matrix = Array(
        Array(5,1,7,5,3,2,2,5,2,3,1,4,4,5,7,1,5,4,5,2,7,3,2,1,6,4,4,7,1,7,6,5,1,3,4,2,7,7,6),
        Array(4,2,5,3,6,2,5,6,7,7,1,6,4,4,5,1,2,3,1,3,6,3,1,1,1,4,4,7,2,5,1,7,6,7,3,4,4,6,2),
        Array(1,2,3,6,2,3,7,2,3,4,4,5,6,5,3,3,5,2,3,7,7,4,6,1,4,5,2,5,1,5,1,6,1,1,2,3,2,1,5),
        Array(4,2,2,3,4,6,3,2,1,1,5,1,5,5,6,4,3,3,7,3,3,2,2,3,3,2,5,2,6,4,2,1,1,1,7,4,2,4,7),
        Array(3,4,5,6,1,5,6,6,3,1,1,3,1,4,7,1,5,7,5,3,5,3,7,2,4,1,4,2,5,3,1,6,7,5,6,1,5,1,2),
        Array(5,7,7,5,6,5,5,2,6,5,4,6,1,1,1,1,7,7,6,6,1,4,2,7,3,7,6,2,5,7,7,6,4,1,6,7,1,3,7),
        Array(7,3,5,4,4,2,5,3,2,5,6,2,2,4,1,1,2,2,5,6,2,2,2,1,4,1,5,5,5,7,6,1,3,1,1,5,6,5,2),
        Array(2,4,3,1,3,6,2,3,1,3,1,4,7,1,1,2,4,6,4,1,4,2,4,2,7,4,4,1,6,6,4,1,6,5,7,3,1,7,5),
        Array(1,7,4,4,2,3,1,4,1,4,5,5,5,1,3,7,5,6,7,7,4,4,5,4,3,3,5,3,1,4,6,2,7,4,4,5,4,5,7),
        Array(6,4,4,7,3,5,6,1,7,5,7,1,7,2,4,7,2,1,5,4,4,2,7,3,1,1,7,5,5,2,7,5,5,6,2,5,6,6,1),
        Array(6,3,2,1,2,3,6,5,1,2,7,1,3,7,5,5,2,7,3,7,1,6,5,6,6,4,7,7,7,6,1,5,4,7,7,2,4,1,1),
        Array(5,2,7,1,7,5,2,5,7,3,4,4,5,2,3,5,7,4,3,5,4,1,4,2,6,3,3,7,7,4,7,1,4,3,3,7,5,2,2),
        Array(6,7,2,7,7,7,3,2,7,4,3,7,5,7,4,6,7,2,4,4,4,6,1,2,6,1,3,6,4,2,2,2,5,6,2,3,2,4,3),
        Array(3,7,2,2,5,4,4,7,4,3,2,6,6,4,2,5,4,1,7,5,7,6,5,7,4,4,4,6,6,6,6,6,3,4,6,1,2,6,7),
        Array(4,4,6,2,3,2,2,2,1,4,4,6,1,1,6,7,5,5,3,5,3,7,1,1,4,1,6,3,4,4,1,3,1,2,5,7,4,3,5),
        Array(1,2,7,3,6,2,6,6,7,1,1,3,7,1,2,7,3,4,4,5,3,4,7,1,4,2,5,4,6,7,7,3,2,7,3,5,7,1,4),
        Array(4,1,3,6,2,4,4,4,7,3,1,7,2,1,3,4,5,2,3,6,6,5,7,4,1,5,2,7,3,7,4,2,5,6,4,4,5,6,1),
        Array(4,1,5,5,7,5,1,5,6,3,7,5,5,6,3,4,1,3,4,5,5,4,7,5,3,4,3,6,2,7,6,6,4,6,7,3,4,2,4),
        Array(4,5,2,6,5,4,3,4,6,3,7,2,7,3,3,3,4,2,2,5,4,6,5,5,4,4,4,2,5,6,2,7,6,3,6,7,7,6,2),
        Array(2,5,5,6,1,5,3,3,1,4,5,4,7,2,7,6,7,6,3,5,6,3,5,7,7,1,3,5,2,1,6,2,5,7,1,1,1,1,3),
        Array(3,6,6,2,5,7,6,6,6,3,7,1,3,7,1,1,2,1,1,6,4,2,3,1,4,2,5,4,4,1,5,1,6,4,5,3,1,5,5),
        Array(1,7,1,5,3,1,6,6,3,2,7,1,1,5,1,2,4,6,5,3,7,3,5,6,6,4,1,7,7,2,2,4,1,2,2,6,1,4,1),
        Array(3,6,4,1,1,6,5,5,1,6,7,3,4,3,4,3,7,4,5,2,6,6,1,3,4,5,4,3,6,3,1,6,4,6,3,7,7,6,1),
        Array(3,2,6,5,2,2,3,2,3,4,7,4,2,6,1,2,3,7,4,2,4,7,1,6,7,7,5,4,1,7,2,5,4,2,1,6,3,5,6),
        Array(3,7,4,3,1,6,5,5,5,7,7,4,1,2,5,3,6,5,4,1,6,4,4,7,3,3,7,6,2,3,6,1,5,5,4,3,7,5,1),
        Array(5,6,7,2,6,2,5,7,4,2,2,2,4,6,2,7,4,1,2,3,6,2,3,7,7,2,4,4,2,2,2,7,6,7,1,6,1,6,2),
        Array(7,6,2,5,1,3,3,3,3,2,2,4,6,6,2,2,4,1,5,5,5,5,7,3,1,6,4,4,5,7,3,6,2,6,2,6,4,7,5),
        Array(6,4,5,7,1,5,1,5,1,6,4,1,3,4,7,6,5,4,1,3,2,6,2,1,6,5,1,4,4,6,4,6,3,6,1,7,3,6,7),
        Array(7,5,4,4,1,5,5,3,6,5,6,6,4,6,5,5,2,3,1,6,7,3,4,4,7,3,5,5,7,4,2,7,3,2,6,3,2,2,2),
        Array(1,5,7,3,7,7,2,1,5,1,1,2,6,4,4,7,1,4,1,2,1,1,4,3,7,1,5,2,5,4,1,6,2,4,1,3,4,1,2),
        Array(5,4,2,7,7,3,1,4,3,7,7,1,3,3,2,1,3,7,5,6,1,4,7,5,4,2,3,1,1,7,7,6,3,6,1,5,2,6,7),
        Array(6,2,2,1,5,6,3,7,3,5,7,4,2,6,2,4,2,4,6,6,3,3,4,1,3,6,3,2,7,1,1,6,6,2,1,5,5,6,1),
        Array(6,6,2,6,5,5,2,4,7,7,1,2,7,1,5,4,6,2,3,6,7,2,1,1,7,1,7,3,2,7,1,4,1,6,7,2,3,3,5),
        Array(1,3,6,6,2,2,5,4,3,7,3,1,5,4,2,3,1,5,1,2,7,1,6,7,4,2,6,7,7,7,3,2,6,7,4,3,4,1,7),
        Array(4,7,7,6,7,1,3,5,5,2,1,1,3,3,5,1,5,2,6,2,2,1,6,5,6,3,1,6,3,3,4,5,3,1,6,6,2,1,2),
        Array(6,4,7,1,1,1,2,3,7,4,3,3,3,4,1,7,2,3,3,5,1,5,4,6,1,2,4,5,6,2,3,1,3,2,2,5,6,1,7),
        Array(3,1,7,5,2,6,2,1,7,5,4,1,2,6,7,7,5,3,3,7,2,4,6,1,3,2,1,6,7,1,2,2,4,2,3,2,6,5,1),
        Array(1,6,1,6,1,3,5,2,2,3,3,7,7,3,7,4,3,1,5,4,3,4,5,3,1,2,2,3,4,6,2,2,7,5,7,3,3,2,2),
        Array(1,4,3,6,2,2,4,1,4,7,5,4,4,7,6,6,7,1,3,1,2,1,1,3,3,4,2,5,7,4,5,5,1,5,6,2,1,7,4),
        )

        var count = 0
        c.io.LU_state.poke(c.clear)
        c.clock.step(1)
        while (c.io.re_stage.peek().litValue != c.idle.litValue) {
          if (c.io.re_stage.peek().litValue == c.load.litValue) {
            if (count < Matrix_stage) {
              val inputs = my_matrix(count).map { a => FixedPoint.fromDouble(a, width.W, point.BP) }
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