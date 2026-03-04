package uk.ac.ed.dal
package structtensor
package codegen

import utils._
import Utils._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.ParallelTestExecution

import org.scalatest.Tag

val CodegenTestTag = Tag("CodegenTest")

class CodegenTest extends AnyFlatSpec with Matchers {
  "Codegen" should "generate code for SpMV_D without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_D.stur",
        "-o",
        "test_outputs/SpMV_D_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_D_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_D_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_D with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_D.stur",
        "-o",
        "test_outputs/SpMV_D_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_D_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_D_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_D without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_D_DataLayout.stur",
        "-o",
        "test_outputs/SpMV_D_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_D_wo_body_DataLayout.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_D_wo_body_DataLayout_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_D with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_D_DataLayout.stur",
        "-o",
        "test_outputs/SpMV_D_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_D_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/SpMV_D_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_UT without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_UT.stur",
        "-o",
        "test_outputs/SpMV_UT_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_D_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/SpMV_D_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_UT with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_UT.stur",
        "-o",
        "test_outputs/SpMV_UT_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_UT_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_UT_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_UT without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_UT_DataLayout.stur",
        "-o",
        "test_outputs/SpMV_UT_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_UT_wo_body_DataLayout.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_UT_wo_body_DataLayout_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for SpMV_UT with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "SpMV_UT_DataLayout.stur",
        "-o",
        "test_outputs/SpMV_UT_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/SpMV_UT_w_body_DataLayout.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/SpMV_UT_w_body_DataLayout_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PGLM without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PGLM.stur",
        "-o",
        "test_outputs/PGLM_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PGLM_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PGLM_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PGLM with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PGLM.stur",
        "-o",
        "test_outputs/PGLM_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/PGLM_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PGLM_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PGLM without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PGLM_DataLayout.stur",
        "-o",
        "test_outputs/PGLM_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PGLM_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/PGLM_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PGLM with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PGLM_DataLayout.stur",
        "-o",
        "test_outputs/PGLM_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PGLM_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/PGLM_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_DP without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_DP.stur",
        "-o",
        "test_outputs/THP_DP_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_DP_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_DP_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_DP with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_DP.stur",
        "-o",
        "test_outputs/THP_DP_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_DP_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_DP_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_DP without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_DP_DataLayout.stur",
        "-o",
        "test_outputs/THP_DP_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_DP_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_DP_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_DP with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_DP_DataLayout.stur",
        "-o",
        "test_outputs/THP_DP_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_DP_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_DP_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_I without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_I.stur",
        "-o",
        "test_outputs/THP_I_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_I_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_I_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_I with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_I.stur",
        "-o",
        "test_outputs/THP_I_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_I_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_I_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_I without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_I_DataLayout.stur",
        "-o",
        "test_outputs/THP_I_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_I_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_I_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_I with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_I_DataLayout.stur",
        "-o",
        "test_outputs/THP_I_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_I_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_I_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_J without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_J.stur",
        "-o",
        "test_outputs/THP_J_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_J_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_J_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_J with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_J.stur",
        "-o",
        "test_outputs/THP_J_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_J_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/THP_J_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_J without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_J_DataLayout.stur",
        "-o",
        "test_outputs/THP_J_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_J_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_J_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for THP_J with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "THP_J_DataLayout.stur",
        "-o",
        "test_outputs/THP_J_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/THP_J_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/THP_J_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_UT without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_UT.stur",
        "-o",
        "test_outputs/TTM_UT_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_UT_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_UT_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_UT with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_UT.stur",
        "-o",
        "test_outputs/TTM_UT_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_UT_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_UT_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_UT without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_UT_DataLayout.stur",
        "-o",
        "test_outputs/TTM_UT_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_UT_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_UT_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_UT with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_UT_DataLayout.stur",
        "-o",
        "test_outputs/TTM_UT_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_UT_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_UT_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_DP without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_DP.stur",
        "-o",
        "test_outputs/TTM_DP_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_DP_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_DP_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_DP with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_DP.stur",
        "-o",
        "test_outputs/TTM_DP_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_DP_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_DP_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_DP without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_DP_DataLayout.stur",
        "-o",
        "test_outputs/TTM_DP_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_DP_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_DP_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_DP with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_DP_DataLayout.stur",
        "-o",
        "test_outputs/TTM_DP_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_DP_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_DP_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_J without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_J.stur",
        "-o",
        "test_outputs/TTM_J_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_J_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_J_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_J with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_J.stur",
        "-o",
        "test_outputs/TTM_J_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_J_w_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/TTM_J_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_J without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_J_DataLayout.stur",
        "-o",
        "test_outputs/TTM_J_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_J_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_J_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for TTM_J with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "TTM_J_DataLayout.stur",
        "-o",
        "test_outputs/TTM_J_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/TTM_J_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/TTM_J_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_I without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_I.stur",
        "-o",
        "test_outputs/MTTKRP_I_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_I_wo_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_I_wo_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_I with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_I.stur",
        "-o",
        "test_outputs/MTTKRP_I_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_I_w_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_I_w_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_I without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_I_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_I_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_I_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_I_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_I with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_I_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_I_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_I_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_I_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_J without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_J.stur",
        "-o",
        "test_outputs/MTTKRP_J_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_J_wo_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_J_wo_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_J with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_J.stur",
        "-o",
        "test_outputs/MTTKRP_J_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_J_w_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_J_w_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_J without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_J_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_J_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_J_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_J_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_J with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_J_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_J_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_J_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_J_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_IJ without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_IJ.stur",
        "-o",
        "test_outputs/MTTKRP_IJ_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_IJ_wo_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_IJ_wo_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_IJ with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_IJ.stur",
        "-o",
        "test_outputs/MTTKRP_IJ_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_IJ_w_body.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_IJ_w_body_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_IJ without the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_IJ_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_IJ_wo_body_DataLayout_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_IJ_wo_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_IJ_wo_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for MTTKRP_IJ with the body with data layout compression" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "MTTKRP_IJ_DataLayout.stur",
        "-o",
        "test_outputs/MTTKRP_IJ_w_body_DataLayout_test.cpp",
        "--init-tensors"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/MTTKRP_IJ_w_body_DataLayout.cpp"
      )
    val file2 =
      scala.io.Source.fromFile(
        "test_outputs/MTTKRP_IJ_w_body_DataLayout_test.cpp"
      )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for LRC without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "LRC.stur",
        "-o",
        "test_outputs/LRC_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/LRC_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/LRC_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for LRC with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "LRC.stur",
        "-o",
        "test_outputs/LRC_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/LRC_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/LRC_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for LRA without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "LRA.stur",
        "-o",
        "test_outputs/LRA_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/LRA_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/LRA_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for LRA with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "LRA.stur",
        "-o",
        "test_outputs/LRA_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/LRA_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/LRA_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR2C without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR2C.stur",
        "-o",
        "test_outputs/PR2C_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PR2C_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR2C_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR2C with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR2C.stur",
        "-o",
        "test_outputs/PR2C_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/PR2C_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR2C_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR2A without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR2A.stur",
        "-o",
        "test_outputs/PR2A_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PR2A_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR2A_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR2A with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR2A.stur",
        "-o",
        "test_outputs/PR2A_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/PR2A_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR2A_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR3C without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR3C.stur",
        "-o",
        "test_outputs/PR3C_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PR3C_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR3C_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR3C with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR3C.stur",
        "-o",
        "test_outputs/PR3C_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/PR3C_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR3C_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR3A without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR3A.stur",
        "-o",
        "test_outputs/PR3A_wo_body_test.cpp"
      )
    )

    val file1 =
      scala.io.Source.fromResource(
        "correct_test_outputs/PR3A_wo_body.cpp"
      )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR3A_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for PR3A with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "PR3A.stur",
        "-o",
        "test_outputs/PR3A_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/PR3A_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/PR3A_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when the structure information is provided independent of the iterators with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "independent-iterator.stur",
        "-o",
        "test_outputs/independent-iterator_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/independent-iterator_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/independent-iterator_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is a scalar tensor in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "scalar-tensor-op.stur",
        "-o",
        "test_outputs/scalar-tensor-op_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/scalar-tensor-op_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/scalar-tensor-op_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is a scalar tensor in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "scalar-tensor-op.stur",
        "-o",
        "test_outputs/scalar-tensor-op_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/scalar-tensor-op_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/scalar-tensor-op_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is subtraction in form of multiplication in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "subtraction.stur",
        "-o",
        "test_outputs/subtraction_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/subtraction_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/subtraction_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is subtraction in form of multiplication in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "subtraction.stur",
        "-o",
        "test_outputs/subtraction_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/subtraction_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/subtraction_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is mixture of scalar tensors and constants in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "scalar-constant-mixture.stur",
        "-o",
        "test_outputs/scalar-constant-mixture_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/scalar-constant-mixture_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/scalar-constant-mixture_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is mixture of scalar tensors and constants in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "scalar-constant-mixture.stur",
        "-o",
        "test_outputs/scalar-constant-mixture_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/scalar-constant-mixture_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/scalar-constant-mixture_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when the iterators are renamed later on in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "independent-iterator-renaming.stur",
        "-o",
        "test_outputs/independent-iterator-renaming_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/independent-iterator-renaming_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/independent-iterator-renaming_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when the iterators are renamed later on in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "independent-iterator-renaming.stur",
        "-o",
        "test_outputs/independent-iterator-renaming_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/independent-iterator-renaming_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/independent-iterator-renaming_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when the same scalar appears in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "same-scalar.stur",
        "-o",
        "test_outputs/same-scalar_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/same-scalar_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/same-scalar_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }
  it should "generate code for when there is two group of tensors with same name in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "two-group-of-same-name.stur",
        "-o",
        "test_outputs/two-group-of-same-name_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/two-group-of-same-name_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/two-group-of-same-name_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is one group of tensors with same name but one has repetetive variable in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "one-group-same-name-one-tensor-same-variable.stur",
        "-o",
        "test_outputs/one-group-same-name-one-tensor-same-variable_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/one-group-same-name-one-tensor-same-variable_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/one-group-same-name-one-tensor-same-variable_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is inverse tensor in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-tensor.stur",
        "-o",
        "test_outputs/inverse-tensor_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-tensor_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-tensor_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is inverse tensor in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-tensor.stur",
        "-o",
        "test_outputs/inverse-tensor_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-tensor_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-tensor_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is inverse tensor with structure in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-with-structure.stur",
        "-o",
        "test_outputs/inverse-with-structure_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-with-structure_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-with-structure_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is inverse tensor with structure in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-with-structure.stur",
        "-o",
        "test_outputs/inverse-with-structure_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-with-structure_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-with-structure_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is mixture of tensor scalar and constant inverse in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-tensor-scalar.stur",
        "-o",
        "test_outputs/inverse-tensor-scalar_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-tensor-scalar_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-tensor-scalar_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is mixture of tensor scalar and constant inverse in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-tensor-scalar.stur",
        "-o",
        "test_outputs/inverse-tensor-scalar_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-tensor-scalar_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-tensor-scalar_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is complex computation over inverse tensors in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-complex.stur",
        "-o",
        "test_outputs/inverse-complex_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-complex_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-complex_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is complex computation over inverse tensors in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inverse-complex.stur",
        "-o",
        "test_outputs/inverse-complex_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inverse-complex_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inverse-complex_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is a symbol used in the computation without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "symbol-computation.stur",
        "-o",
        "test_outputs/symbol-computation_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/symbol-computation_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/symbol-computation_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is a symbol used in the computation with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "symbol-computation.stur",
        "-o",
        "test_outputs/symbol-computation_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/symbol-computation_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/symbol-computation_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is a need for renaming variables while projecting without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "independent-iterator-renaming-complex.stur",
        "-o",
        "test_outputs/independent-iterator-renaming-complex_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/independent-iterator-renaming-complex_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/independent-iterator-renaming-complex_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when inlining gets complex without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "inlining.stur",
        "-o",
        "test_outputs/inlining_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/inlining_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/inlining_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when outputs are defined and use selective inlining rather than aggressive inlining without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "selective-inlining.stur",
        "-o",
        "test_outputs/selective-inlining_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/selective-inlining_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/selective-inlining_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate correct body without bringing unused tensors to the function or deleting them when outputs are mentioned without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "bodygen-output.stur",
        "-o",
        "test_outputs/bodygen-output_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/bodygen-output_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/bodygen-output_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate correct body without bringing unused tensors to the function or deleting them when outputs are mentioned with the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "bodygen-output.stur",
        "-o",
        "test_outputs/bodygen-output_w_body_test.cpp",
        "--init-tensors"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/bodygen-output_w_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/bodygen-output_w_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for skew-symmetry structure without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "skew-symmetry.stur",
        "-o",
        "test_outputs/skew-symmetry_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/skew-symmetry_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/skew-symmetry_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for fancy-symmetry structure without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "fancy-symmetry.stur",
        "-o",
        "test_outputs/fancy-symmetry_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/fancy-symmetry_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/fancy-symmetry_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is an optimal projection, like matrix covariance matrix without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "covar.stur",
        "-o",
        "test_outputs/covar_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/covar_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/covar_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code for when there is an optimal projection, like pseudo lr training without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "pseudo-lr-training.stur",
        "-o",
        "test_outputs/pseudo-lr-training_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/pseudo-lr-training_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/pseudo-lr-training_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate code with variable ordering without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "variable-ordering.stur",
        "-o",
        "test_outputs/variable-ordering_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/variable-ordering_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/variable-ordering_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate correct code with partial variable ordering without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "partial-variable-ordering.stur",
        "-o",
        "test_outputs/partial-variable-ordering_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/partial-variable-ordering_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/partial-variable-ordering_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate correct code while including a non-bound extra variable while mentioning it in the variable ordering without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "including-extra-variable.stur",
        "-o",
        "test_outputs/including-extra-variable_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/including-extra-variable_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/including-extra-variable_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }

  it should "generate correct code for self inner product without the body" taggedAs(CodegenTestTag) in {
    Utils.cnt = 0
    Main.main(
      Array(
        "-i",
        "self-inner-product.stur",
        "-o",
        "test_outputs/self-inner-product_wo_body_test.cpp"
      )
    )

    val file1 = scala.io.Source.fromResource(
      "correct_test_outputs/self-inner-product_wo_body.cpp"
    )
    val file2 = scala.io.Source.fromFile(
      "test_outputs/self-inner-product_wo_body_test.cpp"
    )
    val lines1 = file1.getLines().toList
    val lines2 = file2.getLines().toList
    lines2 should be(lines1)
  }
}
