/*
 * This Scala Testsuite was auto generated by running 'gradle init --type scala-library'
 * by 'ashokk' at '6/3/16 6:46 PM' with Gradle 2.7
 *
 * @author ashokk, @date 6/3/16 6:46 PM
 */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LibrarySuite extends FunSuite {
  test("someLibraryMethod is always true") {
    def library = new Library()
    assert(library.someLibraryMethod)
  }
}
