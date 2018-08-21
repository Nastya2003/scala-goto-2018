import CustomRegion1.{CustomRegion, Region}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.AllOf
import eu.timepit.refined.collection._
import eu.timepit.refined.string.MatchesRegex
import shapeless.{::, HNil}
import eu.timepit.refined._
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.api.{Refined, Validate}
import scala.util.{Failure, Success, Try}

case class TypeValidationException(msg: String) extends Exception(msg)

object CustomRegion1 {

  case class RegionPredicate()

  implicit def either2Try[T](income: Either[String, T]): Try[T] = {
    income match {
      case Right(message) => Success(message)
      case Left(ex) => Failure(TypeValidationException(ex))
    }
  }

  type Region = Refined[String, RegionPredicate]

  object Region {
    def unapply(arg: Region): Option[String] = Some(arg.value)
    def apply(arg: String): Try[Region] = refineV[RegionPredicate](arg)
  }


  implicit val validateRegion: Plain[String, RegionPredicate] =
    Validate.fromPredicate(
      (region: String) => ListOfRegions.contains(region),
      reg => s"Region $reg does not exist",
      RegionPredicate()
    )

  final private val ListOfRegions = Set(
    "Russia-1",
    "Russia-2",
    "Russia-3",
    "Europe-1",
    "Europe-2",
    "Europe-3",
  )


  type CustomRegion = Refined[String, CustomRegionV]

  object CustomRegion {
    def unapply(arg: CustomRegion): Option[String] = Some(arg.value)

    def apply(arg: String): Try[CustomRegion] = refineV[CustomRegionV](arg)
  }

  private type CustomRegionV = AllOf[minCustomRegionSize :: maxCustomRegionSize :: CustomRegionRegEx :: HNil]

  private type maxCustomRegionSize = MaxSize[W.`255`.T]
  private type minCustomRegionSize = MinSize[W.`1`.T]
  private type CustomRegionRegEx = MatchesRegex[W.`"[a-zA-Z]*"`.T]

}


object Main extends App {
  val a: CustomRegion = CustomRegion("Russia").get
  println(a)

  val c = Region("Europe-3").get
  println(c)

  val b = Region("фыв").get
  println(b)

}
