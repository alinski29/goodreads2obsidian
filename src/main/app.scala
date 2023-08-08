import caseapp.*
import com.github.tototoshi.csv.*

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.{BasicFileAttributes, FileAttribute, FileTime}
import java.time.{Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

case class Options(
    inputPath: String,
    obisidianVaultPath: String,
    obisidianFolder: String = "Books",
    inputDateFormat: String = "yyyy/MM/dd"
)

enum ReadStatus:
  case Read, ToRead, CurrentlyReading

case class Book(
    BookId: String,
    Title: String,
    Author: String,
    AdditionalAuthors: List[String] = List.empty,
    Subtitle: Option[String] = None,
    ISBN: Option[String] = None,
    ISBN13: Option[String] = None,
    CoverLink: Option[String] = None,
    PersonalRating: Option[Int] = None,
    GoodreadsRating: Option[Double] = None,
    GoodreadsId: Option[String] = None,
    Pages: Option[Int] = None,
    YearPublished: Option[String] = None,
    DateRead: Option[LocalDate] = None,
    DateAdded: Option[LocalDate] = None,
    ReadStatus: ReadStatus = ReadStatus.ToRead
) {

  def getContent(): String =
    val meta = this.toYamlMetadata()
    "---\n" + meta + "\n" + "---\n" + "\n"

  def fileName: String =
    s"$Title - $Author.md"

  def toYamlMetadata(): String =
    ListMap(
      "title"            -> Title,
      "subtitle"         -> Subtitle,
      "author"           -> Author,
      "authors"          -> AdditionalAuthors,
      "total_page"       -> Pages,
      "date_read"        -> DateRead,
      "date_added"       -> DateAdded,
      "read_status"      -> camelToUnderscore(ReadStatus.toString),
      "rating"           -> PersonalRating,
      "isbn_10"          -> ISBN,
      "isbn_13"          -> ISBN13,
      "goodreads_rating" -> GoodreadsRating,
      "goodreads_id"     -> GoodreadsId,
      "cover_url"        -> CoverLink
    )
      .map { case (k, v) => toYaml(k, v.asInstanceOf[AnyVal]) }
      .filterNot(x => x.isBlank || x.isEmpty)
      .mkString("\n")

  private def toYaml(key: String, value: AnyVal): String =
    value match
      case vs if vs.isInstanceOf[List[Any]] =>
        vs.asInstanceOf[List[Any]] match
          case xs if xs.nonEmpty => s"$key: " + xs.map(_.toString).mkString(", ")
          case _                 => ""
      case v if v.isInstanceOf[Option[Any]] =>
        v.asInstanceOf[Option[Any]] match
          case Some(v) => toYaml(key, v.asInstanceOf[AnyVal])
          case None    => ""
      case v: LocalDate =>
        val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        s"$key: ${v.format(fmt)}"
      case v => s"$key: ${v.toString()}"

  private def toYaml[T](key: String, value: Option[T]): String =
    value match {
      case None    => ""
      case Some(v) => toYaml(key, v.asInstanceOf[AnyVal])
    }
}

def camelToUnderscore(text: String) =
  text.drop(1).foldLeft(text.headOption.map(_.toLower + "") getOrElse "") {
    case (acc, c) if c.isUpper => acc + "_" + c.toLower
    case (acc, c)              => acc + c
  }

def tryParseDate(raw: String, fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")) =
  Try(LocalDate.parse(raw, fmt)).toOption

def parseInput(record: Map[String, String]): Try[Book] =
  Try {
    val book = Book(
      BookId = record.getOrElse("Book Id", ""),
      Title = record("Title"),
      Author = record("Author"),
      AdditionalAuthors = record.getOrElse("Additional Authors", "").split(",").map(_.trim).toList,
      ISBN = record.get("ISBN").map(_.map(c => if c.isDigit then c else "").mkString("")),
      ISBN13 = record.get("ISBN13").map(_.map(c => if c.isDigit then c else "").mkString("")),
      PersonalRating = record.get("Rating").map(_.toInt),
      GoodreadsRating = record.get("Average Rating").flatMap(x => Try(x.toDouble).toOption),
      GoodreadsId = record.get("Book Id"),
      Pages = record.get("Number of Pages").map(_.toInt),
      YearPublished = record.get("Year Published"),
      DateRead = record.get("Date Read").flatMap(x => tryParseDate(x)),
      DateAdded = record.get("Date Added").flatMap(x => tryParseDate(x)),
      ReadStatus = record.getOrElse("Exclusive Shelf", "to-read") match {
        case "currenlty-reading" => ReadStatus.CurrentlyReading
        case "read"              => ReadStatus.Read
        case _                   => ReadStatus.ToRead
      }
    )
    book.copy(CoverLink =
      book.ISBN
        .orElse(book.ISBN13)
        .map(isbn => s"https://covers.openlibrary.org/b/isbn/$isbn-M.jpg")
    )
  }

// def printeAttributes(file: Path): Unit =
//   val attrs = Files.readAttributes(file, classOf[BasicFileAttributes])
//   println(
//     s"created: ${attrs.creationTime}, last modified: ${attrs.lastModifiedTime()}, last access: ${attrs.lastAccessTime()}"
//   )

def writeBook(book: Book, booksDir: URI): Unit =
  val file = new java.io.File(s"${booksDir.getPath}/${book.fileName}")
  val pw   = new java.io.PrintWriter(file)
  val modifiedTime = book.DateAdded
    .orElse(book.DateRead)
    .map(dt => dt.atStartOfDay(ZoneId.systemDefault()).toInstant)
    .getOrElse(Instant.now())
  try
    pw.write(book.getContent())
    Files.setLastModifiedTime(Paths.get(file.getPath), FileTime.from(modifiedTime))
  finally pw.close()

object Goodreads2Obidian {

  def main(args: Array[String]): Unit =
    val (opts, _) = CaseApp.parse[Options](args.toList).right.get

    val dest = java.io.File(s"${opts.obisidianVaultPath}/${opts.obisidianFolder}")
    if !dest.exists() then dest.mkdirs()

    val reader = CSVReader.open(new File(opts.inputPath))
    val res = reader.iteratorWithHeaders.foreach { row =>
      parseInput(row) match {
        case Success(book: Book) =>
          writeBook(book, dest.toURI)
        case Failure(ex) =>
          throw ex
      }

    }

}
