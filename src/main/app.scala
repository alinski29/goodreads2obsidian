import caseapp.*
import com.github.tototoshi.csv.*

import java.io.{File, PrintWriter}
import java.net.{URI, URLEncoder}
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.{BasicFileAttributes, FileAttribute, FileTime}
import java.time.{Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

case class Options(
    inputPath: String,
    obisidianVaultPath: String,
    booksFolder: String = "Books",
    resourcesFolder: String = "resources",
    inputDateFormat: String = "yyyy/MM/dd",
    ratingAsSymbols: Boolean = true
)

@main def run(args: String*): Unit =
  val (opts, _) = CaseApp.parse[Options](args.toList).right.get

  val booksDir     = java.io.File(s"${opts.obisidianVaultPath}/${opts.booksFolder}")
  val resourcesDir = java.io.File(s"${opts.obisidianVaultPath}/${opts.resourcesFolder}")
  if !booksDir.exists() then booksDir.mkdirs()
  if !resourcesDir.exists() then resourcesDir.mkdirs()

  CSVReader.open(new File(opts.inputPath)).iteratorWithHeaders.foreach: row =>
    val tryProcessBook = for
      book <- Book.fromAttributes(row)
      _    <- Try(book.writeMarkdownFile(booksDir.toURI, resourcesDir.toURI))
    yield book

    tryProcessBook match
      case Success(book) => println(s"Sucesfully processed '${book.fileName}'")
      case Failure(ex)   => throw ex

enum ReadState:
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
    ReadStatus: ReadState = ReadState.ToRead
):

  def generateMarkdownContent(coverPath: Option[URI] = None): String =
    val meta        = this.toYamlMetadata()
    val description = """### Description"""
    val highlights  = """### Highlights"""
    val cover       = coverPath.map(path => s"cover_link: ${path.getPath.split("/").last}").getOrElse("")
    val renderCover =
      if cover.nonEmpty then
        val path = cover.split(":").tail.filterNot(_.isBlank).mkString("")
        s"![[$path|250]]"
      else ""

    "---\n" + meta + "\n" + cover + "\n" + "---\n\n" + renderCover + "\n\n" + description + "\n\n" + highlights + "\n"

  def fileName: String =
    s"$Title - $Author.md".replaceAll("/", "%")

  def writeMarkdownFile(booksDir: URI, resourcesDir: URI): Unit =
    val file = new java.io.File(s"${booksDir.getPath}/${this.fileName}")
    val pw   = new java.io.PrintWriter(file)
    val modifiedTime = this.DateAdded
      .orElse(this.DateRead)
      .map(dt => dt.atStartOfDay(ZoneId.systemDefault()).toInstant)
      .getOrElse(Instant.now())
    try
      val maybeCoverPath =
        for
          bytes     <- this.fetchCover
          coverFile <- this.writeCover(bytes, resourcesDir)
        yield coverFile
      pw.write(this.generateMarkdownContent(maybeCoverPath))
      Files.setLastModifiedTime(Paths.get(file.getPath), FileTime.from(modifiedTime))
    catch
      case e: Exception =>
        println(s"Failed to write book for ${this.fileName}")
        ()
    finally pw.close()

  def toYamlMetadata(): String =
    ListMap(
      "title"            -> s""""${Title}"""",
      "subtitle"         -> Subtitle.map(x => s""""${x}""""),
      "author"           -> s""""${Author}"""",
      "authors"          -> AdditionalAuthors,
      "total_page"       -> Pages,
      "date_read"        -> DateRead,
      "date_added"       -> DateAdded,
      "read_status"      -> Book.camelToDash(ReadStatus.toString),
      "rating"           -> PersonalRating.map(x => Book.ratingToSymbol(x, 5)),
      "isbn10"           -> ISBN,
      "isbn13"           -> ISBN13,
      "goodreads_rating" -> GoodreadsRating,
      "goodreads_id"     -> GoodreadsId,
      "cover_url"        -> CoverLink
    )
      .map { case (k, v) => toYaml(k, v.asInstanceOf[AnyVal]) }
      .filterNot(x => x.isBlank || x.isEmpty)
      .filterNot(x => x.split(":").tail.headOption.map(_.trim.replaceAll(""""""", "")).getOrElse("").isBlank)
      .mkString("\n")

  private def toYaml(key: String, value: AnyVal): String =
    value match
      case vs if vs.isInstanceOf[List[Any]] =>
        vs.asInstanceOf[List[Any]] match
          case xs if xs.nonEmpty => s"$key: " + s""""${xs.map(_.toString).mkString(", ")}""""
          case _                 => ""
      case v if v.isInstanceOf[Option[Any]] =>
        v.asInstanceOf[Option[Any]] match
          case Some(v) => toYaml(key, v.asInstanceOf[AnyVal])
          case None    => ""
      case v: LocalDate =>
        val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        s"$key: ${v.format(fmt)}"
      case v => s"$key: ${v.toString()}"

  private def fetchCover: Option[Array[Byte]] =
    def getCover(link: String): Option[Array[Byte]] =
      Try(requests.get(link)) match
        case Success(resp) if resp.statusCode == 200 =>
          if resp.bytes.size > 1024 then Some(resp.bytes)
          else None
        case _ => None

    lazy val isbn13Cover = this.ISBN13
      .flatMap(isbn => getCover(s"https://covers.openlibrary.org/b/isbn/$isbn-L.jpg"))
    lazy val isbnCover = this.ISBN
      .flatMap(isbn => getCover(s"https://covers.openlibrary.org/b/isbn/$isbn-L.jpg"))

    isbn13Cover.orElse(isbnCover)

  private def writeCover(bytes: Array[Byte], resourceDir: URI): Option[URI] =
    val fileName = this.ISBN
      .orElse(this.ISBN13)
      .map(isbn => s"book%$isbn.jpg")
      .getOrElse(URLEncoder.encode(this.fileName, "UTF-8") + ".png")
    val file = new java.io.File(s"${resourceDir.getPath}/${fileName}")

    if file.exists() then
      println(s"Image '${file.getName}' for book '${this.fileName}' already exists")
      return Some(file.toURI)

    val fos = new java.io.FileOutputStream(file)
    try
      fos.write(bytes)
      Some(file.toURI)
    catch
      case e: Exception =>
        println(s"Failed to write image for ${this.fileName}")
        None
    finally fos.close()

  private def toYaml[T](key: String, value: Option[T]): String =
    value match {
      case None    => ""
      case Some(v) => toYaml(key, v.asInstanceOf[AnyVal])
    }

end Book

object Book:

  def fromAttributes(record: Map[String, String]): Try[Book] =
    Try {
      val book = Book(
        BookId = record.getOrElse("Book Id", ""),
        Title = record("Title"),
        Author = record("Author"),
        AdditionalAuthors = record.getOrElse("Additional Authors", "").split(",").map(_.trim).toList,
        ISBN = record.get("ISBN").map(_.map(c => if c.isDigit then c else "").mkString("")).flatMap { x =>
          if x.isBlank then None else Some(x)
        },
        ISBN13 = record
          .get("ISBN13")
          .map(_.map(c => if c.isDigit then c else "").mkString(""))
          .flatMap(x => if x.isBlank then None else Some(x)),
        PersonalRating = record.get("My Rating").flatMap(x => if x == "0" then None else Try(x.toInt).toOption),
        GoodreadsRating = record.get("Average Rating").flatMap(x => Try(x.toDouble).toOption),
        GoodreadsId = record.get("Book Id"),
        Pages = record.get("Number of Pages").flatMap(x => Try(x.toInt).toOption),
        YearPublished = record.get("Year Published"),
        DateRead = record.get("Date Read").flatMap(x => tryParseDate(x)),
        DateAdded = record.get("Date Added").flatMap(x => tryParseDate(x)),
        ReadStatus = record.getOrElse("Exclusive Shelf", "to-read") match {
          case "currenlty-reading" => ReadState.CurrentlyReading
          case "read"              => ReadState.Read
          case _                   => ReadState.ToRead
        }
      )
      book.copy(CoverLink =
        book.ISBN
          .orElse(book.ISBN13)
          .map(isbn => s"https://covers.openlibrary.org/b/isbn/$isbn-M.jpg")
      )
    }

  def ratingToSymbol(rating: Int, max: Int = 5): String =
    val (empty, filled) = ("☆", "★")
    if rating > max then filled.repeat(max)
    else filled.repeat(rating) + empty.repeat(max - rating)

  def camelToDash(text: String) =
    text.drop(1).foldLeft(text.headOption.map(_.toLower + "").getOrElse("")) {
      case (acc, c) if c.isUpper => acc + "-" + c.toLower
      case (acc, c)              => acc + c
    }

  private def tryParseDate(
      raw: String,
      fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")
  ): Option[LocalDate] =
    Try(LocalDate.parse(raw, fmt)).toOption

end Book
