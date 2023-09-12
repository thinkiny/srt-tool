import caseapp.cats.IOCaseApp
import caseapp.core.RemainingArgs
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path

case class AppOptions(input: String, output: Option[String] = None)

object SrtTools extends IOCaseApp[AppOptions]:
  def makeDirectory(p: Path): IO[Unit] =
    Files[IO]
      .exists(p)
      .ifM(IO.unit, Files[IO].createDirectory(p))

  def run(options: AppOptions, arg: RemainingArgs): IO[ExitCode] =
    val outputPath = options.output
      .map(Path(_))
      .getOrElse(Path(options.input).resolve("new_srt"))

    makeDirectory(outputPath) >>
      Files[IO]
        .walk(Path(options.input), 1, false)
        .filter(_.toString.endsWith(".srt"))
        .flatMap(p =>
          Files[IO]
            .readUtf8Lines(p)
            .fold(new SrtBuilder)((b, l) => b.addLine(l))
            .flatMap(i => fs2.Stream.emits(i.mergeResult().zipWithIndex))
            .map((item, i) => item.toSrtString(i + 1))
            .through(
              Files[IO].writeUtf8(outputPath.resolve(p.fileName))
            )
        )
        .compile
        .drain
        .as(ExitCode.Success)
