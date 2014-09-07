package sbtparboiled2boost

import org.apache.commons.io.FilenameUtils
import sbt._

object Plugin extends AutoPlugin {

  import sbt.Keys._

  object autoImport {
    lazy val parboiledCompile = TaskKey[Seq[File]]("parboiled-compile", "Generates parboiled2 files from files")
  }

  import autoImport._

  override def trigger = allRequirements

  override val projectSettings: Seq[Setting[_]] = Seq(
    sourceDirectory in parboiledCompile <<= (sourceDirectory in Compile) / "parboiled",

    target in parboiledCompile <<= (sourceDirectory in Compile) / "scala",

    parboiledCompile <<= (streams, sourceDirectory in parboiledCompile, target in parboiledCompile) map generateFromParboiledFiles,

    (sourceGenerators in Compile) <+= parboiledCompile,
    (managedSourceDirectories in Compile) <+= target in parboiledCompile,

    // watch sources support
    includeFilter in parboiledCompile := "*.pb2",
    excludeFilter in parboiledCompile <<= excludeFilter in Global,
    watch(sourceDirectory in parboiledCompile, includeFilter in parboiledCompile, excludeFilter in parboiledCompile),

    // add managed sources to the packaged sources
    mappings in(Compile, packageSrc) <++=
      (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
        (srcs x (Path.relativeTo(base) | Path.flat))
      }
  )

  def watch(sourceDirKey: SettingKey[File], filterKey: SettingKey[FileFilter], excludeKey: SettingKey[FileFilter]) =
    watchSources <++= (sourceDirKey, filterKey, excludeKey) map descendents

  def descendents(sourceDir: File, filt: FileFilter, excl: FileFilter) =
    descendantsExcept(sourceDir, filt, excl).get

  def generateFromParboiledFiles(streams: TaskStreams, sourceDir: File, targetDir: File): Seq[File] = {
    val files = sourceDir ** "*.pb2"

    def changeExtension(f: File): File = {
      val (ext, name) = f.getName.reverse.span(_ != '.')
      new File(f.getParent, name.drop(1).reverse.toString)
    }

    val mapping = (files x rebase(sourceDir, targetDir)).map {
      case (orig, target) => (orig, changeExtension(target))
    }

    mapping foreach {
      case (parboiledFile, target) =>
        if (parboiledFile.lastModified > target.lastModified) {
          streams.log.info("Generating '%s'" format target.getName)
          val pbFile = IO.read(parboiledFile)
          val name = FilenameUtils.getBaseName(FilenameUtils.getBaseName(parboiledFile.getName))
          IO.write(target, Generator.generateFromParboiledGrammar(name, pbFile))
        } else {
          streams.log.debug("parboiled grammar '%s' older than target. Ignoring." format parboiledFile.getName)
        }
    }

    mapping.map(_._2)
  }

  def descendantsExcept(path: PathFinder, include: FileFilter, intermediateExclude: FileFilter): PathFinder =
    (path ** include) --- (path ** intermediateExclude ** include)
}
