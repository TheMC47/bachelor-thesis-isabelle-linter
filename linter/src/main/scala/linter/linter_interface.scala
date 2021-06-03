package linter

import isabelle._

class Linter_Interface {

  var lint_cache: Map[Document.Node.Name, (Document.Version, Linter.Lint_Report)] = Map.empty

  private def update_cache(snapshot: Document.Snapshot): Unit = {
    lazy val new_cache =
      lint_cache + (snapshot.node_name -> (snapshot.version, Linter.lint(
        snapshot,
        Linter.all_lints
      )))
    lint_cache get snapshot.node_name match {
      case None               => lint_cache = new_cache
      case Some((version, _)) => if (snapshot.version.id < version.id) lint_cache = new_cache
    }
  }

  def lint_results(snapshot: Document.Snapshot): List[Linter.Lint_Result] = {
    update_cache(snapshot)
    lint_cache(snapshot.node_name)._2.results
  }

  def lint_ranges(
      snapshot: Document.Snapshot,
      line_range: Text.Range = Text.Range.full
  ): List[Text.Range] =
    lint_results(snapshot)
      .map(_.range)
      .filter(lint_range => !line_range.apart(lint_range))
      .map(_.restrict(line_range))

}

class Linter_Variable {
  private val no_linter: Option[Linter_Interface] = None
  private var current_linter: Option[Linter_Interface] = no_linter

  def get: Option[Linter_Interface] = synchronized { current_linter }

  def update(options: Options): Unit = synchronized {
    if (options.bool("linter")) {
      if (current_linter.isEmpty) {
        current_linter = Some(new Linter_Interface)
      }
    } else current_linter = no_linter
  }
}
