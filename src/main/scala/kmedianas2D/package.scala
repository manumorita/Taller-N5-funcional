package object kmedianas2D {
  import scala.annotation.tailrec
  import scala.util.Random
  import common._

  // ===== Modelo =====
  final case class Punto(x: Double, y: Double) {
    private def sq(v: Double) = v * v
    def distanciaAlCuadrado(that: Punto): Double = sq(that.x - x) + sq(that.y - y)
  }

  def umbral(n: Int): Int = {
    val p = math.max(1, Runtime.getRuntime.availableProcessors())
    math.max(4096, n / (p * 4))
  }

  private def chunk[A](xs: Seq[A], parts: Int): Seq[Seq[A]] = {
    val n = math.max(1, parts)
    val size = (xs.length.toDouble / n).ceil.toInt max 1
    xs.grouped(size).toVector
  }

  // ===== Datos de prueba =====
  def generarPuntos(k: Int, num: Int): Seq[Punto] = {
    val rx = new Random(3); val ry = new Random(11)
    (0 until num).iterator.map { i =>
      val x = ((i + 1) % k).toDouble / k + rx.nextDouble() * 0.5
      val y = ((i + 5) % k).toDouble / k + ry.nextDouble() * 0.5
      Punto(x, y)
    }.toVector
  }

  def inicializarMedianas(k: Int, puntos: Seq[Punto]): Seq[Punto] =
    new Random(7).shuffle(puntos).take(k).toVector

  // ===== Clasificación =====
  def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]): Punto = {
    require(medianas.nonEmpty, "No hay medianas.")
    medianas.minBy(m => p.distanciaAlCuadrado(m))
  }

  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    val m = puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
    val vacios = medianas.iterator.filterNot(m.contains).map(med => med -> Seq.empty[Punto])
    m ++ vacios
  }

  def clasificarPar(umbral: Int)(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    if (puntos.lengthCompare(umbral) <= 0) clasificarSeq(puntos, medianas)
    else {
      val pieces = chunk(puntos, Runtime.getRuntime.availableProcessors())
      val tasks = pieces.map { part =>
        task {
          part.groupBy(p => hallarPuntoMasCercano(p, medianas)).view.mapValues(_.toVector).toMap
        }
      }
      val merged = tasks.foldLeft(Map.empty[Punto, Seq[Punto]]) { (acc, t) =>
        val mm = t.join()
        mm.foldLeft(acc) { case (a, (k, vs)) => a.updated(k, a.getOrElse(k, Seq.empty) ++ vs) }
      }
      val vacios = medianas.iterator.filterNot(merged.contains).map(med => med -> Seq.empty[Punto])
      merged ++ vacios
    }
  }

  // ===== Promedios  =====

  private def promedio(pts: Seq[Punto]): Option[Punto] =
    if (pts.isEmpty) None
    else {
      val (sx, sy) = pts.foldLeft(0.0 -> 0.0) { case ((ax, ay), p) => (ax + p.x, ay + p.y) }
      Some(Punto(sx / pts.length, sy / pts.length))
    }

  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto =
    promedio(puntos).getOrElse(medianaVieja)

  def calculePromedioPar(medianaVieja: Punto, puntos: Seq[Punto]): Punto =
    promedio(puntos).getOrElse(medianaVieja)

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] =
    medianasViejas.map(m => calculePromedioSeq(m, clasif.getOrElse(m, Seq.empty)))

  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    val pieces = chunk(medianasViejas.zipWithIndex, Runtime.getRuntime.availableProcessors())
    val tasks = pieces.map { part =>
      task { part.map { case (m, i) => i -> calculePromedioPar(m, clasif.getOrElse(m, Seq.empty)) } }
    }
    tasks.flatMap(_.join()).sortBy(_._1).map(_._2)
  }

  // ===== Convergencia =====
  def hayConvergenciaSeq(eta: Double, viejas: Seq[Punto], nuevas: Seq[Punto]): Boolean = {
    val eta2 = eta * eta
    viejas.iterator.zip(nuevas.iterator).forall { case (a, b) => a.distanciaAlCuadrado(b) <= eta2 }
  }

  def hayConvergenciaPar(eta: Double, viejas: Seq[Punto], nuevas: Seq[Punto]): Boolean = {
    val eta2 = eta * eta
    val idx = viejas.indices.toVector
    val pieces = chunk(idx, Runtime.getRuntime.availableProcessors())
    val tasks = pieces.map { part =>
      task { part.forall(i => viejas(i).distanciaAlCuadrado(nuevas(i)) <= eta2) }
    }
    tasks.forall(_.join())
  }

  // ===== Algoritmo =====
  @tailrec
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasif = clasificarSeq(puntos, medianas)
    val nuevas  = actualizarSeq(clasif, medianas)
    if (hayConvergenciaSeq(eta, medianas, nuevas)) nuevas
    else kMedianasSeq(puntos, nuevas, eta)
  }

  @tailrec
  final def kMedianasPar(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val u = umbral(puntos.length)
    val clasif = clasificarPar(u)(puntos, medianas)
    val nuevas  = actualizarPar(clasif, medianas)
    if (hayConvergenciaPar(eta, medianas, nuevas)) nuevas
    else kMedianasPar(puntos, nuevas, eta)
  }
}
