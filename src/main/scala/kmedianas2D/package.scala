package object kmedianas2D {
  import scala.annotation.tailrec
  import scala.collection.{Map, Seq}
  import scala.collection.parallel.CollectionConverters._
  import scala.util.Random
  import common._

  class Punto(val x: Double, val y: Double) {
    private def cuadrado(v: Double): Double = v * v

    def distanciaAlCuadrado(that: Punto): Double =
      cuadrado(that.x - x) + cuadrado(that.y - y)

    private def round(v: Double): Double = (v * 100).toInt / 100.0

    override def toString = s"(${round(x)},${round(y)})"
  }

  def generarPuntos(k: Int, num: Int): Seq[Punto] = {
    val randx = new Random
    val randy = new Random
    (0 until num)
      .map({ i =>
        val x = (((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5)
        val y = (((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5)
        new Punto(x, y)
      })
  }

  def inicializarMedianas(k: Int, puntos: Seq[Punto]): Seq[Punto] = {
    val rand = new Random
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length)))
  }

  // Clasificar puntos
  def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]): Punto = {
    assert(medianas.nonEmpty)
    medianas
      .map(pto => (pto, p.distanciaAlCuadrado(pto)))
      .sortWith((a, b) => (a._2 < b._2))
      .head._1
  }

  // Versiones secuenciales

  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      new Punto(
        puntos.map(p => p.x).sum / puntos.length,
        puntos.map(p => p.y).sum / puntos.length
      )
    }
  }

  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    // groupBy con la mediana más cercana
    puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
  }

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    medianasViejas.map(m => calculePromedioSeq(m, clasif.getOrElse(m, Seq.empty)))
  }

  @tailrec
  def hayConvergenciaSeq(
                          eta: Double,
                          medianasViejas: Seq[Punto],
                          medianasNuevas: Seq[Punto]
                        ): Boolean = {
    // Iterativo (tail-rec) sobre el índice
    @tailrec
    def loop(i: Int): Boolean =
      if (i >= medianasViejas.length) true
      else {
        val d2 = medianasViejas(i).distanciaAlCuadrado(medianasNuevas(i))
        if (d2 <= eta) loop(i + 1) else false
      }
    loop(0)
  }

  @tailrec
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasif  = clasificarSeq(puntos, medianas)
    val nuevas  = actualizarSeq(clasif, medianas)
    val listo   = hayConvergenciaSeq(eta, medianas, nuevas)
    if (listo) nuevas else kMedianasSeq(puntos, nuevas, eta)
  }

  // Versiones paralelas

  def calculePromedioPar(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      val puntosPar = puntos.par
      new Punto(
        puntosPar.map(p => p.x).sum / puntos.length,
        puntosPar.map(p => p.y).sum / puntos.length
      )
    }
  }

  /***   HELPERS EXTRA (dejo documentados)
   *
   *  1) umbral(n: Int): Int
   *     - Heurística simple para decidir cuándo conviene dividir/trabajar en paralelo.
   *     - Se usa sólo por `clasificarPar` y `kMedianasPar`, acorde a Benchmark.
   *
   *  2) mergeClasifs(a, b)
   *     - Fusión inmutable de dos Map[Punto, Seq[Punto]] concatenando las secuencias.
   *
   *  3) actualizarParRange / hayConvParRange
   *     - Divide & conquer con paralelismo de tareas (common.parallel),
   *       preservando el orden en la actualización y evaluando convergencia en paralelo.
   */

  private def umbral(n: Int): Int =
    math.max(1024, n / 8)   // <- heurística conservadora

  private def mergeClasifs(a: Map[Punto, Seq[Punto]], b: Map[Punto, Seq[Punto]]): Map[Punto, Seq[Punto]] = {
    // Unir claves y concatenar listas de puntos (sin mutabilidad)
    (a.keySet ++ b.keySet)
      .toSeq
      .map(k => k -> (a.getOrElse(k, Seq.empty) ++ b.getOrElse(k, Seq.empty)))
      .toMap
  }

  def clasificarPar(umb: Int)(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    if (puntos.length <= umb) clasificarSeq(puntos, medianas)
    else {
      val (izq, der) = puntos.splitAt(puntos.length / 2)
      val (m1, m2) = parallel(
        clasificarPar(umb)(izq, medianas),
        clasificarPar(umb)(der, medianas)
      )
      mergeClasifs(m1, m2)
    }
  }

  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {

    // divide & conquer para mantener orden con paralelismo de tareas
    def actualizarParRange(ms: Seq[Punto]): Seq[Punto] =
      if (ms.length <= 2) {
        // tramo pequeño: secuencial y ordenado
        ms.map(m => calculePromedioPar(m, clasif.getOrElse(m, Seq.empty)))
      } else {
        val (a, b) = ms.splitAt(ms.length / 2)
        val (ra, rb) = parallel(
          actualizarParRange(a),
          actualizarParRange(b)
        )
        ra ++ rb
      }

    actualizarParRange(medianasViejas)
  }

  def hayConvergenciaPar(
                          eta: Double,
                          medianasViejas: Seq[Punto],
                          medianasNuevas: Seq[Punto]
                        ): Boolean = {

    def hayConvParRange(vs: Seq[Punto], ns: Seq[Punto]): Boolean = {
      if (vs.length <= 512) {
        // tramo pequeño: check secuencial (inmutable)
        vs.lazyZip(ns).forall((v, n) => v.distanciaAlCuadrado(n) <= eta)
      } else {
        val mid = vs.length / 2
        val (vA, vB) = vs.splitAt(mid)
        val (nA, nB) = ns.splitAt(mid)
        val (okA, okB) = parallel(
          hayConvParRange(vA, nA),
          hayConvParRange(vB, nB)
        )
        okA && okB
      }
    }

    hayConvParRange(medianasViejas, medianasNuevas)
  }

  @tailrec
  final def kMedianasPar(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasif  = clasificarPar(umbral(puntos.length))(puntos, medianas)
    val nuevas  = actualizarPar(clasif, medianas)
    val listo   = hayConvergenciaPar(eta, medianas, nuevas)
    if (listo) nuevas else kMedianasPar(puntos, nuevas, eta)
  }
}
