package object Benchmark {
  import scala.collection.Seq
  import kmedianas2D._
  import plotly._, element._, layout._
  import java.nio.file.{Files, Paths}


  // ---- Medición liviana: System.nanoTime (sin Scalameter) ----
  private def medirMs[T](body: => T): (Double, T) = {
    val t0 = System.nanoTime()
    val r  = body
    val t1 = System.nanoTime()
    ((t1 - t0) / 1e6, r) // ms
  }

  /** Devuelve (tSeqMs, tParMs, speedup) - no grafica */
  def tiemposKmedianas(puntos: Seq[Punto], k: Int, eta: Double): (Double, Double, Double) = {
    val medianas = inicializarMedianas(k, puntos)
    val (tSeqMs, _) = medirMs { kMedianasSeq(puntos, medianas, eta) }
    val (tParMs, _) = medirMs { kMedianasPar(puntos, medianas, eta) }
    (tSeqMs, tParMs, tSeqMs / tParMs)
  }

  /** AHORA: probarKmedianas es RÁPIDO y NO GRAFICA (para usar en worksheets) */
  def probarKmedianas(puntos: Seq[Punto], k: Int, eta: Double): (Double, Double, Double) = {
    val medianasIni = inicializarMedianas(k, puntos)
    val (tSeqMs, _) = medirMs { kMedianasSeq(puntos, medianasIni, eta) }
    val (tParMs, _) = medirMs { kMedianasPar(puntos, medianasIni, eta) }
    (tSeqMs, tParMs, tSeqMs / tParMs)
  }

  // --------- Función OPCIONAL para graficar cuando tú quieras ----------
  private def guardarPlot(nombre: String, data: Seq[Trace], layout: Layout): Unit = {
    val outDir = Paths.get("target", "plots")
    if (!Files.exists(outDir)) Files.createDirectories(outDir)
    Plotly.plot(outDir.resolve(nombre).toString, data.toList, layout) // <-- aquí el cambio
  }

  /** Genera kmedianasSeq.html y kmedianasPar.html (sin medir tiempos) */
  def graficarKmedianas(puntos: Seq[Punto], k: Int, eta: Double): Unit = {
    // Secuencial
    val medianasSeq    = inicializarMedianas(k, puntos)
    val medianasSeqfin = kMedianasSeq(puntos, medianasSeq, eta)
    val clasifFinalSeq = clasificarSeq(puntos, medianasSeqfin)

    val trazosSeq = for {
      (m, ps) <- clasifFinalSeq.toSeq
      xs = ps.map(_.x); ys = ps.map(_.y)
    } yield Scatter(xs, ys).withMode(ScatterMode(ScatterMode.Markers)).withName(s"Puntos ${m.x},${m.y}")

    val dataSeq =
      Scatter(medianasSeq.map(_.x),    medianasSeq.map(_.y)).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas") +:
        (Scatter(medianasSeqfin.map(_.x), medianasSeqfin.map(_.y)).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas Finales") +:
          trazosSeq.toSeq)

    guardarPlot("kmedianasSeq.html", dataSeq, Layout().withTitle("Plotting de puntos (Secuencial)"))

    // Paralelo
    val medianasPar    = medianasSeq
    val medianasParfin = kMedianasPar(puntos, medianasPar, eta)
    val clasifFinalPar = clasificarPar(umbral(puntos.length))(puntos, medianasParfin)

    val trazosPar = for {
      (m, ps) <- clasifFinalPar.toSeq
      xs = ps.map(_.x); ys = ps.map(_.y)
    } yield Scatter(xs, ys).withMode(ScatterMode(ScatterMode.Markers)).withName(s"Puntos ${m.x},${m.y}")

    val dataPar =
      Scatter(medianasPar.map(_.x),    medianasPar.map(_.y)).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas") +:
        (Scatter(medianasParfin.map(_.x), medianasParfin.map(_.y)).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas Finales") +:
          trazosPar.toSeq)

    guardarPlot("kmedianasPar.html", dataPar, Layout().withTitle("Plotting de puntos (Paralela)"))
  }
}
