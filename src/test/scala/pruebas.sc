import Benchmark._
import kmedianas2D._
import scala.util.Random

// ============================================================================
// INFORME DE DESEMPEÑO: kMedianasSeq vs kMedianasPar
// ============================================================================

println("=" * 80)
println("INFORME DE DESEMPEÑO: kMedianasSeq vs kMedianasPar")
println("=" * 80)

// ---------------------------------------------------------------------------
// Función auxiliar para generar puntos con semilla fija
// ---------------------------------------------------------------------------

def generarPuntosConSeed(k: Int, num: Int, seed: Long): Seq[Punto] = {
  val randx = new Random(seed)
  val randy = new Random(seed + 1000)
  (0 until num).map { i =>
    val x = (((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5)
    val y = (((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5)
    new Punto(x, y)
  }
}

// ---------------------------------------------------------------------------
// Función para medir desempeño
// ---------------------------------------------------------------------------

def pruebaDesempeno(numPuntos: Int, numClusters: Int, eta: Double, seed: Long): Unit = {
  println(s"\n--- Prueba: $numPuntos puntos, $numClusters clusters, eta=$eta (seed=$seed) ---")

  val puntos = generarPuntosConSeed(numClusters, numPuntos, seed)
  val medianas = inicializarMedianas(numClusters, puntos)

  val (tSeq, tPar, aceleracion) = tiemposKmedianas(puntos, numClusters, eta)

  println(f"Tiempo Secuencial: $tSeq%.4f")
  println(f"Tiempo Paralelo:   $tPar%.4f")
  println(f"Aceleración:       $aceleracion%.4f x")

  // Verificar convergencia
  val finalSeq = kMedianasSeq(puntos, medianas, eta)
  val finalPar = kMedianasPar(puntos, medianas, eta)

  val equivalentes = finalSeq.zip(finalPar).forall { case (s, p) =>
    s.distanciaAlCuadrado(p) < 1e-6
  }
  println(s"¿Resultados equivalentes? $equivalentes")
}

// ============================================================================
// PRUEBA 1: Escalamiento con número de puntos (k=8, eta=0.01)
// ============================================================================
println("\n" + "=" * 80)
println("PRUEBA 1: Escalamiento con número de puntos (k=8, eta=0.01)")
println("=" * 80)

val tamanos1 = Seq(16, 64, 256, 1024, 4096, 16384, 65536)
tamanos1.zipWithIndex.foreach { case (n, i) =>
  pruebaDesempeno(n, 8, 0.01, seed = 1000L + i)
}

// ============================================================================
// PRUEBA 2: Escalamiento con número de clusters (n=32768, eta=0.01)
// ============================================================================
println("\n" + "=" * 80)
println("PRUEBA 2: Escalamiento con número de clusters (n=32768, eta=0.01)")
println("=" * 80)

val clusters2 = Seq(2, 4, 8, 16, 32, 64, 128, 256)
clusters2.zipWithIndex.foreach { case (k, i) =>
  pruebaDesempeno(32768, k, 0.01, seed = 2000L + i)
}

// ============================================================================
// PRUEBA 3: Impacto de eta (n=16384, k=16)
// ============================================================================
println("\n" + "=" * 80)
println("PRUEBA 3: Impacto del umbral eta (n=16384, k=16)")
println("=" * 80)

val etas3 = Seq(0.1, 0.01, 0.001)
etas3.zipWithIndex.foreach { case (e, i) =>
  pruebaDesempeno(16384, 16, e, seed = 3000L + i)
}

// ============================================================================
// PRUEBA 4: Casos extremos
// ============================================================================
println("\n" + "=" * 80)
println("PRUEBA 4: Casos extremos")
println("=" * 80)

println("\nCaso 4A: Pocos puntos, muchos clusters")
pruebaDesempeno(256, 64, 0.01, seed = 4000L)

println("\nCaso 4B: Muchos puntos, pocos clusters")
pruebaDesempeno(262144, 4, 0.01, seed = 4001L)

println("\nCaso 4C: Entrada masiva")
pruebaDesempeno(1048576, 32, 0.01, seed = 4002L)

println("\n" + "=" * 80)
println("FIN DEL INFORME DE DESEMPEÑO")
println("=" * 80)



// ---------------------------------------------------------------------------
// PRUEBAS 1: Generación de puntos y medianas iniciales
// ---------------------------------------------------------------------------

println("===== PRUEBA 1: Generando puntos y medianas =====")

val puntos16_3 = generarPuntos(3, 16).toSeq
println(s"Puntos generados (${puntos16_3.length}):")
puntos16_3.foreach(println)

val medianasIni_3 = inicializarMedianas(3, puntos16_3)
println("\nMedianas iniciales:")
medianasIni_3.foreach(println)

// ---------------------------------------------------------------------------
// PRUEBAS 2: Clasificación Secuencial y Paralela
// ---------------------------------------------------------------------------

println("\n===== PRUEBA 2: Clasificación Secuencial =====")
val clasifSeq = clasificarSeq(puntos16_3, medianasIni_3)
clasifSeq.foreach { case (m, ps) =>
  println(s"Mediana $m -> ${ps.length} puntos")
}

println("\n===== PRUEBA 2B: Clasificación Paralela =====")
val clasifPar = clasificarPar(1024)(puntos16_3, medianasIni_3)
clasifPar.foreach { case (m, ps) =>
  println(s"Mediana $m -> ${ps.length} puntos")
}

// ---------------------------------------------------------------------------
// PRUEBAS 3: Actualización de medianas
// ---------------------------------------------------------------------------

println("\n===== PRUEBA 3: Actualización Secuencial =====")
val nuevasSeq = actualizarSeq(clasifSeq, medianasIni_3)
println("Medianas nuevas (Sec):")
nuevasSeq.foreach(println)

println("\n===== PRUEBA 3B: Actualización Paralela =====")
val nuevasPar = actualizarPar(clasifPar, medianasIni_3)
println("Medianas nuevas (Par):")
nuevasPar.foreach(println)

// ---------------------------------------------------------------------------
// PRUEBAS 4: Convergencia
// ---------------------------------------------------------------------------

println("\n===== PRUEBA 4: Verificación de convergencia =====")
val eta = 0.01
val convSeq = hayConvergenciaSeq(eta, medianasIni_3, nuevasSeq)
val convPar = hayConvergenciaPar(eta, medianasIni_3, nuevasPar)
println(s"¿Convergencia secuencial? $convSeq")
println(s"¿Convergencia paralela?  $convPar")

// ---------------------------------------------------------------------------
// PRUEBAS 5: Algoritmo completo K-Medianas (Secuencial y Paralelo)
// ---------------------------------------------------------------------------

println("\n===== PRUEBA 5: Algoritmo completo =====")

val medianasFinalSeq = kMedianasSeq(puntos16_3, medianasIni_3, eta)
println("\nMedianas finales (Secuencial):")
medianasFinalSeq.foreach(println)

val medianasFinalPar = kMedianasPar(puntos16_3, medianasIni_3, eta)
println("\nMedianas finales (Paralela):")
medianasFinalPar.foreach(println)

// ---------------------------------------------------------------------------
// PRUEBAS 6: Medición de tiempos y visualización
// ---------------------------------------------------------------------------

println("\n===== PRUEBA 6: Benchmarks y Visualización =====")
val tiempos = tiemposKmedianas(puntos16_3, 3, eta)
println(s"Tiempos (ms) -> Seq: ${tiempos._1}  Par: ${tiempos._2}  Aceleración: ${tiempos._3}")

println("\nGenerando visualización de clusters...")
// si el worksheet se cuelga al graficar, comenta esta línea y ejecútala luego en `sbt run`
graficarKmedianas(puntos16_3, 3, eta)

println("\n===== FIN DE LAS PRUEBAS =====")