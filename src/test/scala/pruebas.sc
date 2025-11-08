import Benchmark._
import kmedianas2D._
import scala.util.Random

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

