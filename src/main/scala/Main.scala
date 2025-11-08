object Main extends App {
  import kmedianas2D._
  import Benchmark._

  val k = 3
  val eta = 0.01
  val puntos = generarPuntos(k, 16)

  println("=== Verificando ejecución fuera del Worksheet ===")
  val (tSeq, tPar, speedup) = probarKmedianas(puntos, k, eta)
  println(f"Tiempos -> Seq: $tSeq%.3f ms  Par: $tPar%.3f ms  Speedup: $speedup%.2fx")

  println("\nGenerando gráficos HTML...")
  graficarKmedianas(puntos, k, eta)
  println("Archivos listos en target/plots/: kmedianasSeq.html y kmedianasPar.html")
}
