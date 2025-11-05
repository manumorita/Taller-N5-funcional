import kmedianas2D._
import Benchmark._

// Helpers de comparación
def cercaD(a: Double, b: Double, eps: Double = 1e-6): Boolean =
  math.abs(a - b) <= eps

def cercaP(p: Punto, q: Punto, eps: Double = 1e-6): Boolean =
  cercaD(p.x, q.x, eps) && cercaD(p.y, q.y, eps)

def todasCerca(as: Seq[Punto], bs: Seq[Punto], eps: Double = 1e-6): Boolean =
  (as.length == bs.length) && as.zip(bs).forall { case (p, q) => cercaP(p, q, eps) }

def cuentasClusters(clasif: Map[Punto, Seq[Punto]]): Seq[Int] =
  clasif.values.map(_.size).toSeq.sorted



// Pruebas de CORRECCIÓN (seq vs par) - resultados esperados


// Caso pequeño hecho a mano (2 clusters)
// Esperado:
// cuentasClusters iguales (3 y 3)  - true
// medianas actualizadas ~iguales   - true
val A = Seq(Punto(0.05, 0.10), Punto(0.10, 0.05), Punto(0.15, 0.12))
val B = Seq(Punto(1.05, 0.95), Punto(0.90, 1.10), Punto(1.10, 1.05))
val ptsPeq: Seq[Punto] = A ++ B
val medsIni2 = Seq(A.head, B.head)

// Secuencial
val clasifS = clasificarSeq(ptsPeq, medsIni2)
val medsS1  = actualizarSeq(clasifS, medsIni2)

// Paralelo
val clasifP = clasificarPar(umbral = 1)(ptsPeq, medsIni2)
val medsP1  = actualizarPar(clasifP, medsIni2)

// Verificaciones
val okCuentasPeq: Boolean = cuentasClusters(clasifS) == cuentasClusters(clasifP)          // esperado: true
val okMedianasPeq: Boolean = todasCerca(medsS1, medsP1, eps = 1e-9)                        // esperado: true
(okCuentasPeq, okMedianasPeq) // esperado: (true, true)


// Ejecución completa (k=3, n=16)
// Esperado:
// medianas finales seq ~ par    → true
val puntos16_3 = generarPuntos(3, 16).toSeq
val meds16_3   = inicializarMedianas(3, puntos16_3)
val medsSeq16  = kMedianasSeq(puntos16_3, meds16_3, eta = 0.01)
val medsPar16  = kMedianasPar(puntos16_3, meds16_3, eta = 0.01)
val okKmeans16: Boolean = todasCerca(medsSeq16, medsPar16, eps = 1e-6)
okKmeans16 // esperado: true


// Convergencia
// Esperado:
// convGrandeSeq = true, convChicaSeq = false
// convGrandePar = true, convChicaPar = false
val convGrandeSeq = hayConvergenciaSeq(eta = 1.0,   meds16_3, medsSeq16)   // esperado: true
val convChicaSeq  = hayConvergenciaSeq(eta = 1e-12, meds16_3, medsSeq16)   // esperado: false
val convGrandePar = hayConvergenciaPar(eta = 1.0,   meds16_3, medsPar16)   // esperado: true
val convChicaPar  = hayConvergenciaPar(eta = 1e-12, meds16_3, medsPar16)   // esperado: false
(convGrandeSeq, !convChicaSeq, convGrandePar, !convChicaPar) // esperado: (true, true, true, true)


// Caso medio (k=32, n=512) – rápido y útil para equivalencia
// Esperado:
// medianas finales seq ~ par    → true
val puntos512_32 = generarPuntos(32, 512).toSeq
val meds512_32   = inicializarMedianas(32, puntos512_32)
val medsSeqM     = kMedianasSeq(puntos512_32, meds512_32, eta = 0.01)
val medsParM     = kMedianasPar(puntos512_32, meds512_32, eta = 0.01)
val okKmeansMedio: Boolean = todasCerca(medsSeqM, medsParM, eps = 1e-6)
okKmeansMedio // esperado: true


// Resultado global de CORRECCIÓN
val todoOk: Boolean = okCuentasPeq && okMedianasPeq && okKmeans16 && okKmeansMedio
todoOk // esperado: true


// Benchmarks + Gráficas HTML

//Benchmark
//Esperado:
//Se imprimen tiempos en ms y speedup (Double)
//Se generan kmedianasSeq.html y kmedianasPar.html en la raíz del proyecto
val (tSeqSmall, tParSmall, speedupSmall) = tiemposKmedianas(puntos16_3, 3, 0.01)
(tSeqSmall.value, tParSmall.value, speedupSmall) // esperado: (ms, ms, factor)
probarKmedianas(puntos16_3, 3, 0.01)              // esperado: genera los 2 HTML


// Benchmark moderado (sigue siendo razonable)
// Esperado:
// Tiempos y speedup visibles; no debería colgar el worksheet
val (tSeqMid, tParMid, speedupMid) = tiemposKmedianas(puntos512_32, 32, 0.01)
(tSeqMid.value, tParMid.value, speedupMid)


