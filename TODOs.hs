

      "  ************ TODOs *************   "


" ************  Schriftliche Thesis"
* Kapitel über Algorithmuserklärung und Implementierungen kann so etwa bleiben, braucht aber mehr Erklärungen.

* Einleitung muss geschrieben werden (die Papers werden eher nicht gelesen!)
  * Basic: Parallel, Haskell functional Programming
  * Special: Nested Data Parallelsim, Histogram Balancing

* Verständlich auf dem Punkt!

* Struktur
  1. Alle Implementierungen
  2. Transformation ndpv -> ndpn
  3. Komplexitäten
  4. Vergleich

* Es geht um den Kerngedanken! Und nicht um einen Technical Report mit Reproduzierbarkeit.

* Genau erläuteren und präzisieren wie Work&Depth mit der Anzahl der Prozessoren in den distributed Types zusammenhängen.
  * Die tatsächliche Parallelität steck in der Anzahl der PUs (Processing Units) und den verteilten Algorithmen
    zwischen den einzelnen PUs. Damit wird sum auf D(log n) gedrückt. Damit wird ebenfalls 

* Twofold interpretation: divL = <built-in parallel divL> OR < mapD divS> with distributed types and extended library optimization

* sumP implementierung als Beispiel für Laufzeiteinschätung mit Work/Depth und Anzahl von Prozessoren #17

* Erklärung der Types: # 25

* Ab jetzt, Details lieber weglassen.

* Seitenanzahl:
  * Basis -> 15
  * Impls, Vect, Anaylsis -> 40
  * Evaluierung -> 10 
  * Summe: 65 "Seiten bereits!"

* Codeerklärungen lieber stückweise - Code für Menschen!

" ************  Fragen"
* Work Efficient Vectorization genauer durchgehen. wie die Replikation von Nested Arrays effizienter gemacht werden kann.

" ************  Organisation"
OK Der Algorithmus soll "sinngemäß" implementiert werden


" ************ Sonstiges"

VERWERFEN: lifted hBalance verwerfen

NENNEN hbalanceBulk? Verwerfen oder nennen?


"  ************  Kernarbeit"

OK Distributed Types und Fusioning durchführen (e.g myFuncPS = joinD . mapD myFuncS . splitD)

DONE: Konkreter Algorithmus von Pndpn
DONE: Konkreter Algorithmus von Pndpv
* Laufzeiteinschätzung von Pndpv

OK ParAccuHist >=> Normalize >=> Scale >=> Apply als P_man Algorithmus

OK Pseudocode, Divide&Conquer Parallel Histogram Balancing

* Größenvergleiche: n vs. gmax, n*log n vs. gmax
  Am besten zweidimensionaler Graph!

OK Vergleich Divide&Conquer vs Data-Parallel

OK Was tun über PNested und Replikation?
    "Kommt nur einmal am Ende bei der Replikation des Histograms vor."
    "Es mag wie eine Laufzeitexplosion vorkommen, aber das ist es aufgrund einer klugen Replikation"
    "von Nested Arrays nicht. Darauf wird aber nicht länger eingegangen, denn es verschlimmert weder die Laufzeit"
    "noch bringt das konkrete Wissen darüber weitere Erkenntnisse."

DONE: specify execution oder with a let
VERFERFEN: rewrites and special semantics of accumulator calculation
FALSCH: Überall: floor -> round?
VERWORFEN: P_man mit CountingSort unter Berücksichtigung des Kontextes


" ************ ???? "
next: replication of an array: replPS n a = AArr (cycleVector n a) [(0,n),(n,n),...,(n*n-n,n)]
               cycleVector :: Int -> Vector a -> Vector a
               cycleVector 3 [a,b,c] = [a,b,c,a,b,c,a,b,c]


