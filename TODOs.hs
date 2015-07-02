

      "  ************ TODOs *************   "



"  ************  Kernarbeit"

* Distributed Types und Fusioning durchführen (e.g myFuncPS = joinD . mapD myFuncS . splitD)

* specify execution oder with a let

* Twofold interpretation: divL = <built-in parallel divL> OR < mapD divS> with distributed types and extended library optimization

* Überall: floor -> round?

DONE: Konkreter Algorithmus von Pndpn
* Konkreter Algorithmus und Laufzeiteinschätzung von Pndpv

* Genau erläuteren und präzisieren wie Work&Depth mit der Anzahl der Prozessoren in den distributed Types zusammenhängen.
  * Die tatsächliche Parallelität steck in der Anzahl der PUs (Processing Units) und den verteilten Algorithmen
    zwischen den einzelnen PUs. Damit wird sum auf D(log n) gedrückt. Damit wird ebenfalls 

OK ParAccuHist >=> Normalize >=> Scale >=> Apply als P_man Algorithmus

OK Pseudocode, Divide&Conquer Parallel Histogram Balancing

* Größenvergleiche: n vs. gmax, n*log n vs. gmax

* Vergleich Divide&Conquer vs Data-Parallel

* Was tun über PNested und Replikation?

VERFERFEN: rewrites and special semantics of accumulator calculation
Abgelehnt: P_man mit CountingSort unter Berücksichtigung des Kontextes

" ************  Fragen"
* Work Efficient Vectorization genauer durchgehen. wie die Replikation von Nested Arrays effizienter gemacht werden kann.

" ************  Organisation"
* Der Algorithmus soll "sinngemäß" implementiert werden


" ************  Schriftliche Thesis"
* Kapitel über Algorithmuserklärung und Implementierungen kann so etwa bleiben, braucht aber mehr Erklärungen.

* Einleitung muss geschrieben weredn (die Papers werden eher nicht gelesen!)
  * Basic: Parallel, Haskell functional Programming
  * Special: Nested Data Parallelsim, Histogram Balancing

* Verständlich auf dem Punkt!

* Struktur
  1. Alle Implementierungen
  2. Transformation ndpv -> ndpn
  3. Komplexitäten
  4. Vergleich

* Es geht um den Kerngedanken! Und nicht um einen Technical Report mit Reproduzierbarkeit.

* sumP implementierung als Beispiel für Laufzeiteinschätung mit Work/Depth und Anzahl von Prozessoren #17

* Ab jetzt, Details lieber weglassen.

* Seitenanzahl:
  * Basis -> 15
  * Impls, Vect, Anaylsis -> 40
  * Evaluierung -> 10 
  * Summe: 65 "Seiten bereits!"

* Codeerklärungen lieber stückweise - Code für Menschen!



" ************ Sonstiges"

VERWERFEN: lifted hBalance verwerfen

* hbalanceBulk? Verwerfen oder nennen?


" ************ ???? "
next: replication of an array: replPS n a = AArr (cycleVector n a) [(0,n),(n,n),...,(n*n-n,n)]
               cycleVector :: Int -> Vector a -> Vector a
               cycleVector 3 [a,b,c] = [a,b,c,a,b,c,a,b,c]
next: inline replPS


