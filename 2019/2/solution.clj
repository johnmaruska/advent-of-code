(ns solution)

(def input-file "input.txt")

(defn ->int [x]
  (Integer/parseInt x))
(defn ->ints [v]
  (map ->int v))

(defn read-input []
  (-> (slurp input-file)
      clojure.string/trim-newline
      (clojure.string/split #",")
      ->ints))

(defn get-pointer-val
  "Given a position, get the value from the address stored in that position."
  [program pointer]
  (->> pointer
       (get @program)
       (get @program)))

(defn op! [f program start-pos]
  (let [val-1   (get-pointer-val program (+ 1 start-pos))
        val-2   (get-pointer-val program (+ 2 start-pos))
        out-pos (get @program (+ 3 start-pos))
        result  (f val-1 val-2)]
    (swap! program assoc out-pos result)))

(defn add-op! [program address]
  (op! + program address))

(defn mul-op! [program address]
  (op! * program address))

(defn run!
  "Run the program, expected to be an (atom (vec ..)), which will be mutated
  through the procedure."
  [program]
  (loop [address 0]
    (let [opcode (get @program address)]
      (case opcode
        1     (do (add-op! program address)
                  (recur (+ 4 address)))
        2     (do (mul-op! program address)
                  (recur (+ 4 address)))
        99    :halt
        :else :break))))

(defn part-1 [input]
  (let [program (atom (vec input))]
    (run! program)
    (println program)))

(defn matches? [program noun verb goal]
  (let [modified-program (atom (assoc program
                                      1 noun
                                      2 verb))]
    (run! modified-program)
    (= goal (get @modified-program 0))))

(defn part-2 [input]
  (let [program   (vec input)
        goal      19690720
        tot-addrs (count program)]
    ;; naive-search, could do a sorted-binary search terms to narrow faster
    (println
     (for [noun (range tot-addrs)
           verb (range tot-addrs)
           :when (matches? program noun verb goal)]
       (+ (* 100 noun) verb)))))

(defn main []
  (part-2 (read-input)))
