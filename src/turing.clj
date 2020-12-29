(ns turing)

;; tape

(def ex-tape
  "we can represent the tape, alongside the 'current position'
  by keeping track of a 'left' list, a current value, and a 'right' list"
  [[] nil []])

(defn turing-move-right [[left cur right]]
  [(list* cur left)
   (first right)
   (rest right)])

(comment
  (turing-move-right [[] nil []])
  (turing-move-right [[nil] 1 [0]]))

(defn turing-move-left [[left cur right]]
  [(rest left)
   (first left)
   (list* cur right)])

(comment
  (turing-move-left [[] nil []])
  (turing-move-left [[:x] 1 [0]]))

(defn turing-erase [[left _ right]]
  [left nil right])

(comment
  (turing-erase [[0] 1 [0]]))

(defn turing-print [[left _ right] v]
  [left v right])

(comment
  (turing-print [[] nil []] 1))

(defn display-block [block] (or block '_))
(defn tape->pretty-seq [[left cur right]]
  (map
   display-block
   (concat (reverse left) (list (list (display-block cur))) right)))

;; machine

(def machine
  "we can represent a machine, by describing, what happens
  for a specific machine state and tape value

  for example
  [:begin nil :-> ..]

  says if the machine is in a :begin state and the tape has no value, then,

  [.. [:print 0] :begin]

  print 0 on the current tape block, and change the machine state to :begin"

  [[:begin nil :-> [[:print 0]] :begin]
   [:begin 0   :-> [:move-right :move-right [:print 1]] :begin]
   [:begin 1   :-> [:move-right :move-right [:print 0]] :begin]])

;; run

(def ex-configuration {:machine machine
                       :current-state :begin
                       :tape [[] nil []]})

(defn find-instruction [{:keys [machine current-state tape]}]
  (let [[_ current-tape _] tape]
    (first
     (filter
      (fn [[begin-state desired-tape]]
        (let [desired-tape-set (if (set? desired-tape) desired-tape #{desired-tape})]
          (and (= current-state begin-state)
               (contains? desired-tape-set current-tape))))
      machine))))

(defn apply-action [configuration action]
  (cond
    (= action :move-left)
    (update configuration :tape turing-move-left)

    (= action :move-right)
    (update configuration :tape turing-move-right)

    (= action :erase)
    (update configuration :tape turing-erase)

    (= (first action) :print)
    (update configuration :tape turing-print (second action))))

(defn run-step [configuration]
  (let [[_ _ _ actions end-state] (find-instruction configuration)]
    (assoc (reduce apply-action configuration actions)
           :current-state
           end-state)))

(defn run-loop [configuration end]
  (loop [config configuration
         i 0]
    (if (= i end)
      config
      (let [{:keys [current-state tape] :as config'} (run-step config)]
        (println (str "state: " current-state " | ") (tape->pretty-seq tape))
        (Thread/sleep 200)
        (recur config' (inc i))))))

(comment
  (-> ex-configuration run-step run-step run-step :tape tape->pretty-seq)
  (future (run-loop ex-configuration 10)))


;; transc

;; let's print the sequence 001011011101111...


(def transc-machine
  [[:begin nil :-> [[:print :sentinel] :move-right
                    [:print :sentinel] :move-right
                    [:print 0] :move-right :move-right
                    [:print 0] :move-left :move-left] :o]

   [:o 1 :-> [:move-right [:print :todo] :move-left :move-left :move-left] :o]
   [:o 0 :-> [] :q]

   [:q #{0 1} :-> [:move-right :move-right] :q]
   [:q nil    :-> [[:print 1] :move-left] :p]

   [:p :todo     :-> [:erase :move-right] :q]
   [:p :sentinel :-> [:move-right] :f]
   [:p nil       :-> [:move-left :move-left] :p]

   [:f #{0 1} :-> [:move-right :move-right] :f]
   [:f nil    :-> [[:print 0] :move-left :move-left] :o]])

(def transc-configuration {:machine transc-machine
                           :current-state :begin
                           :tape [[] nil []]})

(comment
  (future (run-loop transc-configuration 140)))
