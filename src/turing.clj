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

(defn turing-print [[left _ right] v]
  [left v right])

(comment
  (turing-print [[] nil []] 1))

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
          (and (= current-state begin-state)
               (= current-tape desired-tape)))
        machine))))

(defn apply-action [configuration action]
  (cond
    (= action :move-right)
    (update configuration :tape turing-move-right)

    (= (first action) :print)
    (update configuration :tape turing-print (second action))))

(defn run-step [configuration]
  (let [[_ _ _ actions end-state] (find-instruction configuration)]
    (assoc (reduce apply-action configuration actions)
           :current-state
           end-state)))

(defn tape->seq [[left cur right]]
  (concat
    (reverse left)
    (list cur)
    right))

(defn run-loop [configuration]
  (loop [config configuration]
    (let [{:keys [tape] :as config'} (run-step config)]
      (println (tape->seq tape))
      (Thread/sleep 1000)
      (recur config'))))


(comment
  (-> ex-configuration run-step run-step run-step :tape tape->seq)
  (future (run-loop ex-configuration)))






