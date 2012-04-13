(require '[overtone-morse.morse :as m])
(use 'overtone.live)

(volume 1.0)

(m/beep-char-bits (now) [0 0 0])
(m/beep-char-bits (now) [1 1 1])

(m/beep-chars (now) [\S \O \S])
(m/beep-chars (now) [\@])


(defn time-it [morse] (format "morse time %.2f seconds"
                              (/ (- morse (now)) 1000.0)))


;; For Titanic fans:
(time-it (m/morse "SOS SOS SOS"))
;; For Nokia fans:
(time-it (m/morse "SMS SMS SMS"))

(time-it (m/morse "Morse for Overtone by Nick Rothwell, nick@cassiel.com, @cassieldotcom"))

(time-it (m/morse "Calling all. This is our last cry before our eternal silence."))
                                        ; http://www.economist.com/node/183572
