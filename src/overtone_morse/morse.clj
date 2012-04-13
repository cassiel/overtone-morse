;; Morse code generator for Overtone.
;; Nick Rothwell, nick@cassiel.com
;; @cassieldotcom

;; The code here is pretty simple (even naive): a string of
;; characters is transformed into a sequence of future-timestamped
;; events, each of which generates a synthesiser which puts out
;; a beep and then frees itself. The whole set of events is
;; scheduled in one go, so the Morse output is asynchronous.
;; Most of the internal functions return timestamps for the end of
;; their role, so we need do little more than a pile of reduce
;; calls to fire everything off.

(ns overtone-morse.morse
  (:use overtone.live)
  (:require [clojure.string :as s]))

;; Timings. We should really be calculating msec times from a WPM figure.

(def DOT-MS 100)
(def DASH-MS (* 3 DOT-MS))
(def TONE-GAP-MS DOT-MS)
(def CHAR-GAP-MS (* 3 DOT-MS))
(def WORD-GAP-MS (* 5 DOT-MS))

(def FREQ 440)

(def CODE {\A ". -"
           \B "- . . ."
           \C "- . - ."
           \D "- . ."
           \E "."
           \F ". . - ."
           \G "- - ."
           \H ". . . ."
           \I ". ."
           \J ". - - -"
           \K "- . -"
           \L ". - . ."
           \M "- -"
           \N "- ."
           \O "- - -"
           \P " . - - ."
           \Q "- - . -"
           \R ". - ."
           \S ". . ."
           \T "-"
           \U ". . -"
           \V ". . . -"
           \W ". - -"
           \X "- . . -"
           \Y "- . - -"
           \Z "- - . ."

           \0 "- - - - -"
           \1 ". - - - -"
           \2 ". . - - -"
           \3 ". . . - -"
           \4 ". . . . -"
           \5 ". . . . ."
           \6 "- . . . ."
           \7 "- - . . ."
           \8 "- - - . ."
           \9 "- - - - ."

           \. ". - . - . -"
           \: "- - - . . ."
           \, "- - . . - -"
           \; "- . - . - ."
           \? ". . - - . ."
           \= "- . . . -"
           \' ". - - - - ."
           \+ ". - . - ."
           \! "- . - . - -"
           \- "- . . . . -"
           \/ "- . . - ."
           \_ ". . - - . -"
           \( "- . - - ."
           \" ". - . . - ."
           \) "- . - - . -"
           \$ ". . . - . . -"
           \& ". - . . ."
           \@ ". - - . - ."})

(defsynth beep
  "Rather ugly: creates a synth which beeps and then frees itself."
  [len DOT-MS]
  (out 0 (* (line 1.0 1.0 (/ len 1000.0) :action FREE)
            (sin-osc FREQ))))

(defn beep-char-bits
  "Beep a character from a vector of 0/1 values. We do an immediate
   reduce over the bits and schedule the beeps into the future.
   Returns the timestamp one tone-gap after the last beep."
  [start-t bits]
  (reduce (fn [t bit]
            (let [len ([DOT-MS DASH-MS] bit)]
              (at t (beep len))
              (+ t (+ len TONE-GAP-MS))))
          start-t
          bits))

(defn beep-char
  "Beep out a character (case-sensitive), return timestamp
   at the end of the last beep. Just returns start time for
   character not found."
  [start-t ch]
  (if (contains? CODE ch)
    (let [bits (map {"." 0 "-" 1}
                    (clojure.string/split (CODE ch) #"\s+"))]
      (- (beep-char-bits start-t bits)
         TONE-GAP-MS))
    start-t))

(defn beep-chars
  "Beep out characters, putting out a word gap for spaces.
   Returns the time position for the end of the last beep.
   Will get its sums wrong for endpoint or repeated spaces."
  [start-t chars]
  (- (reduce (fn [t ch]
               (if (= ch \space)
                 (+ t WORD-GAP-MS (- CHAR-GAP-MS))
                 (+ CHAR-GAP-MS (beep-char t ch))))
             start-t chars)
     CHAR-GAP-MS))

(defn morse
  "Beep a character string, starting now. The string is
   trimmed, purged of repeated spaces and upcased."
  [text]
  (let [sanitized (s/replace (s/upper-case (s/trim text))
                             #"\s+" " ")]
    (beep-chars (now) (map #(get sanitized %)
                           (range (count sanitized)))))
  )
