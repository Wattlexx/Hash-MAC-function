;Author Nicholas Pinney
;Date Nov 10, 2019

(ns hash-v2.core
  (:gen-class)
  (:require [clojure.string :as str]))
(def alphabetMap [{:char \a :value 0}
                  {:char \b :value 1}
                  {:char \c :value 2}
                  {:char \d :value 3}
                  {:char \e :value 4}
                  {:char \f :value 5}
                  {:char \g :value 6}
                  {:char \h :value 7}
                  {:char \i :value 8}
                  {:char \j :value 9}
                  {:char \k :value 10}
                  {:char \l :value 11}
                  {:char \m :value 12}
                  {:char \n :value 13}
                  {:char \o :value 14}
                  {:char \p :value 15}
                  {:char \q :value 16}
                  {:char \r :value 17}
                  {:char \s :value 18}
                  {:char \t :value 19}
                  {:char \u :value 20}
                  {:char \v :value 21}
                  {:char \w :value 22}
                  {:char \x :value 23}
                  {:char \y :value 24}
                  {:char \z :value 25}
                  {:char \space :value 26}])
(defn get-upper-bound
  "This function calculates how much padding to add to the string to make it a multiple of 25"
  [number n]
  (loop [remaining-length 0]
    (if (zero? (rem (+ remaining-length number) n))
      remaining-length
      (recur (inc remaining-length)))))
(defn add-padding
  "Adds padding (spaces) to string to make it a multiple of 25"
  [plaintext]
  (let [remaining-length (get-upper-bound (count plaintext) 25)
        text-with-padding (str/lower-case plaintext)]
    (if (zero? remaining-length)
      text-with-padding
      (str text-with-padding (subs "                          " 0 remaining-length)))))
(defn char-to-num
  "Converts a single character to it's corresponding number based on alphabetMap"
  [character]
  (let [relation (first (filter (comp #{character} :char) alphabetMap))]
    (get relation :value)))
(defn num-to-char
  "Converts a single number to a character based on alphabetMap"
  [number]
  (let [relation (first (filter (comp #{number} :value) alphabetMap))]
    (get relation :char)))           
(defn string-to-char-seq
  "Takes the padded plaintext and converts the string into a sequence of chars for it to be passed to char-to-num."
  [padded-plaintext]
  (seq (char-array padded-plaintext)))
(defn get-char-at
  "This function uses drop to get the character in the string at position char-at-position. Used during hash rounds."
  [a-string char-at-position]
  (if (zero? char-at-position)
    (first a-string)
    (first (drop char-at-position a-string))))
(defn string-from-five
  "This function extracts a char from each of the 5 strings in the 5x5 row-wise block."
  [five-strings char-at]
  ;(prn five-strings) ; DEBUGGING prints the full 5 strings
  (reduce (fn [result string]
            (conj result (get-char-at string char-at)))
    []
    five-strings))
(defn string-from-five-shift
  "This funciton extracts a char from each of the 5 strings in the 5x5 row-wise block but increments the extraction position after each extraction."
  [five-strings char-at]
  ;(prn five-strings) ;DEBUGGING prints the full 5 strings
  (reduce (fn [result [str-index string]]
            (conj result (get-char-at string (rem (+ str-index (inc char-at)) 5))))
    []
    (map-indexed vector five-strings)))
(defn sum-of-chars
  "This function is used to calculate the sum of the 5 characters."
  [character-array]
  ;(print character-array "= ") ;DEBUGGING prints the array of characters
  (reduce (fn [total char] 
            ;(print (char-to-num char) " ") ;DEBUGGING prints the value of each character during addition of each
            (+ (char-to-num char) total)) 
    0
    character-array))
(defn rem-27
  "This function returns the remainder of the num. Used to keep the sum below 27."
  [num]
  (rem num 27))
(defn round-one
  "This function does the first round of the hash function."
  [partitioned-string]
  ;(println "Round One") ;DEBUGGING 
  (let [result []
        sum-letters-r-one (comp rem-27 sum-of-chars string-from-five)]
    (conj result (sum-letters-r-one partitioned-string 0)
                 (sum-letters-r-one partitioned-string 1)
                 (sum-letters-r-one partitioned-string 2)
                 (sum-letters-r-one partitioned-string 3)
                 (sum-letters-r-one partitioned-string 4))))
(defn round-two
  "This function does the second round of the hash function."
  [partitioned-string]
  ;(println "\nRound Two") ;DEBUGGING 
  (let [result []
        sum-letters-r-two (comp rem-27 sum-of-chars string-from-five-shift)]
    (conj result (sum-letters-r-two partitioned-string 0)
                 (sum-letters-r-two partitioned-string 1)
                 (sum-letters-r-two partitioned-string 2)
                 (sum-letters-r-two partitioned-string 3)
                 (sum-letters-r-two partitioned-string 4))))
(defn round-three
  "This function does the third round of the hash function."
  [partitioned-string]
  ;(println "\nRound Three") ;DEBUGGING 
  (let [result []
        sum-letters-r-three (comp rem-27 sum-of-chars string-from-five)]
    (conj result (sum-letters-r-three partitioned-string 0)
                 (sum-letters-r-three partitioned-string 1)
                 (sum-letters-r-three partitioned-string 2)
                 (sum-letters-r-three partitioned-string 3)
                 (sum-letters-r-three partitioned-string 4))))
(defn output-of-hash 
  "This function takes the sum of all the rounds and converts it is characters."
  [sum-of-rounds]
  (reduce (fn [result number]
            (str result (num-to-char number)))
    ""
    sum-of-rounds))
(defn hash-function
  "This function defines the hash function of Problem 1."
  [plaintext]
  (let [pt (add-padding plaintext)
        p-pt (re-seq #".{1,5}" pt)]
    (loop [c 0
           strings []
           total [0 0 0 0 0]]
      (if (= 5 (count strings))
        (do
          (let [r-one (round-one strings)
                r-two (round-two strings)
                r-three (round-three strings)
                sum-of-rounds (map (comp rem-27 +) r-one r-two r-three)
                resulting (map (comp rem-27 +) sum-of-rounds total)]
            ;(println "\nOutput of the three rounds:" sum-of-rounds) ;DEBUGGING 
            (recur c [] resulting)))
        (if (= (count p-pt) c)
          (str/upper-case (output-of-hash total))
          (recur (inc c) (conj strings (nth p-pt c)) total))))))   
(defn mac-function
  "This function defines the MAC function of Problem 2."
  [key plaintext]
  (let [string (-> (str key plaintext) (hash-function))]
    (-> (str key string) (hash-function))))



(defn -main 
  "This function gets called and executed when you press Run."
  []
  (def plaintext-string-one "abcdefghi jklmnopqrstuvwx")
  (def plaintext-string-two "the birthday attack can be performed for any hash functions including sha three")
  (let [pt-two plaintext-string-two
        pt-one plaintext-string-one
        key "abcde"]
    (print "\nString: ")
    (prn pt-two)
    (print "Output of hash function: ")
    (prn (hash-function pt-two))
    (print "\nUsing key: ")
    (prn key)
    (print "Output of the MAC function: ")
    (prn (mac-function key pt-two))))
    
