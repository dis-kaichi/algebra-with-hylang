#!/usr/bin/env hy
(import [sympy [*]])
(import [sympy.functions.combinatorial.numbers [*]])
(require [hy.extra.anaphoric [ap-if ap-pipe]])
(require [hy.contrib.loop [loop]])

(defn solve-semi-2nd-1 []
  ;; 準2級 1次
  ;; https://www.su-gaku.net/common/pdf/support_sample/question/j2q_q_1ji.pdf

  (setv (, x y) (symbols "x y"))
  (setv (, a b) (symbols "a b"))
  ;; [1]
  ;; (1) 展開 : (a + x) (b - y)
  (print (expand (* (+ a x) (- b y))))
  ;; (2) 計算 : (sqrt(7) + sqrt(3))^2
  (print (simplify (** (+ (sqrt 7) (sqrt 3)) 2)))
  ;; (3) 因数分解 : x^2 + 24x - 25
  (print (factor (+ (** x 2) (* 24 x) (- 25))))
  ;; (4) 方程式 : (2x - 1)^2 = 5
  (print (solveset (Eq (** (- (* 2 x) 1) 2) 5) x))
  ;; (5) aについて解く : y = ax^2 where x = 4, y = 2
  (print (-> (Eq y (* a (** x 2)))
             (.subs x 4)
             (.subs y 2)
             (solveset a)))
  ;; [2]
  ;; (6) 角度の問題
  ;;  どうする？
  ;; (7) 円の半径
  ;;  どうする？

  ;; (8) 展開 : (a + 2) (a + 1) (a - 2)
  (print (-> (* (+ a 2) (+ a 1) (- a 2))
             expand))

  ;; (9) 因数分解 : x^2 - xy + 2x - 2y
  (print (-> (+ (** x 2) (- (* x y)) (* 2 x) (- (* 2 y)))
             factor))
  ;; (10) 方程式 : |x| = |x - 3|
  ;;      (*) real = True が必要
  (setv x (apply symbols ["x"] {:real True}))
  (print (ap-pipe [(abs x) 2]
                  (apply Eq it {})
                  (solve it x)))

  ;; [3]
  ;; (11) 頂点を求める : y = 2x^2 + 12x + 7
  ;;      (*) 微分してから解を求める
  (print (-> (+ (* 2 (** x 2)) (* 12 x) 7)
             (.diff x)
             (solve x)))
  ;; (12) 不等式 : x^2 + 5x - 14 >= 0
  (print (-> (>= (+ (** x 2) (* 5 x) (- 14)) 0)
             (solve-univariate-inequality x)))

  ;; (13) 値を求める : tan135度
  ;;      (*) うまくラジアンで回答は出せないのか？
  (print (-> (tan 135)
             (.evalf)))
  ;; (14-1) 値を求める : 7P3
  (print (-> (nP 7 3)))
  ;; (14-2) 値を求める : 7C3
  (print (-> (nC 7 3)))
  ;; (15-1) (101)2 を10進法で表す
  (print (-> "0b101"
             (int 2)))
  ;; (15-2) (224)5 を10進法で表す
  ;; http://techtipshoge.blogspot.jp/2010/05/10n.html
  (defn base-10-from [num-list b]
    (loop [[n 0]
           [xs num-list]]
      (if (empty? xs)
        n
        (do
          (setv n (* n b))
          (recur (+ n (int (first xs))) (list (rest xs)))))))

  (print (-> "224"
             (base-10-from 5)))
  )

(defn solve-semi-2nd-2 []
  ;; 準2級 2次
  ;; https://www.su-gaku.net/common/pdf/support_sample/question/j2q_q_2ji.pdf
  ;; [1]
  ;; (1) 点Pの座標
  (setv x (apply symbols ["x"] {:positive True}))
  (setv y (symbols "y"))
  (print (ap-pipe [y (* 2 (** x 2))]
                  (apply Eq it {})
                  (.subs it y x)
                  (simplify it)
                  (solve it x)
                  (first it)))
  ;; (2) OQPRの周の長さが12の場合の点Pの座標
  (setv (, a b) (apply symbols ["a b"] {:positive True}))
  (print (ap-pipe [(+ a b (- 6))
                   (+ (* 2 (** a 2)) (- b))]
                  (solve it [a b])))
  ;; [2]
  ;; (3) a^2b - ab^2
  (setv (, a b) (symbols "a b"))
  (print (-> (- (* (** a 2) b) (* a (** b 2)))
             factor
             (.subs a (- (sqrt 5) (sqrt 3)))
             (.subs b (+ (sqrt 5) (sqrt 3)))
             (simplify)))

  ;; [3]
  ;; (4) 台形の面積
  (setv h (apply symbols ["h"] {:positive True}))
  (setv cos-dcb (Rational (- 9 7) 7))
  (setv sin-dcb (/ h 7))
  (setv height (ap-pipe [(+ (** cos-dcb 2) (** sin-dcb 2)) 1]
                        (apply Eq it {})
                        (solve it h)
                        (first it)))
  (print (/ (* (+ 7 9) height) 2)) ;; (上底 + 下底) * 高さ / 2

  ;; [4]
  ;; (5) 2点間の距離 (x^2を求める)
  (setv (, x2 cos34) (symbols "x2 cos34"))
  (setv x-pow2 (ap-pipe [x2 (+ (** 25 2) (** 17 2) (- (* 2 25 17 cos34)))]
                        (apply Eq it {})
                        (.subs it cos34 0.83)
                        (solve it x2)
                        (first it)
                        ))

  ;; (6) 何光年? (xを求める)
  (setv x (apply symbols ["x"] {:positive True}))
  (print (ap-pipe [(** x 2) x-pow2]
                  (apply Eq it {})
                  (solve it x)
                  (first it)))

  ;; [5]
  ;; (7) 確率
  (defn weather [today tomorrow]
    (defn weather-to-num [w]
      (cond
        [(= w "晴れ") 0]
        [(= w "曇り") 1]
        [(= w "雨") 2]))
    (setv (, today-num tomorrow-num)
          [(weather-to-num today) (weather-to-num tomorrow)])
    (setv code (.join "" (list (map str [today-num tomorrow-num]))))
    (cond [(= "00" code) (Rational 6 10)]
          [(= "01" code) (Rational 3 10)]
          [(= "02" code) (Rational 1 10)]
          [(= "10" code) (Rational 5 10)]
          [(= "11" code) (Rational 3 10)]
          [(= "12" code) (Rational 2 10)]
          [(= "20" code) (Rational 4 10)]
          [(= "21" code) (Rational 3 10)]
          [(= "22" code) (Rational 3 10)]))
  (print (+ (* (weather "晴れ" "晴れ") (weather "晴れ" "雨"))
            (* (weather "晴れ" "曇り") (weather "曇り" "雨"))
            (* (weather "晴れ" "雨") (weather "雨" "雨"))))


  ;; [6]
  ;; (8-A) 平均値
  (defn average [xs]
    (setv size (len xs))
    (/ (sum xs) size))
  (print (average [13 15 8 10 9 17]))
  ;; (8-B) 平均値
  (print (average [9 12 11 7 12 15]))
  ;; (9) 分散
  (defn variance2 [xs] ;; 分散の２乗
    (setv ave (average xs))
    (Rational (sum (map (fn [x] (** (- x ave) 2)) xs)) (len xs)))
  (print (variance2 [13 15 8 10 9 17]))
  (print (variance2 [9 12 11 7 12 15]))
  ;; [7]
  ;; (10) 置換
  (defn conv-a [s]
    (.replace s "○○○○" "○"))
  (defn conv-b [s]
    (.replace s "○△" "△○"))
  (defn conv-c [s]
    (.replace s "△△" "○"))
  (print (-> "△○△○○"
             conv-b
             conv-c
             conv-a))
  )

(defmain
  [&rest args]
  ;(solve-semi-2nd-1)
  (solve-semi-2nd-2)
)

