(ns rapp
  (:require [instaparse.core :as insta]
          ;  [clojure.walk :as w]
            [clojurewerkz.titanium.graph :as tg]
            [clojurewerkz.titanium.edges :as te]
            [clojurewerkz.titanium.vertices :as tv]
            [clojurewerkz.titanium.query :as tq]
            [ogre.core :as q]
            [ogre.tinkergraph :as g]
            )
  )
;(def start-parser
                                        ;  (def as-and-bs
;    (insta/parser (slurp "myparser.bnf")
;     ))
                                        ;  )
;(((insta/parser (slurp "myparser.bnf")) "code.beeScript"))
;((insta/parser (slurp "myparser.bnf")) (slurp "code.beeScript"))
 ;(dewfn analyze [node n a] (println node))
                                        ;(println
((defn start-parser []
   (def graph (tg/open {"storage.backend" "inmemory"}))
   (tg/transact!
    (do

    (defn counters []
    (let [count (ref 0)]
      (list #(dosync (alter count inc))
            #(dosync (alter count dec))
            #(dosync (deref count)))))
    (def vertexCounter (counters))
    (def cid (nth vertexCounter 2))
    ;(defmacro spy/n [& body] ~@body)
    (def cidd (nth vertexCounter 1))
    (def cidi (nth vertexCounter 0))

    (def nfa (atom (list )))
    (def startg (atom (tv/create! { :cid (cidi) :character "start"})))
    (reset! nfa (into @nfa (list @startg)))
    (def parser (insta/parser (slurp "myparser.bnf")))
    )
    (let [lchar (atom 1)
          cchar (atom 1)
          llchar (atom 1)
                                        ;println (fn [])
          vertexChar(fn [r]
                      ((tv/to-map r) :character)
                      )
          connect(fn [left right]
            (println "--------------connecting start--------")
            (doseq [l left
                    r right]

              (println (str "connecting " ((tv/to-map l) :character) " to " ((tv/to-map r) :character) " with " ((tv/to-map r) :character)))
              (te/connect! l :meaningful  r {:tchar (vertexChar r)})
              (println (str "connecting " l " to " r )))
              (println "///-----------connecting start--------")
            )
          isOptional(fn [key]
                      (contains? key :opt)
                      )
         mergee (fn [left right]
                  ;; todo connect last with first allways
                    (connect (left :last) (right :first))

                    (if (isOptional left)
                      (do
                       ;;(merge first left and right as new first)
                        (def new (assoc-in left [:first] (into (left :first) (right :first))))
                      (if (isOptional right)
                        ;;merge last left and right as new last() ;;mark new left as optc
                        ;;yes
                        (do
                        (def new (assoc-in new [:last] (into (left :last) (right :last))))
                        (def new (assoc-in new [:opt] true)))
                        ;;no
                        (do
                          (def new (dissoc new :opt))
                          (def new (assoc-in new [:last] (right :last)))
                          )
                        ;; newLast = last right() ;;        newFirst= merge first left right
                        )
                      )
                      (do
                        (def new (assoc-in left [:first] (left :first)))
                        (def new (dissoc new :opt))
                        (if (isOptional right)
                        ;;if right opt merge last left and last right as new last()
                          (def new (assoc-in new [:last] (into (left :last) (right :last))))
                        ;;new first = left first ;; new last = right last()
                          (def new (assoc-in new [:last] (right :last)))
                        ))
                      )
                    new
                  )
          number (fn [& args])
          char (fn [& args]
                 (println "------char-----------")
                 (doseq [item args]
                   (println (str "creating new character: " item))
                   (reset! cchar  (tv/create! { :cid (cidi) :character item}))
                   ;(reset! lchar  (peek @nfa))
                   ;(println (str "last character is" (tv/to-map @lchar)))
                   ;(te/connect! @lchar :meaningful  @cchar {:tchar item}))
                   ;(println (str "connecting old one with the new one: "
                   ;              (q/query @lchar
                   ;               q/--E>
                   ;               q/map
                   ;q/into-vec!))
          )
                   ;(reset! nfa (conj @nfa  @cchar))
                   ;(println "char added nfa :")
                   ;(println @nfa)
                   (println "----//char-------------")
                   @cchar;(first args)
            )
          lparen (fn [& args]
                   (println "lparen"))
          rparen (fn [& args]
                   (println "rparen"))
          plus (fn [& args]
                 (println "plus"))
          orm (fn [& args]
                (println "orm"))
          star (fn [& args]
            (doseq [item args]
              ;(>debug-repl)
              (reset! cchar  (tv/create! { :cid (cidi) :character "epsilon"}))
              (reset! lchar (peek @nfa))
              (reset! llchar (nth @nfa 1))
              (te/connect! @lchar :meaningful  @cchar {:tchar "epsilon"})
              (te/connect! @llchar :meaningful  @cchar {:tchar "epsilon"})
              (te/connect! @lchar :meaningful  @lchar {:tchar ((tv/to-map @lchar) :character)})
              (reset! nfa (conj @nfa  @cchar))
              (println item))
              (first args))
          questionmark (fn [& args]
           (doseq [item args]
                ;(>debug-repl)
            (reset! cchar  (tv/create! { :cid (cidi) :character "epsilon"}))
            (reset! lchar (peek @nfa))
            (reset! llchar (nth @nfa 1))
            (te/connect! @lchar :meaningful  @cchar {:tchar "epsilon"})
            (te/connect! @llchar :meaningful  @cchar {:tchar "epsilon"})
            (reset! nfa (conj @nfa @cchar))
            (println item))
           (first args))
          regexpTrain (fn [& args]
                ;        ( "regexp train")
                        )
          orregexp (fn [left op right]
                                        ;merger
                ;     (>debug-repl)
                       ["merger" left op right   ]
                      { :first (into ((find left :first) 1)
                                            ((find right :first) 1))
                             :last (into ((find left :last) 1)
                                           ((find right :last) 1))
                          }
                      ; ["merger"  ( left :first) ( right :first)]
                      )
          regexp (fn [& args]
                   "regexp"
                   (first args)
             )
          subRegexp (fn [paren & reg]
            ;if opt merge and pass if not just pass
             ["subregex" paren reg]
             (if (empty? reg)
                     paren ;just pass
                     (do
                       ;TODO connect the two things together
                      {:first [((find paren :first) 1)]
                      :last ((find (first reg) :last) 1)
                      })
                      ;merge and pass
                  ))
          baseRegexp (fn [& args]
                       (if (< 1 (count args))
                         (do
                           (println "need to deal with multiple ch")
                           )
                         (do
                           {
                            :first [(first args)]
                            :last [(last args)]
                            }
                           )
                         )

;              ["base regex first" (first args)]
;              ["base regex last" (last args)]
             )
          ;orregexp (fn [first orr second & args]
          ;    ["first" first "orr" orr "second" second]
          ;   )
          regexpStar (fn [ args]
                       args
                      ;(assoc args :qmark 1)
             )
          regexpStarEnd (fn [first star]
                          ["first" first "*" star ]
                          first
             )
          regexpStarCont (fn [left cont]
                           ["first" left  "cont" cont]
                           { :first (into ((find left :first) 1)
                                            ((find cont :first) 1))
                             :last (into ((find left :last) 1)
                                           ((find cont :last) 1))
                          }
             )
          regexpQMark (fn [args]
                        args
                  ; ["first" first "*" star "cont" cont]
                                        ;(assoc first :qmark 1)

             )
          regexpQMarkEnd (fn [first qmark]
                           ["first" first "*" qmark]
                           (println "---qmark----------")
                           (println "")
                           (println "---//qmark----------")
                           (assoc first :opt 1)
                       ;    (assoc first :qmarkEnd 1)
             )
          regexpQMarkCont (fn [left cont]
                                        ;["first" first "*" qmark "cont" cont]
                            { :first (into ((find left :first) 1)
                                            ((find cont :first) 1))
                             :last (into ((find left :last) 1)
                                           ((find cont :last) 1))
                          }
             )
          regexpPlus (fn [& args]
             args
             )
          regexpTrain (fn [& args]
             args
             )
          parenRegexp  (fn [args]
             args
             )
          wholeRegexp (fn [regxp]
                        (println "whole regexp")
                        (def st (tv/create! { :cid (cidi) :character "start"}))
                       (connect [st] (regxp :first))
                       (println (str [st] (regxp :first)))

                       )
          wholeRegexp1(fn [& args]
            (def left (first args))
            (if (->> (count args) (< 1 ))
              (do (println "multiple regexps 1")

                (doseq [right (next args)]
                  (def left (mergee left right))

                )
                ) ; check for optional ones
             (println "there is no multiple regexps under one this time")
             )
             left
            )

          wholeRegexp2(fn [& args]
            (def left (first args))
            (if (->> (count args) (< 1 ))
              (do (println "multiple regexps 2")

                (doseq [right (next args)]
                  (def left (mergee left right))

                )
                ) ; check for optional ones
             (println "there is no multiple regexps under one this time")
             )
             left
            )
          ]
      (def AST (parser (slurp "code.beeScript")))
      (def args {
             :char char
             ;questionmark questionmark
             ;:plus plus
             ;:star star
             ;:orm orm
             ;:lparen lparen
             ;:orregexps orregexps
             ;:regexpTrain regexpTrain
                                        ;:rparen rparen
             :wholeRegexp2 wholeRegexp2
             :wholeRegexp1 wholeRegexp1
             :wholeRegexp wholeRegexp
             :regexp regexp
             :subRegexp subRegexp
             :baseRegexp baseRegexp
             :orregexp orregexp
             :regexpStar regexpStar
             :regexpStarEnd regexpStarEnd
             :regexpStarCont regexpStarCont
             :regexpQMark regexpQMark
             :regexpQMarkEnd regexpQMarkEnd
             :regexpQMarkCont regexpQMarkCont
             :regexpPlus regexpPlus
             :regexpTrain regexpTrain
             :parenRegexp parenRegexp
             })
         (clojure.pprint/pprint
          (insta/transform             ;{}
                                        args
            AST))
         (let [start (atom 0)
               markTerminals (fn []
                (println (str "latest vertex: " (tv/to-map (tv/merge! (first  (tv/find-by-kv :cid (cid))) {:terminal true}))))
                               )
               transformOptional (fn [vert]
                 (doseq [epsilonv vert]
                    (doseq [e (q/query epsilonv
                              q/--E>
                              q/map
                              q/into-vec!)
                           v (q/query epsilonv
                              q/<E--
                              q/out-vertex
                              q/map
                              q/into-vec!)
                           ]
                     (println (str "[edge vertex] :" [e v]))
                     ;remmember to deal with terminals
                     )))
               printGraph (fn []
                 (doseq [v (tv/get-all-vertices)
                         e (q/query v
                            q/--E>
                            q/into-vec!)
                         ]
                   (println (str "edge:: "
                                 (q/query e
                                  q/map
                                  q/into-vec!)
                                 " vertices:: "
                                 (q/query e
                                  q/both-vertices
                                  q/map
                                  q/into-vec!
                                  )))))
               transChar(fn [e]
                               ((te/to-map e) :tchar)
                               )
               joinOnVert (fn [v]
                            (def excl [])
                            (doseq [ e1      (q/query v
                                                      q/--E>

                                             q/into-vec!)
                                    e2       (q/query v
                                                      q/--E>
                                                      (q/except (conj excl e1))
                                             (q/into-vec!))
                                    ]
                              (def excl (conj excl e1))
                              (when (= (transChar e1) (transChar e2))
                               ; (println (str e1;(transChar e1)
                               ;               e2;(transChar e2)
                               ;               ))
                                        ;grab edges out of e1 e2
                                (def outSet #{})
                                (doseq [vo (q/query (list e1 e2)
                                                    q/in-vertex
                                                    q/into-vec!)
                                        outer    (conj outSet (q/query vo
                                                          q/-->
                                                          q/map
                                                    q/into-set!))
                                        ]
                                        (println (str "outer " outer)))
                                ;v1 v2 remove
                                (println "remove")
                                (printGraph)
                                
                                  (println  (q/query (list e1 e2)
                                                    q/in-vertex
                                                    q/map
                                                    q/into-vec!))
                 
                                  (println e2)
                                  (println (transChar e2))
                                  (println  (q/query (list e1 e2)
                                                     q/in-vertex
                                                     q/into-vec!
                                                     first
                                                     tv/remove! 
                                                    )
                  )
                                  (println te/removed? e2)
                  (printGraph)
                                  ;remove edges
                                  (println (get (first (q/query (list e1 e2)
                                                    q/in-vertex
                                                    q/--E>
                                                    q/in-vertex
                                                    q/map
                                                    q/into-vec!))
                                                "character"
))
                                   ;remove e1 e2
                                   ;create new
                                 ; (def nw (tv/create! { :cid (cidi) :character "epsilon"}) )
                                  ;(connect [v] [nw])
                                  ;connect new with outer
                                  ;(connect [nw] (q/query (into [] outSet)
                                ;                    q/in-vertex
                                 ;                   q/into-vec!))
                                        ;remove outSet

                                        ;connect v to it
                                        ;for each in set connect all to it
                                        ;for each grab outer and map
                                        ;connect v to each outer with ap
                                        ;merge
;
;  8   -  16\
;x 12 -/   - -  20 
;   
;
                               )))
               transformJoinState (fn []
                (println "join States")
                (doseq [v (tv/get-all-vertices)]

                    (when-not (apply distinct? (into (q/query v
                                             q/-->
                                             (q/property :character)
                                             q/into-vec!) [1]))
                      (println (str "join can be performed" ))
                      (joinOnVert v)
                      )

                  ;deal with terminals
                ))
               ]
           ;(reset! start (q/query   (tv/find-by-kv :character "start")
            ;                       (q/first-of!)))
             (println (str "start : "@start))
           ;(markTerminals)
           ;(transformOptional (tv/find-by-kv :character "epsilon"))
           ;|star|questionmark
           (transformJoinState)
           )))))
