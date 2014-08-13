;(ns dots)

(def running true)
(def running false)

(def dimension 100)
(def num-dots 25)

(defrecord Cell [x y]) ; also :dot
(defrecord Dot [])

(defn create-world
  []
  (apply vector
         (map (fn [x]
                (apply vector
                       (map (fn [y]
                              (ref (Cell. x y)))
                            (range dimension))))
              (range dimension))))

(def world (create-world))

(defn place
  "looks up a cell in the world"
  [[x y]]
  (-> world
      (nth x)
      (nth y)))

(defn create-dot
  "in transaction: add a dot to a location in the world"
  [location]
  (let [cell-ref (place location)
        dot      (Dot.)]
    (alter cell-ref assoc :dot dot)
    (agent location)))

(defn location-has-dot?
  [x y]
  (let [location (place [x y])]
    (:dot @location)))

(defn setup-dots
  "places dots around the world"
  []
  (doall
    (for [n (range num-dots)]
      (dosync
        (create-dot [(rand-int dimension) (rand-int dimension)])))))

(def move-sleep-ms 150)

(defn rand-bool
  ""
  []
  (= 0 (rand-int 2)))

(defn move-to
  "in transaction: moves a dot from x y to next-x next-y"
  [cell-ref next-x next-y dot]
  (do
    (alter cell-ref dissoc :dot)
    (alter (place [next-x next-y]) assoc :dot dot)
    [next-x next-y]))

(defn move
  "in transaction: moves a dot around the world"
  [location]
  (let [cell-ref (place location)
        cell     @cell-ref
        dot      (:dot cell)
        x        (:x cell)
        y        (:y cell)
        move-x   (rand-bool)
        move-y   (not move-x)
        prev-x   (mod (- x 1) dimension)
        next-x   (mod (+ x 1) dimension)
        prev-y   (mod (- y 1) dimension)
        next-y   (mod (+ y 1) dimension)]
    (Thread/sleep move-sleep-ms)
    (dosync
      (when dot
        (when running
          (send-off *agent* #'move))
        (cond
          move-x
            (cond
              (not (location-has-dot? x next-y)) (move-to cell-ref x next-y dot)
              (not (location-has-dot? x prev-y)) (move-to cell-ref x prev-y dot))
          move-y
            (cond
              (not (location-has-dot? prev-x y)) (move-to cell-ref prev-x y dot)
              (not (location-has-dot? next-x y)) (move-to cell-ref next-x y dot)))))))

; draw it

(import
  '(java.awt Color Graphics Dimension)
  '(java.awt.image BufferedImage)
  '(javax.swing JPanel JFrame))

(def scale 5)
(def width  (* scale dimension))
(def height (* scale dimension))

(defn cell-color
  "pick color for a cell based on wether or not it has a dot"
  [cell]
  (if (location-has-dot? (:x cell) (:y cell))
    (Color/blue)
    (Color/yellow)))

(defn render-cell
  "renders a cell in a given color"
  [bg cell color]
  (let [x     (:x cell)
        y     (:y cell)
        pos-x (* x scale)
        pos-y (* y scale)
        end-x (+ scale pos-x)
        end-y (+ scale pos-y)]
    (doto bg
      (.setColor (cell-color cell))
      (.fillRect pos-x pos-y end-x end-y))))

(defn render-cells
  "renders cells"
  [bg world]
  (doseq [cell-ref (flatten world)]
    (let [cell @cell-ref]
      (render-cell bg cell (cell-color cell)))))

(defn render [g]
  (let [img (BufferedImage. width height (BufferedImage/TYPE_INT_ARGB))
        bg  (.createGraphics img)]
    (doto bg
      (.setColor (Color/white))
      (.fillRect 0 0 width height))
    (render-cells bg world)
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn jpanel
  []
  (proxy [JPanel] []
    (paint [g] (render g))))

(def panel
  (doto (jpanel)
    (.setPreferredSize (Dimension. width height))))

(def frame
  (doto (JFrame.)
    (.add panel)
    (.setDefaultCloseOperation (JFrame/DISPOSE_ON_CLOSE))
    .pack
    .show))

(comment
  ; demo

  (def animation-sleep-ms 20)
  (def animator (agent nil))
  (defn animation [x]
    (when running
      (Thread/sleep animation-sleep-ms)
      (.repaint panel)
      (send-off *agent* #'animation))
    nil)
  (send-off animator animation)

  (def dots (setup-dots))
  (doall (map #(send-off % move) dots)))

