(ns dots.dots
  "A simulation of colliding dots, highly inspired by rich's ants
  https://github.com/juliangamble/clojure-ants-simulation")

(def running
  "is the animation running"
  true)

(def dimension
  "world dimensions"
  50)

(def num-dots
  "number of dots to animate"
  50)

(defrecord Cell [x y]) ; and :dot
(defrecord Dot [dir-x dir-y color])

(defn- create-world
  "creates a world of refs to cells, each with x and y coordinates"
  []
  (let [dimensions (range dimension)]
    (apply vector
          (map (fn [x]
                 (apply vector
                        (map (fn [y]
                               (ref (Cell. x y)))
                             dimensions)))
               dimensions))))

(def world
  "the world, holding refs to cells, some cells with dots"
  (create-world))

(defn- place
  "looks up a cell-ref in the world"
  [[x y]]
  (-> world
      (nth x)
      (nth y)))

(defn- dot-in-location?
  "tests if a dot exists in the cell in a location"
  [x y]
  (let [cell-ref (place [x y])]
    (:dot @cell-ref)))

(defn- dot-can-move-to?
  "tests if dot can be moved to a location; it is within the world, it doesn't have a dot already"
  [x y]
  (and (< x dimension)
       (< y dimension)
       (>= x 0)
       (>= y 0)
       (not (dot-in-location? x y))))

(defn- rand-dir
  "picks a random direction, [-1 1]"
  []
  (- 1 (rand-int 3)))

(defn- rand-color
  "picks a random color"
  []
  (java.awt.Color. (rand-int 256) (rand-int 256) (rand-int 256)))

(defn- rand-dot
  "initializes a dot, with random x and y directions, and a random color"
  []
  (Dot. (rand-dir) (rand-dir) (rand-color)))

(defn- add-dot-to-location
  "in transaction: associates a dot with a location in the world. returns an agent on the location"
  [location]
  (let [cell-ref (place location)
        dot      (rand-dot)]
    (alter cell-ref assoc :dot dot)
    (agent location)))

(defn- create-dots
  "creates num-dots number of dots around the world. returns list of agents on their locations"
  []
  (doall
    (for [n (range num-dots)]
      (dosync
        (add-dot-to-location [(rand-int dimension) (rand-int dimension)])))))

(def move-sleep-ms
  "ms to sleep between each attempt move of a dot"
  100)

(defn- move-to
  "in transaction: attempts to move a dot to the next position, or changes the dots direction if it cant move where it wanted"
  [old-loc next-x next-y dot]
  (let [new-loc [next-x next-y]]
    (if (dot-can-move-to? next-x next-y)
      (do
        (alter (place old-loc) dissoc :dot)
        (alter (place new-loc) assoc :dot dot)
        new-loc)
      (do
        (alter (place old-loc) assoc :dot
               (assoc (rand-dot) :color (:color dot)))
        old-loc))))

(defn- move-loop
  "in transaction: agent move loop - updates a dot's position in the world. returns the new position that is keps in the agent."
  [location]
  (let [cell-ref (place location)
        cell     @cell-ref
        dot      (:dot cell)
        dir-x    (:dir-x dot)
        dir-y    (:dir-y dot)
        next-x   (+ (:x cell) dir-x)
        next-y   (+ (:y cell) dir-y)]
    (Thread/sleep move-sleep-ms)
    (dosync
      (when dot
        (when running
          (send-off *agent* #'move-loop))
        (move-to location next-x next-y dot)))))

(def dots
  "list of agents for locations for each dot"
  (create-dots))

(defn start-move-loop
  "kicks of a move loop agent per dot"
  []
  (doall (map #(send-off % move-loop) dots)))

(start-move-loop)

; ui

(import
  '(java.awt Color Graphics Dimension)
  '(java.awt.image BufferedImage)
  '(javax.swing JPanel JFrame))

(def scale
  "pixels per cell"
  7)

(def size
  "size of cell in pixels"
  (* scale dimension))

(defn- cell-color
  "picks color for a cell, based on wether or not it has a dot"
  [cell]
  (if (dot-in-location? (:x cell) (:y cell))
    (do
      (:color (:dot cell)))
    (Color/white)))

(defn- render-cell
  "renders a cell in a given color"
  [bg cell color]
  (let [pos-x (* (:x cell) scale)
        pos-y (* (:y cell) scale)
        end-x (+ scale pos-x)
        end-y (+ scale pos-y)]
    (doto bg
      (.setColor (cell-color cell))
      (.fillRect pos-x pos-y end-x end-y))))

(defn- render-cells
  "renders each of the world's cells"
  [bg world]
  (doseq [each-cell-ref (flatten world)]
    (let [cell @each-cell-ref]
      (render-cell bg cell (cell-color cell)))))

(defn- render [g]
  "renders all cells into a buffered image that is drawn to the panel on each tick"
  (let [img (BufferedImage. size size (BufferedImage/TYPE_INT_ARGB))
        bg  (.createGraphics img)]
    (doto bg
      (.setColor (Color/white))
      (.fillRect 0 0 size size))
    (render-cells bg world)
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel
  "subclasses JPanel to call render when paint is called"
  (doto (proxy [JPanel] []
          (paint [g] (render g)))
        (.setPreferredSize (Dimension. size size))))

(def frame
  (doto (JFrame.)
    (.add panel)
    (.setDefaultCloseOperation (JFrame/DISPOSE_ON_CLOSE))
    .pack
    .show))

(def animator
  "agent for the animation loop"
  (agent nil))

(def animation-sleep-ms
  "sleep time between each iteration of the animation loop"
  100)

(defn- animation [x]
  "animation loop, repaints the jpanel"
  (when running
    (Thread/sleep animation-sleep-ms)
    (.repaint panel)
    (send-off *agent* #'animation))
  nil)

(defn start-animation-loop
  "kicks of the animation loop"
  []
  (send-off animator animation))

(start-animation-loop)
