;; title: RayTracer!
;; author: jonah
;; script: fennel

;; screen width and height
(local height 136)
(local width 240)
;; a collection of palettes for each line
(local palettes [])

;; --------------------------------------------------------------------
;; vectors
(fn make-vector [x y z]
  "Creates a vector object."
  {:x (or x 0)
   :y (or y 0)
   :z (or z 0)
   :+= (fn [self v2]
         (tset self :x (+ self.x v2.x))
         (tset self :y (+ self.y v2.y))
         (tset self :z (+ self.z v2.z)))
   :*= (fn [self t]
         (tset self :x (* self.x t))
         (tset self :y (* self.y t))
         (tset self :z (* self.z t)))
   :/= (fn [self t] (self:*= (/ 1 t)))})

(fn make-point [x y z] (make-vector x y z))

(fn vec-at [v i]
  "Makes vector coordinates 0-indexable."
  (case i
    0 v.x
    1 v.y
    2 v.z
    _ (trace "ERROR -- vector index out of range (vec-at)")))

(fn vec-neg [v] (make-vector (- v.x) (- v.y) (- v.z)))

(fn vec-len-sq [v] (+ (* v.x v.x) (* v.y v.y) (* v.z v.z)))

(fn vec-len [v] (math.sqrt (vec-len-sq v)))

(fn vec+ [...]
  "Add an arbitrary number of vectors element wise."
  (let [new_x (accumulate [sum 0 _ v (ipairs ...)] (+ sum v.x))
        new_y (accumulate [sum 0 _ v (ipairs ...)] (+ sum v.y))
        new_z (accumulate [sum 0 _ v (ipairs ...)] (+ sum v.z))]
    (make-vector new_x new_y new_z)))

(fn vec- [v1 ...]
  "Subtract an arbitrary number of vectors element wise."
  (let [new_x (accumulate [sum v1.x _ v2 (ipairs ...)] (- sum v2.x))
        new_y (accumulate [sum v1.y _ v2 (ipairs ...)] (- sum v2.y))
        new_z (accumulate [sum v1.z _ v2 (ipairs ...)] (- sum v2.z))]
    (make-vector new_x new_y new_z)))

(fn vec* [...]
  "Multiply an arbitrary number of vectors element wise."
  (let [new_x (accumulate [prod 1 _ v (ipairs ...)] (* prod v.x))
        new_y (accumulate [prod 1 _ v (ipairs ...)] (* prod v.y))
        new_z (accumulate [prod 1 _ v (ipairs ...)] (* prod v.z))]
    (make-vector new_x new_y new_z)))

(fn vec-mul [v t] (make-vector (* v.x t) (* v.y t) (* v.z t)))

(fn vec-div [v t] (vec-mul v (/ 1 t)))

(fn vec-dot [v1 v2]
  "Vector dot product."
  (+ (* v1.x v2.x) (* v1.y v2.y) (* v1.z v2.z)))

(fn vec-cross [v1 v2]
  "Vector cross product."
  (make-vector (- (* v1.y v2.z) (* v1.z v2.y)) (- (* v1.z v2.x) (* v1.x v2.z))
               (- (* v1.x v2.y) (* v1.y v2.x))))

(fn unit-vector [v] (vec-div v (vec-len v)))

(fn vec2str [v] (.. "(" v.x ", " v.y ", " v.z ")"))
;; --------------------------------------------------------------------


;; --------------------------------------------------------------------
;; colours
(fn make-colour [x y z] (make-vector x y z))
(fn write-colour [c col]
  (let [r (math.floor (* 255.999 c.x))
        g (math.floor (* 255.999 c.y))
        b (math.floor (* 255.999 c.z))]
    [r g b col]))
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; camera
(local focal_length 1.0)
(local viewport_height 2.0)
(local viewport_width (* viewport_height (/ width height)))
(local camera_center (make-point 0 0 0))
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; viewport
(local viewport_u (make-vector viewport_width 0 0))
(local viewport_v (make-vector 0 (- viewport_height) 0))

(local pixel_delta_u (vec-div viewport_u width))
(local pixel_delta_v (vec-div viewport_v height))

(local viewport_upper_left (vec- camera_center
                                 (make-vector 0 0 focal_length)
                                 (vec-div viewport_u 2)
                                 (vec-div viewport_v 2)))

(local pixel00_loc (vec+ viewport_upper_left
                         (vec-mul (vec+ pixel_delta_u pixel_delta_v) 0.5)))
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; rays
(fn make-ray [orig dir]
  {: orig
   : dir
   :at (fn [self t] (+ self.orig (* self.dir t)))})

(fn ray-colour [r] [0 0 0])
;; --------------------------------------------------------------------

(fn range [tbl]
  "Returns the range of a collection of values."
  (- (math.max (table.unpack tbl)) (math.min (table.unpack tbl))))

(fn mean [tbl]
  "Calculates the mean value of a table."
  (/ (accumulate [sum 0 _ n (ipairs tbl)] (+ sum n)) (length tbl)))

(fn round [x ?increment]
  "Rounds floats (from lume.lua)."
  (if increment (* (round (/ x increment)) increment))
  (or (and (>= x 0) (math.floor (+ x 0.5))) (math.ceil (- x 0.5))))

(fn mean-pixel [pixels]
  "Returns the mean pixel."
  (let [mean_r (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 1))) 1)
        mean_g (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 2))) 1)
        mean_b (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 3))) 1)]
    [mean_r mean_g mean_b]))

(fn get-greatest-range [pixels]
  "Returns the index of the colour channel with the greatest range
   in a collection of pixels."
  (var max_range -1)
  (var greatest_ch -1)
  (for [ch_idx 1 3]
    (let [rng (range (icollect [_ rgb (ipairs pixels)] (. rgb ch_idx)))]
      (if (> rng max_range)
          (do
            (set max_range rng)
            (set greatest_ch ch_idx)))))
  greatest_ch)

(fn median-cut [pixels n buckets]
  "Performs the median cut colour quantisation algorithm."
  (if (= n 1)
      (table.insert buckets pixels)
      (let [ch_idx (get-greatest-range pixels)
            lower_half []
            upper_half []
            len (length pixels)
            half (// len 2)]
        (table.sort pixels #(< (. $1 ch_idx) (. $2 ch_idx)))
        (table.move pixels 1 half 1 lower_half)
        (table.move pixels (+ half 1) len 1 upper_half)
        (median-cut lower_half (/ n 2) buckets)
        (median-cut upper_half (/ n 2) buckets))))

(fn render []
  "Calculates colour value of each pixel and sets them to frame, scan
   line by scanline."
  (var left height)
  (for [j 0 (- height 1)]
    (local scanline [])
    (for [i 0 (- width 1)]
      (let [r (math.floor (* 255.999 (/ i (- width 1))))
            g (math.floor (* 255.999 (/ j (- height 1))))
            b 0
            pixel [r g b i]]
        (table.insert scanline pixel)))
    (let [buckets []
          palette []]
      (median-cut scanline 16 buckets)
      (each [pal_idx bucket (ipairs buckets)]
        (table.insert palette (mean-pixel bucket))
        (each [_ pxl (ipairs bucket)]
          (pix (. pxl 4) j pal_idx)))
      (table.insert palettes palette))
    (set left (- left 1))
    (trace (.. "Scanlines left: " left))))

;; clear once
(cls)

;; call 'render'
;;(render)

(fn _G.TIC []
  "game loop that's called 60/s")

(fn _G.BDR [scanline]
  "called between rendering each scanline"
  (if (not= (length palettes) 0)
      (let [line_no (- scanline 3)
            palette (. palettes line_no)]
        (if palette
            (change-palette palette)
            (if (and palettes (< line_no 1))
                (change-palette (. palettes 1))
                (change-palette (. palettes 136)))))))

;; <PALETTE>
;; 000:101010202020303030404040505050606060707070808080909090a0a0a0b0b0b0c0c0c0d0d0d0e0e0e0f0f0f0ffffff
;; </PALETTE>
