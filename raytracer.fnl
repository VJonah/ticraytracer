;; title: RayTracer!
;; author: jonah
;; script: fennel

;; screen width and height
(local height 136)
(local width 240)
;; a collection of palettes for each line
(local palettes [])

;; --------------------------------------------------------------------
;; helpers
(fn range [tbl]
  "Returns the range of a collection of values."
  (- (math.max (table.unpack tbl)) (math.min (table.unpack tbl))))

(fn mean [tbl]
  "Calculates the mean value of a table."
  (/ (accumulate [sum 0 _ x (ipairs tbl)] (+ sum x)) (length tbl)))

(fn median [tbl]
  "Calculates the median value of a table."
  (let [len (length tbl)
        half (math.ceil len 2)]
    (if (= (% len 2) 0)
        (/ (+ (. tbl half) (. tbl (- half 1))) 2)
        (. tbl half))))

(fn square [x] (* x x))

(fn variance [tbl]
  "Calculates the vairnace of table of values."
  (let [mu (mean tbl)]
    (/ (accumulate [sum 0 _ x (ipairs tbl)]
         (+ sum (square (- x mu)))) (length tbl))))

(fn round [x ?increment]
  "Rounds floats (from lume.lua)."
  (if increment (* (round (/ x increment)) increment))
  (or (and (>= x 0) (math.floor (+ x 0.5))) (math.ceil (- x 0.5))))

;; --------------------------------------------------------------------

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
  (let [new_x (accumulate [sum 0 _ v (ipairs [...])]
                (+ sum v.x))
        new_y (accumulate [sum 0 _ v (ipairs [...])]
                (+ sum v.y))
        new_z (accumulate [sum 0 _ v (ipairs [...])]
                (+ sum v.z))]
    (make-vector new_x new_y new_z)))

(fn vec- [v1 ...]
  "Subtract an arbitrary number of vectors element wise."
  (let [new_x (accumulate [sum v1.x _ v2 (ipairs [...])]
                (- sum v2.x))
        new_y (accumulate [sum v1.y _ v2 (ipairs [...])]
                (- sum v2.y))
        new_z (accumulate [sum v1.z _ v2 (ipairs [...])]
                (- sum v2.z))]
    (make-vector new_x new_y new_z)))

(fn vec* [...]
  "Multiply an arbitrary number of vectors element wise."
  (let [new_x (accumulate [prod 1 _ v (ipairs [...])]
                (* prod v.x))
        new_y (accumulate [prod 1 _ v (ipairs [...])]
                (* prod v.y))
        new_z (accumulate [prod 1 _ v (ipairs [...])]
                (* prod v.z))]
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

(local viewport_upper_left
       (vec- camera_center (make-vector 0 0 focal_length)
             (vec-div viewport_u 2) (vec-div viewport_v 2)))

(local pixel00_loc
       (vec+ viewport_upper_left (vec-mul (vec+ pixel_delta_u pixel_delta_v)
                                          0.5)))

;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; objects
(fn hit-sphere [center radius ray]
  (let [oc (vec- center ray.orig)
        a (vec-len-sq ray.dir)
        h (vec-dot ray.dir oc)
        c (- (vec-len-sq oc) (* radius radius))
        discriminant (- (* h h) (* a c))]
    (if (< discriminant 0)
        -1
        (/ (- h (math.sqrt discriminant)) a))))

;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; rays
(fn make-ray [orig dir]
  {: orig : dir :at (fn [self t] (vec+ self.orig (vec-mul self.dir t)))})

(fn ray-colour [r]
  (let [t (hit-sphere (make-point 0 0 -1) 0.5 r)]
    (if (> t 0)
        (let [N (unit-vector (vec- (r:at t) (make-vector 0 0 -1)))]
          (vec-mul (make-colour (+ N.x 1) (+ N.y 1) (+ N.z 1)) 0.5))
        (let [unit_direction (unit-vector r.dir)
              a (* 0.5 (+ unit_direction.y 1))]
          (vec+ (vec-mul (make-colour 1 1 1) (- 1 a))
                (vec-mul (make-colour 0.5 0.7 1) a))))))

(fn round [x ?increment]
  "Rounds floats (from lume.lua)."
  (if increment (* (round (/ x increment)) increment))
  (or (and (>= x 0) (math.floor (+ x 0.5))) (math.ceil (- x 0.5))))

;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; colour quantisation
(fn change-palette [colours]
  "Takes a sequential table of 16 [r g b] and sets it to memory."
  (let [palette 16320]
    (each [i [r g b] (ipairs colours)]
      (poke (+ palette (* (- i 1) 3)) r)
      (poke (+ palette (* (- i 1) 3) 1) g)
      (poke (+ palette (* (- i 1) 3) 2) b))))

(fn mean-pixel [pixels]
  "Returns the mean pixel."
  (if (not= (length pixels) 0)
      (let [mean_r (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 1))) 1)
            mean_g (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 2))) 1)
            mean_b (round (mean (icollect [_ rgb (ipairs pixels)] (. rgb 3))) 1)]
        [mean_r mean_g mean_b])
      [0 0 0]))

(fn sse [pixels]
  (let [[mu_r mu_g mu_b] (mean-pixel pixels)]
    (accumulate [sum 0 _ pxl (ipairs pixels)]
      (+ sum (square (- (. pxl 1) mu_r)) (square (- (. pxl 2) mu_g))
         (square (- (. pxl 3) mu_b))))))

(fn get-greatest-variance [pixels]
  "Returns the index of the colour channel with the greatest variance
   in a collection of pixels."
  (var max_var -1)
  (var max_ch -1)
  (for [ch_idx 1 3]
    (let [v (variance (icollect [_ rgb (ipairs pixels)] (. rgb ch_idx)))]
      (if (> v max_var)
          (do
            (set max_var v)
            (set max_ch ch_idx)))))
  max_ch)

(fn binary-insert [tbl item comp]
  (var low 1)
  (var high (length tbl))
  (if (or (= high 0) (comp (. tbl high) item))
      (table.insert tbl (+ high 1) item)
      (do
        (while (< low high)
          (let [mid (// (+ low high) 2)
                element (. tbl mid)]
            (if (comp item element)
                (set high (- mid 1))
                (if (comp element item)
                    (set low (+ mid 1))
                    (do
                      (set low mid)
                      (set high mid))))))
        (table.insert tbl low item))))

(fn binary-search [tbl key low high]
  (if (> low high)
      (- low 1)
      (let [mid (// (+ low high) 2)
            mid_el (. tbl mid)]
        (if (= mid_el key)
            (do
              (var return mid)
              (for [i mid high]
                (if (= (. tbl i) key)
                    (set return i)))
              return)
            (if (> key mid_el)
                (binary-search tbl key (+ mid 1) high)
                (binary-search tbl key low (- mid 1)))))))

(fn linear-search [tbl item]
  (var idx -1)
  (for [i 1 (length tbl)]
    (if (<= (. tbl i) item)
        (set idx i)))
  idx)

(fn median-cut [buckets n]
  (if (= (length buckets) n)
      buckets
      (let [bucket (. buckets 1)
            ch_idx (get-greatest-variance bucket)
            ch_mean (round (mean (icollect [_ rgb (ipairs bucket)]
                                   (. rgb ch_idx))))
            lower_half []
            upper_half []
            len (length bucket)]
        (table.sort bucket #(< (. $1 ch_idx) (. $2 ch_idx)))
        (local ch_mean_idx
               (binary-search (icollect [_ rgb (ipairs bucket)] (. rgb ch_idx))
                              ch_mean 1 (length bucket)))
        (table.move bucket 1 ch_mean_idx 1 lower_half)
        (table.move bucket (+ ch_mean_idx 1) len 1 upper_half)
        (tset lower_half :sse (sse lower_half))
        (tset upper_half :sse (sse upper_half))
        (table.remove buckets 1)
        (binary-insert buckets lower_half #(< (. $1 :sse) (. $2 :sse)))
        (binary-insert buckets upper_half #(< (. $1 :sse) (. $2 :sse)))
        (median-cut buckets n))))

;; --------------------------------------------------------------------

(fn render-scanline [row]
  "Calculates colour value of each pixel and sets them to frame, scan
   line by scanline."
  (let [scanline []]
    (for [i 0 (- width 1) 1]
      (let [pixel_center (vec+ pixel00_loc (vec-mul pixel_delta_u i)
                               (vec-mul pixel_delta_v row))
            ray_direction (vec- pixel_center camera_center)
            r (make-ray camera_center ray_direction)
            pixel_colour (ray-colour r)]
        (table.insert scanline (write-colour pixel_colour i))))
    (let [buckets (median-cut [scanline] 16)
          palette []]
      (each [pal_idx bucket (ipairs buckets)]
        (table.insert palette (mean-pixel bucket))
        (each [_ pxl (ipairs bucket)]
          (pix (. pxl 4) row (- pal_idx 1))))
      (table.insert palettes palette))
    (trace (.. "Scanline " row " done!"))))

;; clear at boot
(fn _G.BOOT [] (cls))

(var rendering? false)
(var row 0)

(fn _G.TIC []
  (when (and (not rendering?) (< row height))
    (set rendering? true)
    (render-scanline row)
    (set row (+ row 1))
    (set rendering? false)))

(fn _G.BDR [scanline]
  (if (and (> scanline 3) (< scanline 140))
      (let [uine_no (- scanline 3)
            palette (. palettes (- scanline 3))]
        (if palette
            (change-palette palette)
            (change-palette [[0 0 0]])))
      (change-palette [[0 0 0]])))

;; <PALETTE>
;; 000:101010202020303030404040505050606060707070808080909090a0a0a0b0b0b0c0c0c0d0d0d0e0e0e0f0f0f0ffffff
;; </PALETTE>
