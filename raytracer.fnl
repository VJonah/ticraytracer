;; title: RayTracer!
;; author: jonah
;; script: fennel

;; screen width and height
(local height 136)
(local width 240)
;; a collection of palettes for each line
(local palettes [])

(fn change-palette [colours]
  "Takes a sequential table of 16 [r g b] and sets it to memory."
  (let [palette 16320]
    (each [i [r g b] (ipairs colours)]
      (poke (+ palette (* (- i 1) 3)) r)
      (poke (+ palette (* (- i 1) 3) 1) g)
      (poke (+ palette (* (- i 1) 3) 2) b))))

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
(render)

(fn _G.TIC []
  "game loop that's called 60/s")

(fn _G.BDR [scanline]
  "called between rendering each scanline"
  (let [line_no (- scanline 3)
        palette (. palettes line_no)]
    (if palette
        (change-palette palette)
        (if (< line_no 1)
            (change-palette (. palettes 1))
            (change-palette (. palettes 136))))))

;; <PALETTE>
;; 000:101010202020303030404040505050606060707070808080909090a0a0a0b0b0b0c0c0c0d0d0d0e0e0e0f0f0f0ffffff
;; </PALETTE>
