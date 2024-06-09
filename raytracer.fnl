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

(fn render []
  "renders pixel by pixel to frame"
  (for [i 0 (- height 1)]
    (local scanline [])
    (for [j 0 (- width 1)]
      (let [r (math.floor (* 255.999 ) (/ i (- width 1)))
            g (math.floor (* 255.999 ) (/ j (- height 1)))
            b 0]
        (table.insert scanline [r g b]))
      )
    (table.insert palettes (get-palette scanline))))

(cls) ;; clear once
(fn _G.TIC []
  "game loop that's called 60/s")

(fn _G.BDR [scanline]
  "called between rendering each scanline"
  (let [line_no (- scanline 3)
        palette (. palettes line_no)]
    (if palette
        (change-palette (unpack palette)))))

;; <PALETTE>
;; 000:101010202020303030404040505050606060707070808080909090a0a0a0b0b0b0c0c0c0d0d0d0e0e0e0f0f0f0ffffff
;; </PALETTE>
