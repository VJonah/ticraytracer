;; title: RayTracer!
;; author: jonah
;; script: fennel

;; screen width and height
(local height 136)
(local width 240)
;; a collection of palettes for each line
(local palettes [])



(fn change-palette [...]
  "takes a sequential table of 16 [r g b] and sets it"
  (local palette 0x3fc0)
  (each [i [r g b] (ipairs ...)]
      (poke (+ palette (* (- i 1) 3)) r)
      (poke (+ palette (* (- i 1) 3) 1) g)
      (poke (+ palette (* (- i 1) 3) 2) b)))

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
