;;;; charsets.lisp -- WordPerfect character sets, as Unicode code points.
;;;;
;;;; WordPerfect text stores characters outside ASCII as (set, index) pairs.
;;;; These tables cover the sets clip art uses; they are libwpd's data
;;;; (libwpd_internal.cpp and WP6Parser.cpp). A zero entry means unmapped.

(in-package #:wordperfect-graphics)

(defparameter +wp-multinational+
  #(
    #x0300 #x00B7 #x0303 #x0302 #x0335 #x0338 #x0301 #x0308 #x0304 #x0313 #x0315 #x02BC
    #x0326 #x0315 #x00B0 #x0307 #x030B #x0327 #x0328 #x030C #x0337 #x0305 #x0306 #x00DF
    #x0138 #x006A #x00C1 #x00E1 #x00C2 #x00E2 #x00C4 #x00E4 #x00C0 #x00E0 #x00C5 #x00E5
    #x00C6 #x00E6 #x00C7 #x00E7 #x00C9 #x00E9 #x00CA #x00EA #x00CB #x00EB #x00C8 #x00E8
    #x00CD #x00ED #x00CE #x00EE #x00CF #x00EF #x00CC #x00EC #x00D1 #x00F1 #x00D3 #x00F3
    #x00D4 #x00F4 #x00D6 #x00F6 #x00D2 #x00F2 #x00DA #x00FA #x00DB #x00FB #x00DC #x00FC
    #x00D9 #x00F9 #x0178 #x00FF #x00C3 #x00E3 #x0110 #x0111 #x00D8 #x00F8 #x00D5 #x00F5
    #x00DD #x00FD #x00D0 #x00F0 #x00DE #x00FE #x0102 #x0103 #x0100 #x0101 #x0104 #x0105
    #x0106 #x0107 #x010C #x010D #x0108 #x0109 #x010A #x010B #x010E #x010F #x011A #x011B
    #x0116 #x0117 #x0112 #x0113 #x0118 #x0119 #x01F4 #x01F5 #x011E #x011F #x01E6 #x01E7
    #x0122 #x0123 #x011C #x011D #x0120 #x0121 #x0124 #x0125 #x0126 #x0127 #x0130 #x0069
    #x012A #x012B #x012E #x012F #x0128 #x0129 #x0132 #x0133 #x0134 #x0135 #x0136 #x0137
    #x0139 #x013A #x013D #x013E #x013B #x013C #x013F #x0140 #x0141 #x0142 #x0143 #x0144
    #x0000 #x0149 #x0147 #x0148 #x0145 #x0146 #x0150 #x0151 #x014C #x014D #x0152 #x0153
    #x0154 #x0155 #x0158 #x0159 #x0156 #x0157 #x015A #x015B #x0160 #x0161 #x015E #x015F
    #x015C #x015D #x0164 #x0165 #x0162 #x0163 #x0166 #x0167 #x016C #x016D #x0170 #x0171
    #x016A #x016B #x0172 #x0173 #x016E #x016F #x0168 #x0169 #x0174 #x0175 #x0176 #x0177
    #x0179 #x017A #x017D #x017E #x017B #x017C #x014A #x014B #x0000 #x0000 #x0000 #x0000
    #x0000 #x0000 #x0000 #x0000 #x0000 #x0000 #x0000 #x0000 #x0000 #x0000 #x1EF2 #x1EF3
    #x010E #x010F #x01A0 #x01A1 #x01AF #x01B0 #x0114 #x0115 #x012C #x012D #x0049 #x0131
    #x014E #x014F))

(defparameter +wp-typographic+
  #(
    #x25CF #x25CB #x25A0 #x2022 #x002A #x00B6 #x00A7 #x00A1 #x00BF #x00AB #x00BB #x00A3
    #x00A5 #x20A7 #x0192 #x00AA #x00BA #x00BD #x00BC #x00A2 #x00B2 #x207F #x00AE #x00A9
    #x00A4 #x00BE #x00B3 #x201B #x2019 #x2018 #x201F #x201D #x201C #x2013 #x2014 #x2039
    #x203A #x25CB #x25A1 #x2020 #x2021 #x2122 #x2120 #x211E #x25CF #x25E6 #x25A0 #x25AA
    #x25A1 #x25AB #x2012 #xFB00 #xFB03 #xFB04 #xFB01 #xFB02 #x2026 #x0024 #x20A3 #x20A2
    #x20A0 #x20A4 #x201A #x201E #x2153 #x2154 #x215B #x215C #x215D #x215E #x24C2 #x24C5
    #x20AC #x2105 #x2106 #x2030 #x2116 #x2014 #x00B9 #x2409 #x240C #x240D #x240A #x2424
    #x240B #x267C #x20A9 #x20A6 #x20A8 #x0020 #x0020 #x0020 #x0020 #x0020 #x0020 #x0020
    #x0020 #x0020 #x0020 #x0020 #x1D11E #x1D122))

(defparameter +wp-box-drawing+
  #(
    #x2591 #x2592 #x2593 #x2588 #x258C #x2580 #x2590 #x2584 #x2500 #x2502 #x250C #x2510
    #x2518 #x2514 #x251C #x252C #x2524 #x2534 #x253C #x2550 #x2551 #x2554 #x2557 #x255D
    #x255A #x2560 #x2566 #x2563 #x2569 #x256C #x2552 #x2555 #x255B #x2558 #x2553 #x2556
    #x255C #x2559 #x255E #x2565 #x2561 #x2568 #x255F #x2564 #x2562 #x2567 #x256B #x256A
    #x2574 #x2575 #x2576 #x2577 #x2578 #x2579 #x257A #x257B #x257C #x257E #x257D #x257F
    #x251F #x2522 #x251E #x2521 #x252E #x2532 #x252D #x2531 #x2527 #x2526 #x252A #x2529
    #x2536 #x253A #x2535 #x2539 #x2541 #x2546 #x253E #x2540 #x2544 #x254A #x253D #x2545
    #x2548 #x2543 #x2549 #x2547))

(defparameter +wp6-iconic+
  #(
    #x2661 #x2662 #x2667 #x2664 #x2642 #x2640 #x263C #x263A #x263B #x266A #x266C #x25AC
    #x2302 #x203C #x221A #x21A8 #x2310 #x2319 #x25D8 #x25D9 #x21B5 #x2104 #x261C #x23B5
    #x2610 #x2612 #x2639 #x266F #x266D #x266E #x260E #x231A #x231B #x2701 #x2702 #x2703
    #x2704 #x260E #x2706 #x2707 #x2708 #x2709 #x261B #x261E #x270C #x270D #x270E #x270F
    #x2710 #x2711 #x2712 #x2713 #x2714 #x2715 #x2716 #x2717 #x2718 #x2719 #x271A #x271B
    #x271C #x271D #x271E #x271F #x2720 #x2721 #x2722 #x2723 #x2724 #x2725 #x2726 #x2727
    #x2605 #x2606 #x272A #x272B #x272C #x272D #x272E #x272F #x2730 #x2731 #x2732 #x2733
    #x2734 #x2735 #x2736 #x2737 #x2738 #x2739 #x273A #x273B #x273C #x273D #x273E #x273F
    #x2740 #x2741 #x2742 #x2743 #x2744 #x2745 #x2746 #x2747 #x2748 #x2749 #x274A #x274B
    #x25CF #x274D #x25A0 #x274F #x2750 #x2751 #x2752 #x25B2 #x25BC #x25C6 #x2756 #x25D7
    #x2758 #x2759 #x275A #x275B #x275C #x275D #x275E #x2036 #x2033 #x0020 #x0020 #x0020
    #x0020 #x2329 #x232A #x005B #x005D #x0020 #x0020 #x0020 #x0020 #x0020 #x0020 #x0020
    #x2190 #x0020 #x0020 #x0020 #x0020 #x0020 #x21E8 #x21E6 #x2794 #x0020 #x0020 #x0020
    #x0020 #x0020 #x25D6 #x0020 #x0020 #x2761 #x2762 #x2763 #x2764 #x2765 #x2766 #x2767
    #x2663 #x2666 #x2665 #x2660 #x2780 #x2781 #x2782 #x2783 #x2784 #x2785 #x2786 #x2787
    #x2788 #x2789 #x2776 #x2777 #x2778 #x2779 #x277A #x277B #x277C #x277D #x277E #x277F
    #x2780 #x2781 #x2782 #x2783 #x2784 #x2785 #x2786 #x2787 #x2788 #x2789 #x278A #x278B
    #x278C #x278D #x278E #x278F #x2790 #x2791 #x2792 #x2793 #x2794 #x2192 #x2194 #x2195
    #x2798 #x2799 #x279A #x279B #x279C #x279D #x279E #x279F #x27A0 #x27A1 #x27A2 #x27A3
    #x27A4 #x27A5 #x27A6 #x27A7 #x27A8 #x27A9 #x27AA #x27AB #x27AC #x27AD #x27AE #x27AF
    #x0020 #x27B1 #x27B2 #x27B3 #x27B4 #x27B5 #x27B6 #x27B7 #x27B8 #x27B9 #x27BA #x27BB
    #x27BC #x27BD #x27BE))

(defparameter +wp5-iconic+
  #(
    #x2665 #x2666 #x2663 #x2660 #x2642 #x2640 #x263C #x263A #x263B #x266A #x266C #x25AC
    #x2302 #x203C #x221A #x21A8 #x2310 #x2319 #x25D8 #x25D9 #x21B5 #x261E #x261C #x2713
    #x2610 #x2612 #x2639 #x266F #x266D #x266E #x260E #x231A #x231B #x2104 #x23B5))

(defparameter +wp5-international-2+
  #(
    #x0323 #x0324 #x02DA #x0325 #x02BC #x032D #x2017 #x005F #x0138 #x032E #x033E #x2018
    #x0020 #x02BD #x02DB #x0327 #x0321 #x0322 #x030D #x2019 #x0329 #x0020 #x0621 #x02BE
    #x0306 #x0310 #x2032 #x2034))

;; WP6 text bytes #x01-#x20 are these characters.
(defparameter +wp6-low-characters+
  #(
    #x00E5 #x00C5 #x00E6 #x00C6 #x00E4 #x00C4 #x00E1 #x00E0 #x00E2 #x00E3 #x00C3 #x00E7
    #x00C7 #x00EB #x00E9 #x00C9 #x00E8 #x00EA #x00ED #x00F1 #x00D1 #x00F8 #x00D8 #x00F5
    #x00D5 #x00F6 #x00D6 #x00FC #x00DC #x00FA #x00F9 #x00DF))

(defun wp-character (set index &key (version 6))
  "The character for INDEX in WordPerfect character SET, or #\\? if this
library has no mapping for it. VERSION is 5 or 6; the two differ in a few sets."
  (let* ((table (case set
                  (1 +wp-multinational+)
                  (2 (and (= version 5) +wp5-international-2+))
                  (3 +wp-box-drawing+)
                  (4 +wp-typographic+)
                  (5 (if (= version 5) +wp5-iconic+ +wp6-iconic+))))
         (code (cond ((zerop set) (and (<= #x20 index #x7e) index))
                     ((and table (< index (length table))) (aref table index)))))
    (if (and code (plusp code) (< code char-code-limit))
        (code-char code)
        #\?)))
