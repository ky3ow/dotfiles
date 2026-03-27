;;extends

(string
  content: _ @injection.content
  (#lua-match? @injection.content "^<cmd>.*<cr>")
  (#offset! @injection.content 0 5 0 -4) ;; match_id start_col start_row end_col end_row
  (#set! injection.language "vim"))
