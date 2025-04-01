;;extends

(block_mapping_pair
  key: (flow_node) @_run
  (#any-of? @_run "run" "script" "before_script" "after_script" "bash")
  value: (flow_node
    (plain_scalar
      (string_scalar) @injection.content)
    (#set! injection.language "bash")))

(block_mapping_pair
  key: (flow_node) @_run
  (#any-of? @_run "run" "script" "before_script" "after_script" "bash")
  value: (block_node
    (block_scalar) @injection.content
    (#set! injection.language "bash")
    (#offset! @injection.content 0 1 0 0)))

; (block_mapping
;   (block_mapping_pair
; 	key: (flow_node) @_container
; 	value: (block_node
; 			 (block_mapping
; 			   (block_mapping_pair
; 				 key: (flow_node) @_key
; 				 value: (block_node
; 						  (block_scalar) @injection.content)))))
;   (#any-of? @_container "inputs")
;   (#any-of? @_key "inlineScript")
;   (#set! injection.language "bash")
;   (#offset! @injection.content 0 1 0 0))

(block_mapping
  (block_mapping_pair
	key: (flow_node) @_task_key
	value: (flow_node
			 (plain_scalar
			   (string_scalar) @_task_name))
	(#lua-match? @_task_name "AzureCLI@2")
	(#lua-match? @_task_key "task"))
  (block_mapping_pair
	key: (flow_node) @_container
	value: (block_node
			 (block_mapping
			   (block_mapping_pair
				 key: (flow_node) @_key
				 value: (block_node
						  (block_scalar) @injection.content))))
	(#any-of? @_container "inputs")
	(#any-of? @_key "inlineScript")
	(#set! injection.language "bash")
	(#offset! @injection.content 0 1 0 0)))
