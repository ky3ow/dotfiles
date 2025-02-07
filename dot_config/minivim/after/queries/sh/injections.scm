;;extends

((heredoc_redirect
	(heredoc_start) @heredoc_start
	(heredoc_body) @injection.content
	(#lua-match? @heredoc_start "YAML")
	(#set! injection.language "yaml")))
