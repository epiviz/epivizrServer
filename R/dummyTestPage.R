.dummyTestPage=function(req) {
  wsUrl = paste(sep='',
                '"',
                "ws://",
                ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                '"')
  
  list(
    status = 200L,
    headers = list(
      'Content-Type' = 'text/html'
    ),
    body = paste(
      sep = "\r\n",
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<title>EpivizServer Test Page</title>",
      '<style type="text/css">',
      'body { font-family: Helvetica; }',
      'pre { margin: 0 }',
      '</style>',
      "</head>",
      "<body>",
      '<h3>Test Page</h3>',
      "</body>",
      "</html>"
    )
  )
}
