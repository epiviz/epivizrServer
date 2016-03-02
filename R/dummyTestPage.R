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
      '<style type="text/css">',
      'body { font-family: Helvetica; }',
      'pre { margin: 0 }',
      '</style>',
      "<script>",
      sprintf("var ws = new WebSocket(%s);", wsUrl),
      "ws.onmessage = function(msg) {",
      '  var req = JSON.parse(msg.data)',
      '  msgDiv = document.createElement("pre");',
      '  msgDiv.innerHTML = req.data.msg.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
      '  document.getElementById("output").appendChild(msgDiv);',
      '  ws.send(JSON.stringify({type: "response", requestId: req.requestId, data: {msg: "that msg"}}));',
      "}",
      "function sendInput() {",
      "  var input = document.getElementById('input');",
      "  ws.send(JSON.stringify({type: 'request', requestId: 0, data: {action: 'getAllData', measurements: {}, chr: input.value, start: 0, end: 0}}));",
      "  input.value = '';",
      "}",
      "</script>",
      "</head>",
      "<body>",
      '<h3>Send Message</h3>',
      '<form action="" onsubmit="sendInput(); return false">',
      '<input type="text" id="input"/>',
      '<h3>Received</h3>',
      '<div id="output"/>',
      '</form>',
      "</body>",
      "</html>"
    )
  )
}
