mode == "e" {
  print substr($0, 2)
  mode = "x"
  next
}

{
  oldMode = mode
  mode = substr($0, 1, 1)
  line = substr($0, 2)
}

/^ echo/ {
  mode = "e"
}

(oldMode == " " || oldMode == "+") && mode != oldMode {
  print "```"
  print ""
}

(mode == " " || mode "+") && /^ *$/ {
  mode = oldMode
  next
}

mode == " " && mode != oldMode {
  print ""
  print "```"
}

mode == "+" && mode != oldMode {
  print ""
  print "```console"
}

mode == " " || mode == "+" {
  print line
}
