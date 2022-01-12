BEGIN {
  OFS = "\t"
  mode = "v"
}

/^ echo ["']# / && mode == "v" {
  mode = ""
}

mode == "v" {
  next
}

mode == "e" {
  if ((oldMode == " " || oldMode == "+")  && closed != 1) {
    print "```"
    print ""
    closed = 1
  }
  print substr($0, 2)
  mode = "x"
  next
}

{
  oldMode = mode
  mode = substr($0, 1, 1)
}

/^ echo/ {
  mode = "e"
  next
}

oldMode == "x" && /^ $/ {
  print ""
  mode = "x"
  next
}

oldMode == "x" && mode == " " {
  print "```"
  closed = 0
}

oldMode == " " && mode == "+" {
  print "```"
  print ""
  print "```console"
}

oldMode == "+" && mode == " " && closed != 1 {
  print "```"
  print ""
  if (!/^ *$/) {
    print "```"
    closed = 0
  } else
    closed = 1
}

mode == " " && !/^  *$/ {
  print substr($0, 2)
}

mode == "+" && !/^+ *$/ {
  print substr($0, 2)
}
