#/bin/sh 
scala -deprecation colorize.scala $1 > out.html && open out.html
