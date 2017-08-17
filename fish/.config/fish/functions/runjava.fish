function runjava --description "Compile and run single-file java program"
  javac -cp "guava-17.0.jar" "$argv[1].java"; and java -ea -cp "guava-17.0.jar:." "$argv[1]"
end
