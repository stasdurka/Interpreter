# 09 (przesłanianie i statyczne wiązanie)

main [string s = "na poczatku"] {
    println s;
    [string s] {
        s = "w pierwszym bloku";
        println "  ", s;
        [string s = "w drugim bloku"] {
            print "    ", s, "\n";
        }
        println "  po wyjsciu z drugiego bloku == ", s;


    }
    print "po wyjsciu z pierwszego bloku == ";
    println s;
}
