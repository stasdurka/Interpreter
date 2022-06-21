# 09 (przesłanianie i statyczne wiązanie)

main [string s = "na poczatku"] {
    println s;
    [string s] {
        s = "w pierwszym bloku";
        print "  ", s, "\n";
        [string s = "w drugim bloku"] {
            print "    ", s, "\n";
        }
        print "  po wyjsciu z drugiego bloku == ";
        println s;


    }
    print "po wyjsciu z pierwszego bloku == ";
    println s;
}
