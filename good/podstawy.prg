# test podstaw (do 15 pkt)
int fibo(int n) [int x; int y] {
    if (n == 0) {
        return 1;
    } 
    if (n == 1) {
        return 1;
    } else {
        return fibo(n-1) + fibo(n-2);
    }
}

main [int i; bool stop = false; string str = "test"] {
    print str;
    i = 0;
    while (i < 10 && (!stop)) {
        print "fibo(", "zzzz", true;
        print i;
        print ") = ";
        print fibo(i);
        i = i+1;
    }
    print "koniec pętli; i = ";
    print i;
}