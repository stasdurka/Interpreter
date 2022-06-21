int f(bool &a, int arr[]) {
    arr[0] = 123;
    a = false;
    return 0;
}

main [
      bool a;
      int arr[4];
      int brr[3];
      string s = "aaaa"
     ] 
 {
    a = true;
    f(a, arr);
    println a;
    println arr[0];
    // s = "zzzz";
    // print s;
    // print "a: ";
    // println a;
    // // brr = arr;
    // arr[1] = 1;
    // println arr[1];
    // println brr[0];
    // print 10/0;
    // a = false;
    // println arr[0];

    // arr[0] = 1;
    // f(a, arr);
    // print "a after f(): ";
    // println a;
    // println arr[0];
}