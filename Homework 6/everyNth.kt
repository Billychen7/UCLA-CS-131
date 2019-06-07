fun main(args: Array<String>) {
    //test 1: empty list
    val test1 = everyNth(listOf<Int>(), 0)
    if (test1 == listOf<Int>()) {
        println("Test 1 Passed")
    }
    else {
        println("Test 1 Failed")
    }

    //test 2 and 3: invalid N returns empty list
    val test2 = everyNth(listOf(1,2,3), -1)
    if (test2 == listOf<Int>()) {
        println("Test 2 Passed")
    }
    else {
        println("Test 2 Failed")
    }

    val test3 = everyNth(listOf(1,2,3), 4)
    if (test3 == listOf<Int>()) {
        println("Test 3 Passed")
    }
    else {
        println("Test 3 Failed")
    }

    //tests 4 and 5: list of ints
    val test4 = everyNth(listOf(1,2,3,4,5,6), 2)
    if (test4 == listOf(2,4,6)) {
        println("Test 4 Passed")
    }
    else {
        println("Test 4 Failed")
    }

    val test5 = everyNth(listOf(1,2,3,4,5,6), 3)
    if (test5 == listOf(3,6)) {
        println("Test 5 Passed")
    }
    else {
        println("Test 5 Failed")
    }

    //test 6: list of strings
    val test6 = everyNth(listOf("a","b","c","d","e","f","g"), 2)
    if (test6 == listOf("b","d","f")) {
        println("Test 6 Passed")
    }
    else {
        println("Test 6 Failed")
    }
}

// accepts a list L and positive integer N
// returns a list containing every Nth element of L
fun <T> everyNth(L: List<T>, N: Int): List<T> {
    //check for valid N
    if (N > L.size || N < 1) {
        return emptyList()
    }
    
    val temp_result = ArrayList<T>()
    for (i in N - 1 .. L.size - 1 step N)
    {
        temp_result.add(L.get(i))
    }
    
    val result: List<T> = temp_result
    return result
}
