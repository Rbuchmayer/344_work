val test1 = only_lowercase(["Abc"]) = []
val test2 = only_lowercase(["Abc", "ryan", "Andrew", "buchmayer"]) = ["ryan", "buchmayer"]
val test4 = longest_string1(["apple", "test"]) = "apple"
val test5 = longest_string1([ "apples", "orange"]) = "apples"
val test6 = longest_string2([ "apples", "orange"]) = "orange"
val test8 = longest_string3([ "apple", "test"]) = "apple"
val test9 = longest_string3([ "apples", "orange"]) = "apples"
val test11 = longest_string4([ "apple", "test"]) = "apple"
val test12 = longest_string4([ "apples", "orange"]) = "orange"
val test13 = longest_lowercase([ "apple", "Adfdav", "dd"]) = "apple"
val test14 = longest_lowercase(["apple", "Adfdav", "dd"]) = "apple"
val test15 = caps_no_X_string("rCXgxxh") = "RCGH"
val test16 = caps_no_X_string("") = ""
val test17 = count_wildcards(TupleP [WildcardP, WildcardP, VariableP "aa", WildcardP]) = 3
val test18 = count_wildcards(ConstructorP ("aa", WildcardP)) = 1
val test19 = count_a_var("yo", TupleP [WildcardP, WildcardP, VariableP "yo", WildcardP]) = 1
val test20 = count_a_var("yo", TupleP [WildcardP, WildcardP, VariableP "o", WildcardP]) = 0
val test21 = check_pat(TupleP [VariableP "yo", ConstructorP ("aa", VariableP "yo")]) = false
val test22 = check_pat(TupleP [VariableP "yo", ConstructorP ("yo", VariableP "o")]) = true
val test23 = match(Tuple [Constructor("ya", Constant 10), Constructor ("a", Unit), Unit], TupleP [ConstructorP ("ya", VariableP "test"), ConstructorP ("a", VariableP "b"), UnitP])
val test24 = first_match (Constructor("test", Constant 8)) [UnitP, ConstantP 8, ConstructorP("test", VariableP "check")]
