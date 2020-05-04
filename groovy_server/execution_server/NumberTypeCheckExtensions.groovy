// onMethodSelection { expr, node ->
//     // println(node)
//     // println(expr)
//     // println("")
//     if (node.name == "compareTo") {
//         println(node)
//     }
//     handled = true
// }
incompatibleAssignment { lhsType, rhsType, expr ->
    handled=true
}