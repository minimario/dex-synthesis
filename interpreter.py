import copy
# for i. (x.i * y.i)

# expr = \
#     ("Sum", 
#         ("For", "i",
#             ("BinProd",
#                 ("Dot", ("Var", "x"), "i"),
#                 ("Dot", ("Var", "y"), "i")
#             )
#         )
#     )

# for i. for j. sum (for k. x1.i.k * x2.k.j)
    
expr = \
    ("For", "i",
        ("For", "j",
            ("Sum",
                ("For", "k",
                    ("BinProd",
                        ("Dot", ("Dot", ("Var", "x1"), "i"), "k"),
                        ("Dot", ("Dot", ("Var", "x2"), "k"), "j")
                    )
                )
            )
        )
    )

ctx = {"x1": [[1, 2, 3], [4, 5, 6]], "x2": [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]}

values = {"i": 2, "j": 4, "k": 3}
var_values = {}
def interpret(expr, ctx, values, var_values):
    if expr[0] == "Sum":
        return sum(interpret(expr[1], ctx, values, var_values))
    elif expr[0] == "Var":
        return ctx[expr[1]]
    elif expr[0] == "BinProd":
        return interpret(expr[1], ctx, values, var_values) * interpret(expr[2], ctx, values, var_values)
    elif expr[0] == "Dot":
        body = interpret(expr[1], ctx, values, var_values)
        index = var_values[expr[2]]
        return body[index]
    elif expr[0] == "For":
        res = []
        for i in range(values[expr[1]]):
            new_var_values = copy.deepcopy(var_values)
            new_var_values[expr[1]] = i
            res.append(interpret(expr[2], ctx, values, new_var_values))
        return res

print(interpret(expr, ctx, values, var_values))


# let input_1_type_4 = ArrowType (VarType "n", VarType "v")
# let input_2_type_4 = ArrowType (VarType "n", VarType "v")
# let output_type_4 = VarType "v"
# let ctx_4 = String.Map.of_alist_exn [("x", input_1_type_4); ("y", input_2_type_4)]

