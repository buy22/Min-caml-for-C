(Prog
    ((Function
         ((fun_type IntType) (name (ID main)) (params ())
             (body
                 (((Decl
                       ((var_type IntType) (var_name (ID a))
                           (init ((Const (Int 123))))))
                      (Decl
                          ((var_type IntType) (var_name (ID b))
                              (init ((Const (Int 456))))))
                      (Decl
                          ((var_type IntType) (var_name (ID b))
                              (init
                                  ((BinOp Add
                                       (BinOp Mult
                                           (BinOp Div (Const (Int 222))
                                               (Var (ID a)))
                                           (Var (ID b)))
                                       (Const (Int 1)))))))
                      (Statement (ReturnVal (Var (ID b)))))))))))
