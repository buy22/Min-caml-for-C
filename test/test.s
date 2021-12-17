(Prog
    ((Function
         ((fun_type IntType) (name (ID test0)) (params ())
             (body
                 (((Decl
                       ((var_type IntType) (var_name (ID c))
                           (init ((Const (Int 0))))))
                      (Statement
                          (ForDecl
                              (init
                                  ((var_type IntType) (var_name (ID i))
                                      (init ((Const (Int 0))))))
                              (cond (BinOp Lt (Var (ID i)) (Const (Int 4))))
                              (post
                                  ((Assign Equals (ID i)
                                       (BinOp Add (Var (ID i))
                                           (Const (Int 1))))))
                              (body ())))
                      (Statement (ReturnVal (Var (ID c)))))))))
        (Function
            ((fun_type StringType) (name (ID main)) (params ())
                (body
                    (((Decl
                          ((var_type StringType) (var_name (ID a))
                              (init ((Const (String w))))))
                         (Statement (ReturnVal (Var (ID a)))))))))))
