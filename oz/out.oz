    call proc_main
    halt
proc_main:
  # Prologue
    push_stack_frame 1
  # Passing parameters
  # Initialise Declaration
    int_const r0, 0
    real_const r1, 0.0
    store 0, r0         # x
  # Compile Statements
  # Read (line 4, column 5) (SBaseVar "x")
    call_builtin read_int
    store 0, r0
  # Call (line 5, column 5) "factorial" [Id (line 5, column 20) (SBaseVar "x")]
    load_address r0, 0
    call proc_factorial
  # Write (line 6, column 5) (Id (line 6, column 11) (SBaseVar "x"))
    load r0, 0
    call_builtin print_int
  # Epilogue
    pop_stack_frame 1
    return
proc_factorial:
  # Prologue
    push_stack_frame 2
  # Passing parameters
    store 0, r0         # Ref n
  # Initialise Declaration
    int_const r0, 0
    real_const r1, 0.0
    store 1, r0         # f
  # Compile Statements
  # Assign (line 12, column 5) (SBaseVar "f") (IntConst (line 12, column 10) 1)
    int_const r0, 1
    store 1, r0
  # If (line 14, column 5) (Less (line 14, column 10) (Id (line 14, column 8) (SBaseVar "n")) (IntConst (line 14, column 12) 0)) [SWrite (line 15, column 9) "Less than zero!"] [While (line 17, column 9) (Greater (line 17, column 17) (Id (line 17, column 15) (SBaseVar "n")) (IntConst (line 17, column 19) 1)) [Assign (line 18, column 13) (SBaseVar "f") (Mul (line 18, column 20) (Id (line 18, column 18) (SBaseVar "f")) (Id (line 18, column 22) (SBaseVar "n"))),Assign (line 19, column 13) (SBaseVar "n") (Minus (line 19, column 20) (Id (line 19, column 18) (SBaseVar "n")) (IntConst (line 19, column 22) 1))]]
    load r0, 0
    load_indirect r0, r0
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_false r0, label_0
    string_const r0, "Less than zero!"
    call_builtin print_string
    branch_uncond label_1
label_0:
label_2:
    load r0, 0
    load_indirect r0, r0
    int_const r1, 1
    cmp_gt_int r0, r0, r1
    branch_on_false r0, label_3
    load r1, 1
    load r2, 0
    load_indirect r2, r2
    mul_int r1, r1, r2
    store 1, r1
    load r2, 0
    load_indirect r2, r2
    int_const r3, 1
    sub_int r2, r2, r3
    load r3, 0
    store_indirect r3, r2
    branch_uncond label_2
label_3:
label_1:
  # Write (line 22, column 5) (Id (line 22, column 11) (SBaseVar "f"))
    load r0, 1
    call_builtin print_int
  # Epilogue
    pop_stack_frame 2
    return
