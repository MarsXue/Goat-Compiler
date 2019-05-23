    call proc_main
    halt
proc_boo:
  # Prologue
    push_stack_frame 2
  # Passing parameters
    store 0, r0         # Ref m
    store 1, r1         # Val n
  # Initialise Declaration
  # Compile Statements
  # SWrite "test\\n"
    string_const r0, "test\n"
    call_builtin print_string
  # Assign (SBaseVar "m") (Add (Id (SBaseVar "m")) (Id (SBaseVar "n")))
    load r0, 0
    load_indirect r0, r0
    load r1, 1
    add_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
  # Epilogue
    pop_stack_frame 2
    return
proc_main:
  # Prologue
    push_stack_frame 3
  # Passing parameters
  # Initialise Declaration
    int_const r0, 0
    real_const r1, 0.0
    store 0, r0         # x
    store 1, r0         # y
    store 2, r0         # z
  # Compile Statements
  # Assign (SBaseVar "x") (IntConst 1)
    int_const r0, 1
    store 0, r0
  # Assign (SBaseVar "z") (IntConst 2)
    int_const r0, 2
    store 2, r0
  # Read (SBaseVar "y")
    call_builtin read_int
    store 1, r0
  # SWrite "SWrite Test\\n"
    string_const r0, "SWrite Test\n"
    call_builtin print_string
  # Call "boo" [Id (SBaseVar "x"),Id (SBaseVar "z")]
    load_address r0, 0
    load r1, 2
    call proc_boo
  # Write (Id (SBaseVar "x"))
    load r0, 0
    call_builtin print_int
  # Epilogue
    pop_stack_frame 3
    return
