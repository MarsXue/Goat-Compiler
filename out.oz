    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
    int_const r0, 0
    real_const r1, 0.0
# initialise int val x[3]
    store 0, r0
    store 1, r0
    store 2, r0
# initialise int val y
    store 3, r0
# initialise int val z
    store 4, r0
# if (y = 0) || ((y / 0) = 0)
    load r0, 3
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_1
    load r1, 3
    int_const r2, 0
    div_int r1, r1, r2
    int_const r2, 0
    cmp_eq_int r1, r1, r2
    or r0, r0, r1
label_1:
    branch_on_false r0, label_0
# then
# write "success\n";
    string_const r0, "success\n"
    call_builtin print_string
label_0:
# fi
# epilogue
    pop_stack_frame 5
    return
