    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 2
# initialise int val a[2]
    int_const r0, 0
    store 0, r0
    int_const r1, 0
    store 1, r1
# a[1] := 42;
    int_const r0, 42
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[0] := a[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write a[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 2
    return