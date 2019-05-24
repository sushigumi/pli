    call label_main
    halt
label_main:
    push_stack_frame 2
    int_const r0, 0
    real_const r1, 0.0
    store 0, r0
    store 1, r0
    int_const r0, 3
    store 0, r0
    load r0, 0
    int_const r1, 0
    cmp_le_int r0, r0, r1
    store 1, r0
    load r0, 1
label_0:
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    store 0, r0
label_1:
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
