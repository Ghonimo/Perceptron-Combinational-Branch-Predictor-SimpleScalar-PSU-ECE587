gcc2_compiled.:
___gnu_compiled_c:
.data
	.align 4
_in_section:
	.word	0
.text
	.align 8
LC0:
	.ascii "%s\12\0"
	.align 8
LC1:
	.ascii ".text\0"
	.align 4
	.global _text_section
	.proc	020
_text_section:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,1
	be L4
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC1),%o2
	call _fprintf,0
	or %o2,%lo(LC1),%o2
	mov 1,%o0
	st %o0,[%l0+%lo(_in_section)]
L4:
	ret
	restore
	.align 8
LC2:
	.ascii ".data\0"
	.align 4
	.global _data_section
	.proc	020
_data_section:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	sethi %hi(_in_section),%o0
	ld [%o0+%lo(_in_section)],%o0
	cmp %o0,2
	be L10
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC2),%o2
	call _fprintf,0
	or %o2,%lo(LC2),%o2
	mov 2,%o0
	sethi %hi(_in_section),%o1
	st %o0,[%o1+%lo(_in_section)]
L10:
	ret
	restore
	.align 4
	.global _make_function_rtl
	.proc	020
_make_function_rtl:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+64],%o0
	cmp %o0,0
	bne,a L17
	mov 1,%o0
	ld [%i0+36],%o1
	ld [%o1+20],%o2
	mov 39,%o0
	call _gen_rtx,0
	mov 4,%o1
	mov %o0,%o2
	ld [%i0+28],%o1
	call _gen_rtx,0
	mov 37,%o0
	st %o0,[%i0+64]
	mov 1,%o0
L17:
	sethi %hi(_function_defined),%o1
	st %o0,[%o1+%lo(_function_defined)]
	ret
	restore
	.align 8
LC3:
	.ascii "register name not specified for `%s'\0"
	.align 8
LC4:
	.ascii "invalid register name for `%s'\0"
	.align 8
LC5:
	.ascii "register name given for non-register variable `%s'\0"
	.align 8
LC6:
	.ascii "function declared `register'\0"
	.align 8
LC7:
	.ascii "data type of `%s' isn't suitable for a register\0"
	.align 8
LC8:
	.ascii "ANSI C forbids global register variables\0"
	.align 8
LC9:
	.ascii "global register variable has initial value\0"
	.align 8
LC10:
	.ascii "global register variable follows a function definition\0"
	.align 8
LC11:
	.ascii "%s.%d\0"
	.align 4
	.global _make_decl_rtl
	.proc	020
_make_decl_rtl:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+36],%o0
	mov -1,%l0
	cmp %i1,0
	be L19
	ld [%o0+20],%l2
	ldub [%i1+12],%o0
	cmp %o0,38
	be L20
	mov 0,%l0
	call _abort,0
	nop
L20:
	sethi %hi(_reg_names),%o0
	or %o0,%lo(_reg_names),%l3
	mov 0,%l1
L25:
	ld [%i1+24],%o0
	call _strcmp,0
	ld [%l1+%l3],%o1
	cmp %o0,0
	be L65
	cmp %l0,55
	add %l0,1,%l0
	cmp %l0,55
	ble L25
	add %l1,4,%l1
	cmp %l0,55
L65:
	ble L19
	sethi %hi(_saveable_obstack),%o1
	ld [%i1+24],%o0
	call _strlen,0
	ld [%o1+%lo(_saveable_obstack)],%l0
	ld [%l0+12],%o1
	add %o0,2,%l1
	ld [%l0+16],%o0
	add %o1,%l1,%o1
	cmp %o1,%o0
	bleu L29
	mov %l0,%o0
	call __obstack_newchunk,0
	mov %l1,%o1
L29:
	ld [%l0+12],%o0
	mov %l0,%o3
	add %o0,%l1,%o0
	st %o0,[%l0+12]
	ld [%o3+24],%o1
	ld [%o3+16],%o4
	ld [%o3+8],%o2
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o3+4],%o0
	st %o1,[%o3+12]
	sub %o1,%o0,%o1
	sub %o4,%o0,%o0
	cmp %o1,%o0
	bg,a L31
	st %o4,[%o3+12]
L31:
	mov %o2,%l2
	ld [%l0+12],%o1
	add %l2,1,%o0
	st %o1,[%l0+8]
	mov 42,%o1
	stb %o1,[%l2]
	ld [%i1+24],%o1
	call _strcpy,0
	mov -2,%l0
L19:
	ld [%i0+64],%o0
	cmp %o0,0
	be,a L66
	st %g0,[%i0+64]
	ldub [%o0+2],%o1
	ld [%i0+28],%o0
	cmp %o1,%o0
	be L32
	nop
	st %g0,[%i0+64]
L66:
	ld [%i0+12],%o1
	sethi %hi(8192),%o0
	andcc %o1,%o0,%g0
	be L78
	cmp %l0,-1
	bne L67
	andcc %o1,%o0,%g0
	mov %i0,%o0
	sethi %hi(LC3),%o1
	call _error_with_decl,0
	or %o1,%lo(LC3),%o1
	ld [%i0+12],%o1
	sethi %hi(8192),%o0
L78:
	andcc %o1,%o0,%g0
L67:
	be L35
	cmp %l0,-2
	bne L68
	cmp %l0,0
	mov %i0,%o0
	sethi %hi(LC4),%o1
	call _error_with_decl,0
	or %o1,%lo(LC4),%o1
	b L69
	ld [%i0+64],%o0
L35:
	cmp %l0,0
L68:
	bl L37
	sethi %hi(8192),%o0
	ld [%i0+12],%o1
	andcc %o1,%o0,%g0
	bne,a L70
	ld [%i0+12],%o2
	mov %i0,%o0
	sethi %hi(LC5),%o1
	call _error_with_decl,0
	or %o1,%lo(LC5),%o1
	b L69
	ld [%i0+64],%o0
L37:
	ld [%i0+12],%o2
L70:
	sethi %hi(-16769024),%o0
	sethi %hi(654319616),%o1
	and %o2,%o0,%o0
	cmp %o0,%o1
	bne L39
	sethi %hi(8192),%o0
	sethi %hi(LC6),%o0
	call _error,0
	or %o0,%lo(LC6),%o0
	b L69
	ld [%i0+64],%o0
L39:
	andcc %o2,%o0,%g0
	be,a L71
	ld [%i0+12],%o1
	ld [%i0+8],%o0
	ldub [%o0+28],%o0
	cmp %o0,26
	bne,a L71
	ld [%i0+12],%o1
	mov %i0,%o0
	sethi %hi(LC7),%o1
	call _error_with_decl,0
	or %o1,%lo(LC7),%o1
	b L69
	ld [%i0+64],%o0
L71:
	sethi %hi(8192),%o0
	andcc %o1,%o0,%g0
	be L36
	sethi %hi(_pedantic),%o0
	ld [%o0+%lo(_pedantic)],%o0
	cmp %o0,0
	be L44
	sethi %hi(LC8),%o0
	call _warning,0
	or %o0,%lo(LC8),%o0
L44:
	ld [%i0+60],%o0
	cmp %o0,0
	be L45
	sethi %hi(LC9),%o0
	st %g0,[%i0+60]
	call _error,0
	or %o0,%lo(LC9),%o0
L45:
	sethi %hi(_fixed_regs),%o0
	or %o0,%lo(_fixed_regs),%o0
	ldsb [%l0+%o0],%o0
	cmp %o0,0
	bne L72
	mov 34,%o0
	sethi %hi(_function_defined),%o0
	ld [%o0+%lo(_function_defined)],%o0
	cmp %o0,0
	be L46
	sethi %hi(LC10),%o0
	call _error,0
	or %o0,%lo(LC10),%o0
L46:
	mov 34,%o0
L72:
	ld [%i0+28],%o1
	call _gen_rtx,0
	mov %l0,%o2
	cmp %l0,15
	bg L47
	st %o0,[%i0+64]
	sethi %hi(_mode_size),%o1
	ld [%i0+28],%o0
	or %o1,%lo(_mode_size),%o1
	sll %o0,2,%o0
	ld [%o0+%o1],%o0
	addcc %o0,3,%o1
	bneg,a L49
	add %o0,6,%o1
L49:
	b L48
	sra %o1,2,%o1
L47:
	mov 1,%o1
L48:
	cmp %o1,0
	ble L51
	sethi %hi(_global_regs),%o0
	or %o0,%lo(_global_regs),%o3
	mov 1,%o2
L52:
	add %o1,-1,%o1
	add %l0,%o1,%o0
	cmp %o1,0
	bg L52
	stb %o2,[%o0+%o3]
L51:
	call _init_reg_sets_1,0
	nop
L36:
	ld [%i0+64],%o0
L69:
	cmp %o0,0
	bne L32
	cmp %i2,0
	bne L73
	mov 39,%o0
	ld [%i0+12],%o1
	sethi %hi(8388608),%o0
	andcc %o1,%o0,%g0
	bne,a L73
	mov 39,%o0
	cmp %i1,0
	bne,a L73
	mov 39,%o0
	call _strlen,0
	mov %l2,%o0
	add %o0,116,%o0
	and %o0,-8,%o0
	sub %sp,%o0,%sp
	add %sp,96,%l0
	mov %l0,%o0
	sethi %hi(LC11),%o1
	or %o1,%lo(LC11),%o1
	sethi %hi(_var_labelno),%l3
	ld [%l3+%lo(_var_labelno)],%o3
	call _sprintf,0
	mov %l2,%o2
	sethi %hi(_saveable_obstack),%o0
	ld [%o0+%lo(_saveable_obstack)],%l1
	call _strlen,0
	mov %l0,%o0
	ld [%l1+12],%o1
	mov %o0,%i1
	ld [%l1+16],%o0
	add %o1,%i1,%o1
	add %o1,1,%o1
	cmp %o1,%o0
	bleu L56
	mov %l1,%o0
	call __obstack_newchunk,0
	add %i1,1,%o1
L56:
	mov %l0,%o1
	ld [%l1+12],%o0
	call _memcpy,0
	mov %i1,%o2
	ld [%l1+12],%o0
	mov %l1,%o3
	add %o0,%i1,%o0
	add %o0,1,%o1
	st %o1,[%l1+12]
	stb %g0,[%o0]
	ld [%o3+12],%o0
	ld [%o3+24],%o1
	ld [%o3+16],%o4
	ld [%o3+8],%o2
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o3+4],%o0
	st %o1,[%o3+12]
	sub %o1,%o0,%o1
	sub %o4,%o0,%o0
	cmp %o1,%o0
	bg,a L58
	st %o4,[%o3+12]
L58:
	ld [%l1+12],%o1
	mov %o2,%l2
	ld [%l3+%lo(_var_labelno)],%o0
	st %o1,[%l1+8]
	add %o0,1,%o0
	st %o0,[%l3+%lo(_var_labelno)]
	mov 39,%o0
L73:
	mov 4,%o1
	call _gen_rtx,0
	mov %l2,%o2
	mov %o0,%o2
	ld [%i0+28],%o1
	call _gen_rtx,0
	mov 37,%o0
	mov %o0,%o2
	st %o2,[%i0+64]
	ld [%i0+12],%o1
	sethi %hi(1048576),%o0
	andcc %o1,%o0,%g0
	be L74
	sethi %hi(262144),%o0
	ld [%o2],%o0
	or %o0,16,%o0
	st %o0,[%o2]
	ld [%i0+12],%o1
	sethi %hi(262144),%o0
L74:
	andcc %o1,%o0,%g0
	be,a L75
	ld [%i0+8],%o0
	ld [%i0+64],%o1
	ld [%o1],%o0
	or %o0,32,%o0
	st %o0,[%o1]
	ld [%i0+8],%o0
L75:
	ldub [%o0+12],%o0
	mov 0,%o1
	cmp %o0,16
	be L62
	ld [%i0+64],%i0
	add %o0,-19,%o0
	and %o0,0xff,%o0
	cmp %o0,1
	bgu,a L76
	ld [%i0],%o0
L62:
	mov 1,%o1
	ld [%i0],%o0
L76:
	sll %o1,3,%o1
	and %o0,-9,%o0
	or %o0,%o1,%o0
	st %o0,[%i0]
L32:
	ret
	restore
	.align 8
LC12:
	.ascii "\11%s\12\0"
	.align 4
	.global _assemble_asm
	.proc	020
_assemble_asm:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	call _app_enable,0
	nop
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC12),%o1
	ld [%i0+24],%o2
	call _fprintf,0
	or %o1,%lo(LC12),%o1
	ret
	restore
	.align 8
LC13:
	.ascii "\11.even\12\0"
	.align 8
LC14:
	.ascii ".globl \0"
	.align 8
LC15:
	.ascii "\12\0"
	.align 8
LC16:
	.ascii ":\12\0"
	.align 4
	.global _assemble_function
	.proc	020
_assemble_function:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+64],%o1
	lduh [%o1],%o0
	cmp %o0,37
	be,a L82
	ld [%o1+4],%o1
	call _abort,0
	nop
L82:
	lduh [%o1],%o0
	cmp %o0,39
	be L83
	nop
	call _abort,0
	nop
L83:
	call _app_disable,0
	ld [%o1+4],%l1
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,1
	be L84
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC1),%o2
	call _fprintf,0
	or %o2,%lo(LC1),%o2
	mov 1,%o0
	st %o0,[%l0+%lo(_in_section)]
L84:
	call _floor_log2,0
	mov 2,%o0
	cmp %o0,1
	bne L86
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC13),%o1
	call _fprintf,0
	or %o1,%lo(LC13),%o1
	b L96
	ld [%i0+12],%o1
L86:
	call _floor_log2,0
	mov 2,%o0
	cmp %o0,0
	be,a L96
	ld [%i0+12],%o1
	call _abort,0
	nop
L96:
	sethi %hi(4194304),%o0
	andcc %o1,%o0,%g0
	be L89
	sethi %hi(_asm_out_file),%l0
	sethi %hi(LC14),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC14),%o0
	ld [%l0+%lo(_asm_out_file)],%o0
	call _assemble_name,0
	mov %l1,%o1
	sethi %hi(LC15),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC15),%o0
L89:
	sethi %hi(_asm_out_file),%l0
	ld [%l0+%lo(_asm_out_file)],%o0
	call _assemble_name,0
	mov %l1,%o1
	sethi %hi(LC16),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC16),%o0
	ret
	restore
	.align 8
LC17:
	.ascii "\11.long \0"
	.align 4
	.global _assemble_integer_zero
	.proc	020
_assemble_integer_zero:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	sethi %hi(_asm_out_file),%l0
	sethi %hi(LC17),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC17),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	sethi %hi(_const0_rtx),%o1
	call _output_addr_const,0
	ld [%o1+%lo(_const0_rtx)],%o1
	sethi %hi(LC15),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC15),%o1
	ret
	restore
	.align 8
LC18:
	.ascii "\11.ascii \"\0"
	.align 8
LC19:
	.ascii "\\%o\0"
	.align 8
LC20:
	.ascii "\"\12\11.ascii \"\0"
	.align 8
LC21:
	.ascii "\"\12\0"
	.align 4
	.global _assemble_string
	.proc	020
_assemble_string:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	mov 0,%l5
	cmp %l5,%i1
	bge L101
	mov 2000,%l7
	sethi %hi(_asm_out_file),%l1
	mov 92,%l6
	sub %i1,%l5,%l2
L130:
	cmp %l2,%l7
	bg,a L102
	mov %l7,%l2
L102:
	ld [%l1+%lo(_asm_out_file)],%o0
	sethi %hi(LC18),%o1
	or %o1,%lo(LC18),%o1
	call _fprintf,0
	mov 0,%l3
	cmp %l3,%l2
	bge L125
	ld [%l1+%lo(_asm_out_file)],%o0
	mov %i0,%l4
L123:
	ldub [%l4],%l0
	cmp %l0,34
	be L107
	ld [%l1+%lo(_asm_out_file)],%o2
	cmp %l0,92
	bne L126
	add %l0,-32,%o0
L107:
	ld [%o2],%o0
	add %o0,-1,%o3
	cmp %o3,0
	bl L108
	st %o3,[%o2]
	ld [%o2+4],%o1
	add %o1,1,%o0
	st %o0,[%o2+4]
	b L106
	stb %l6,[%o1]
L108:
	lduh [%o2+16],%o0
	andcc %o0,128,%g0
	be L110
	sub %g0,%o3,%o0
	ld [%o2+12],%o1
	cmp %o0,%o1
	bge L127
	ld [%l1+%lo(_asm_out_file)],%o1
	ld [%o2+4],%o0
	stb %l6,[%o0]
	ld [%l1+%lo(_asm_out_file)],%o1
	ld [%o1+4],%o0
	add %o0,1,%o0
	b L106
	st %o0,[%o1+4]
L110:
	ld [%l1+%lo(_asm_out_file)],%o1
L127:
	call __flsbuf,0
	mov 92,%o0
L106:
	add %l0,-32,%o0
L126:
	cmp %o0,94
	bgu L114
	ld [%l1+%lo(_asm_out_file)],%o2
	ld [%o2],%o0
	add %o0,-1,%o3
	cmp %o3,0
	bl L115
	st %o3,[%o2]
	ld [%o2+4],%o1
	add %o1,1,%o0
	st %o0,[%o2+4]
	b L105
	stb %l0,[%o1]
L115:
	lduh [%o2+16],%o0
	andcc %o0,128,%g0
	be L117
	sub %g0,%o3,%o0
	ld [%o2+12],%o1
	cmp %o0,%o1
	bge L128
	ld [%l1+%lo(_asm_out_file)],%o1
	ld [%o2+4],%o1
	mov %l0,%o0
	cmp %o0,10
	be L119
	stb %o0,[%o1]
	ld [%l1+%lo(_asm_out_file)],%o1
	ld [%o1+4],%o0
	add %o0,1,%o0
	b L105
	st %o0,[%o1+4]
L119:
	ld [%l1+%lo(_asm_out_file)],%o1
	ld [%o1+4],%o0
	call __flsbuf,0
	ldub [%o0],%o0
	b L129
	add %l3,1,%l3
L117:
	ld [%l1+%lo(_asm_out_file)],%o1
L128:
	call __flsbuf,0
	mov %l0,%o0
	b L129
	add %l3,1,%l3
L114:
	ld [%l1+%lo(_asm_out_file)],%o0
	sethi %hi(LC19),%o1
	or %o1,%lo(LC19),%o1
	call _fprintf,0
	mov %l0,%o2
	add %l2,-1,%o0
	cmp %l3,%o0
	bge,a L129
	add %l3,1,%l3
	ldub [%l4+1],%o0
	add %o0,-48,%o0
	and %o0,0xff,%o0
	cmp %o0,9
	bgu,a L129
	add %l3,1,%l3
	ld [%l1+%lo(_asm_out_file)],%o0
	sethi %hi(LC20),%o1
	call _fprintf,0
	or %o1,%lo(LC20),%o1
L105:
	add %l3,1,%l3
L129:
	cmp %l3,%l2
	bl L123
	add %l4,1,%l4
	ld [%l1+%lo(_asm_out_file)],%o0
L125:
	sethi %hi(LC21),%o1
	or %o1,%lo(LC21),%o1
	add %l5,%l2,%l5
	call _fprintf,0
	add %i0,%l2,%i0
	cmp %l5,%i1
	bl L130
	sub %i1,%l5,%l2
L101:
	ret
	restore
	.align 8
LC22:
	.ascii "storage size of static var `%s' isn't known\0"
	.align 8
LC23:
	.ascii ".comm \0"
	.align 8
LC24:
	.ascii ",%d\12\0"
	.align 8
LC25:
	.ascii ".lcomm \0"
	.align 8
LC26:
	.ascii "\11.skip %d\12\0"
	.align 4
	.global _assemble_variable
	.proc	020
_assemble_variable:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+64],%o0
	lduh [%o0],%o0
	cmp %o0,34
	be L131
	sethi %hi(8388608),%o0
	ld [%i0+12],%o1
	andcc %o1,%o0,%g0
	bne L131
	nop
	ldub [%i0+12],%o0
	cmp %o0,39
	be L131
	nop
	ld [%i0+24],%o0
	cmp %o0,0
	bne L179
	mov %i0,%o0
	call _layout_decl,0
	mov 0,%o1
	ld [%i0+24],%o0
	cmp %o0,0
L179:
	bne,a L136
	ld [%i0+12],%o0
	ld [%i0+36],%o2
	ld [%i0+16],%o0
	ld [%o2+20],%o3
	ld [%i0+20],%o1
	sethi %hi(LC22),%o2
	call _error_with_file_and_line,0
	or %o2,%lo(LC22),%o2
	b,a L131
L136:
	andcc %o0,1024,%g0
	bne L131
	or %o0,1024,%o0
	cmp %i2,2
	bne L138
	st %o0,[%i0+12]
	cmp %i1,0
	be L138
	mov %i0,%o0
	call _dbxout_symbol,0
	mov 0,%o1
L138:
	cmp %i2,1
	bne,a L180
	ld [%i0+24],%o0
	call _set_current_gdbfile,0
	ld [%i0+16],%o0
	ld [%i0+24],%o0
L180:
	ld [%o0+12],%o1
	sethi %hi(131072),%o0
	andcc %o1,%o0,%g0
	be L131
	nop
	call _app_disable,0
	nop
	ld [%i0+64],%o0
	ld [%i0+60],%o1
	ld [%o0+4],%o0
	cmp %o1,0
	be L142
	ld [%o0+4],%i2
	sethi %hi(_error_mark_node),%o0
	ld [%o0+%lo(_error_mark_node)],%o0
	cmp %o1,%o0
	bne,a L141
	ld [%i0+12],%o1
L142:
	ld [%i0+24],%o0
	ldub [%i0+32],%o1
	call .umul,0
	ld [%o0+16],%o0
	cmp %o0,0
	bl,a L143
	add %o0,7,%o0
L143:
	sra %o0,3,%i1
	cmp %i1,0
	be,a L144
	mov 1,%i1
L144:
	add %i1,1,%o0
	srl %o0,31,%o1
	add %o0,%o1,%o0
	sethi %hi(_flag_shared_data),%o1
	ld [%o1+%lo(_flag_shared_data)],%o1
	cmp %o1,0
	be L145
	and %o0,-2,%i1
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,2
	be L145
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC2),%o2
	call _fprintf,0
	or %o2,%lo(LC2),%o2
	mov 2,%o0
	st %o0,[%l0+%lo(_in_section)]
L145:
	ld [%i0+12],%o1
	sethi %hi(4194304),%o0
	andcc %o1,%o0,%g0
	be L150
	sethi %hi(_asm_out_file),%l0
	sethi %hi(LC23),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC23),%o0
	b L181
	ld [%l0+%lo(_asm_out_file)],%o0
L150:
	sethi %hi(LC25),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC25),%o0
	ld [%l0+%lo(_asm_out_file)],%o0
L181:
	call _assemble_name,0
	mov %i2,%o1
	sethi %hi(LC24),%o1
	or %o1,%lo(LC24),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	mov %i1,%o2
	b,a L131
L141:
	sethi %hi(4194304),%o0
	andcc %o1,%o0,%g0
	be,a L182
	ld [%i0+60],%o0
	ld [%i0+36],%o0
	cmp %o0,0
	be L152
	sethi %hi(_asm_out_file),%l0
	sethi %hi(LC14),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC14),%o0
	ld [%l0+%lo(_asm_out_file)],%o0
	call _assemble_name,0
	mov %i2,%o1
	sethi %hi(LC15),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC15),%o0
L152:
	ld [%i0+60],%o0
L182:
	cmp %o0,0
	be,a L183
	ld [%i0+12],%o0
	call _output_addressed_constants,0
	nop
	ld [%i0+12],%o0
L183:
	sethi %hi(1310720),%o1
	and %o0,%o1,%o0
	sethi %hi(262144),%o1
	cmp %o0,%o1
	bne L157
	sethi %hi(_in_section),%o0
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,1
	be L160
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC1),%o2
	call _fprintf,0
	or %o2,%lo(LC1),%o2
	mov 1,%o0
	b L160
	st %o0,[%l0+%lo(_in_section)]
L157:
	ld [%o0+%lo(_in_section)],%o0
	cmp %o0,2
	be L160
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC2),%o2
	call _fprintf,0
	or %o2,%lo(LC2),%o2
	mov 2,%o0
	sethi %hi(_in_section),%o1
	st %o0,[%o1+%lo(_in_section)]
L160:
	ldub [%i0+33],%o0
	cmp %o0,16
	bl L166
	mov 0,%o1
	mov %o0,%o2
	mov 8,%o3
	add %o1,1,%o1
L184:
	add %o1,1,%o0
	sll %o3,%o0,%o0
	cmp %o2,%o0
	bge,a L184
	add %o1,1,%o1
L166:
	cmp %o1,1
	bne L169
	cmp %o1,0
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC13),%o1
	call _fprintf,0
	or %o1,%lo(LC13),%o1
	b L185
	sethi %hi(_asm_out_file),%l0
L169:
	be L185
	sethi %hi(_asm_out_file),%l0
	call _abort,0
	nop
L185:
	ld [%l0+%lo(_asm_out_file)],%o0
	call _assemble_name,0
	mov %i2,%o1
	sethi %hi(LC16),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC16),%o0
	ld [%i0+60],%o0
	cmp %o0,0
	be L175
	nop
	call _int_size_in_bytes,0
	ld [%i0+8],%o0
	mov %o0,%o1
	call _output_constant,0
	ld [%i0+60],%o0
	b,a L131
L175:
	call _int_size_in_bytes,0
	ld [%i0+8],%o0
	sethi %hi(LC26),%o1
	mov %o0,%o2
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC26),%o1
L131:
	ret
	restore
	.align 4
	.global _assemble_external
	.proc	020
_assemble_external:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ret
	restore
	.align 8
LC27:
	.ascii "_%s\0"
	.align 4
	.global _assemble_name
	.proc	020
_assemble_name:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	mov %i1,%o2
	ldsb [%o2],%o0
	cmp %o0,42
	bne L194
	mov %i0,%o1
	call _fputs,0
	add %o2,1,%o0
	b,a L195
L194:
	mov %o1,%o0
	sethi %hi(LC27),%o1
	call _fprintf,0
	or %o1,%lo(LC27),%o1
L195:
	ret
	restore
	.align 8
LC28:
	.ascii "*%s%d\0"
	.align 8
LC29:
	.ascii "LF\0"
	.align 4
	.global _assemble_static_space
	.proc	0110
_assemble_static_space:
	!#PROLOGUE# 0
	save %sp,-128,%sp
	!#PROLOGUE# 1
	sethi %hi(_flag_shared_data),%o0
	ld [%o0+%lo(_flag_shared_data)],%o0
	add %i0,1,%i0
	cmp %o0,0
	srl %i0,31,%o0
	add %i0,%o0,%i0
	be L197
	and %i0,-2,%l3
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,2
	be L197
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC2),%o2
	call _fprintf,0
	or %o2,%lo(LC2),%o2
	mov 2,%o0
	st %o0,[%l0+%lo(_in_section)]
L197:
	add %fp,-32,%l2
	mov %l2,%o0
	sethi %hi(LC28),%o1
	or %o1,%lo(LC28),%o1
	sethi %hi(_const_labelno),%l0
	sethi %hi(LC29),%o2
	ld [%l0+%lo(_const_labelno)],%o3
	call _sprintf,0
	or %o2,%lo(LC29),%o2
	mov %l2,%o0
	ld [%l0+%lo(_const_labelno)],%o2
	sethi %hi(_saveable_obstack),%o1
	ld [%o1+%lo(_saveable_obstack)],%l1
	add %o2,1,%o2
	call _strlen,0
	st %o2,[%l0+%lo(_const_labelno)]
	ld [%l1+12],%o1
	add %o0,2,%l0
	ld [%l1+16],%o0
	add %o1,%l0,%o1
	cmp %o1,%o0
	bleu L203
	mov %l1,%o0
	call __obstack_newchunk,0
	mov %l0,%o1
L203:
	ld [%l1+12],%o0
	mov %l1,%o2
	add %o0,%l0,%o0
	st %o0,[%l1+12]
	ld [%o2+24],%o1
	ld [%o2+16],%o3
	ld [%o2+8],%l0
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o2+4],%o0
	st %o1,[%o2+12]
	sub %o1,%o0,%o1
	sub %o3,%o0,%o0
	cmp %o1,%o0
	bg,a L205
	st %o3,[%o2+12]
L205:
	mov %l0,%o0
	ld [%l1+12],%o2
	mov %l2,%o1
	call _strcpy,0
	st %o2,[%l1+8]
	mov 39,%o0
	mov 4,%o1
	call _gen_rtx,0
	mov %l0,%o2
	sethi %hi(_asm_out_file),%l0
	mov %o0,%i0
	sethi %hi(LC25),%o0
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	or %o0,%lo(LC25),%o0
	ldsb [%fp-32],%o0
	cmp %o0,42
	bne L207
	ld [%l0+%lo(_asm_out_file)],%o1
	call _fputs,0
	add %fp,-31,%o0
	b L209
	sethi %hi(_asm_out_file),%o0
L207:
	mov %o1,%o0
	sethi %hi(LC27),%o1
	or %o1,%lo(LC27),%o1
	call _fprintf,0
	mov %l2,%o2
	sethi %hi(_asm_out_file),%o0
L209:
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC24),%o1
	or %o1,%lo(LC24),%o1
	call _fprintf,0
	mov %l3,%o2
	ret
	restore
	.align 4
	.global _immed_double_const
	.proc	0110
_immed_double_const:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	mov %i0,%o3
	mov %i2,%o1
	cmp %o1,6
	bne L218
	mov %i1,%o4
	cmp %o3,0
	bne L227
	sethi %hi(_real_constant_chain),%o0
	cmp %o4,0
	bne L228
	ld [%o0+%lo(_real_constant_chain)],%i0
	sethi %hi(_const0_rtx),%o0
	b L224
	ld [%o0+%lo(_const0_rtx)],%i0
L218:
	sethi %hi(_real_constant_chain),%o0
L227:
	ld [%o0+%lo(_real_constant_chain)],%i0
L228:
	cmp %i0,0
	be L229
	mov 31,%o0
	ld [%i0+12],%o0
L231:
	cmp %o0,%o3
	bne,a L230
	ld [%i0+8],%i0
	ld [%i0+16],%o0
	cmp %o0,%o4
	bne,a L230
	ld [%i0+8],%i0
	ldub [%i0+2],%o0
	cmp %o0,%o1
	be L224
	nop
	ld [%i0+8],%i0
L230:
	cmp %i0,0
	bne,a L231
	ld [%i0+12],%o0
	mov 31,%o0
L229:
	call _gen_rtx,0
	mov 0,%o2
	sethi %hi(_real_constant_chain),%o1
	ld [%o1+%lo(_real_constant_chain)],%o2
	mov %o0,%i0
	st %i0,[%o1+%lo(_real_constant_chain)]
	sethi %hi(_const0_rtx),%o0
	ld [%o0+%lo(_const0_rtx)],%o0
	st %o2,[%i0+8]
	st %o0,[%i0+4]
L224:
	ret
	restore
	.align 4
	.global _immed_real_const_1
	.proc	0110
_immed_real_const_1:
	!#PROLOGUE# 0
	save %sp,-120,%sp
	!#PROLOGUE# 1
	std %i0,[%fp-16]
	ldd [%fp-16],%f6
	fmovs %f6,%f2
	fmovs %f7,%f3
	fnegs %f2,%f4
	fmovs %f3,%f5
	mov %i2,%o1
	fcmpd %f4,%f2
	nop
	fbne L233
	std %f2,[%fp-24]
	cmp %o1,11
	bne L234
	sethi %hi(_fconst0_rtx),%o0
	sethi %hi(_dconst0_rtx),%o0
	b L249
	ld [%o0+%lo(_dconst0_rtx)],%i0
L234:
	b L249
	ld [%o0+%lo(_fconst0_rtx)],%i0
L233:
	ld [%fp-24],%o3
	cmp %o1,6
	bne L238
	ld [%fp-20],%o4
	cmp %o3,0
	bne L254
	sethi %hi(_real_constant_chain),%o0
	cmp %o4,0
	bne L255
	ld [%o0+%lo(_real_constant_chain)],%i0
	sethi %hi(_const0_rtx),%o0
	b L249
	ld [%o0+%lo(_const0_rtx)],%i0
L238:
	sethi %hi(_real_constant_chain),%o0
L254:
	ld [%o0+%lo(_real_constant_chain)],%i0
L255:
	cmp %i0,0
	be L256
	mov 31,%o0
	ld [%i0+12],%o0
L258:
	cmp %o0,%o3
	bne,a L257
	ld [%i0+8],%i0
	ld [%i0+16],%o0
	cmp %o0,%o4
	bne,a L257
	ld [%i0+8],%i0
	ldub [%i0+2],%o0
	cmp %o0,%o1
	be L249
	nop
	ld [%i0+8],%i0
L257:
	cmp %i0,0
	bne,a L258
	ld [%i0+12],%o0
	mov 31,%o0
L256:
	call _gen_rtx,0
	mov 0,%o2
	sethi %hi(_real_constant_chain),%o1
	ld [%o1+%lo(_real_constant_chain)],%o2
	mov %o0,%i0
	st %i0,[%o1+%lo(_real_constant_chain)]
	sethi %hi(_const0_rtx),%o0
	ld [%o0+%lo(_const0_rtx)],%o0
	st %o2,[%i0+8]
	st %o0,[%i0+4]
L249:
	ret
	restore
	.align 4
	.global _immed_real_const
	.proc	0110
_immed_real_const:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+8],%o2
	ldd [%i0+24],%o0
	call _immed_real_const_1,0
	ldub [%o2+28],%o2
	ret
	restore %g0,%o0,%o0
	.align 4
	.global _force_const_double_mem
	.proc	0110
_force_const_double_mem:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+4],%o0
	sethi %hi(_cc0_rtx),%o1
	ld [%o1+%lo(_cc0_rtx)],%o1
	cmp %o0,%o1
	bne L270
	sethi %hi(_const0_rtx),%o1
	sethi %hi(_real_constant_chain),%o0
	ld [%o0+%lo(_real_constant_chain)],%o1
	st %i0,[%o0+%lo(_real_constant_chain)]
	sethi %hi(_const0_rtx),%o0
	ld [%o0+%lo(_const0_rtx)],%o0
	st %o1,[%i0+8]
	st %o0,[%i0+4]
	ld [%i0+4],%o0
	sethi %hi(_const0_rtx),%o1
L270:
	ld [%o1+%lo(_const0_rtx)],%o1
	cmp %o0,%o1
	bne,a L271
	ld [%i0+4],%o1
	ldub [%i0+2],%o0
	call _force_const_mem,0
	mov %i0,%o1
	st %o0,[%i0+4]
	ld [%i0+4],%o1
L271:
	ldub [%i0+2],%o0
	call _memory_address_p,0
	ld [%o1+4],%o1
	cmp %o0,0
	bne,a L269
	ld [%i0+4],%i0
	ld [%i0+4],%o0
	ld [%o0+4],%o2
	ldub [%i0+2],%o1
	call _gen_rtx,0
	mov 37,%o0
	mov %o0,%i0
L269:
	ret
	restore
	.align 4
	.global _clear_const_double_mem
	.proc	020
_clear_const_double_mem:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	sethi %hi(_real_constant_chain),%g2
	ld [%g2+%lo(_real_constant_chain)],%g3
	cmp %g3,0
	be L279
	sethi %hi(_cc0_rtx),%g2
	ld [%g2+%lo(_cc0_rtx)],%i0
	ld [%g3+8],%g2
L283:
	st %g0,[%g3+8]
	st %i0,[%g3+4]
	orcc %g2,%g0,%g3
	bne,a L283
	ld [%g3+8],%g2
L279:
	sethi %hi(_real_constant_chain),%g2
	st %g0,[%g2+%lo(_real_constant_chain)]
	ret
	restore
	.align 4
	.proc	020
_decode_addr_const:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+20],%i0
	mov 0,%l1
L285:
	ldub [%i0+12],%o0
	cmp %o0,48
	bne L287
	cmp %o0,52
	ld [%i0+24],%o0
	b L302
	ld [%o0+44],%o0
L287:
	bne,a L286
	ldub [%i0+12],%o0
	ld [%i0+24],%l0
	ldub [%l0+12],%o0
	cmp %o0,35
	bne L292
	nop
	ld [%i0+8],%o1
	ld [%o1+24],%o2
	ldub [%o2+12],%o0
	cmp %o0,35
	be,a L291
	ldub [%o1+29],%o0
L292:
	call _abort,0
	nop
L291:
	call .umul,0
	ld [%o2+16],%o1
	call .umul,0
	ld [%l0+16],%o1
L302:
	cmp %o0,0
	bl,a L293
	add %o0,7,%o0
L293:
	ld [%i0+20],%i0
	sra %o0,3,%o0
	b L285
	add %l1,%o0,%l1
L286:
	cmp %o0,43
	be L296
	cmp %o0,39
	bne,a L295
	ld [%i0+12],%o1
L296:
	b L297
	ld [%i0+64],%o1
L295:
	sethi %hi(131072),%o0
	andcc %o1,%o0,%g0
	be L298
	nop
	b L297
	ld [%i0+16],%o1
L298:
	call _abort,0
	nop
L297:
	lduh [%o1],%o0
	cmp %o0,37
	be,a L300
	ld [%o1+4],%o1
	call _abort,0
	nop
L300:
	st %l1,[%i1+4]
	st %o1,[%i1]
	ret
	restore
	.align 4
	.global _const_hash
	.proc	04
_const_hash:
	!#PROLOGUE# 0
	save %sp,-120,%sp
	!#PROLOGUE# 1
	ldub [%i0+12],%o1
	cmp %o1,35
	bne L304
	cmp %o1,36
	add %i0,16,%l0
	b L305
	mov 8,%g2
L304:
	bne L306
	cmp %o1,38
	add %i0,24,%l0
	b L305
	mov 8,%g2
L306:
	bne L308
	cmp %o1,37
	ld [%i0+24],%l0
	b L305
	ld [%i0+20],%g2
L308:
	bne L310
	cmp %o1,53
	call _const_hash,0
	ld [%i0+20],%o0
	mov %o0,%l0
	call _const_hash,0
	ld [%i0+24],%o0
	b L348
	sll %l0,2,%i0
L310:
	bne L312
	cmp %o1,114
	ld [%i0+24],%l0
	cmp %l0,0
	be L327
	mov 5,%i0
L316:
	call _const_hash,0
	ld [%l0+20],%o0
	sll %i0,4,%o1
	add %o1,%i0,%o1
	sll %o1,2,%o1
	sub %o1,%i0,%o1
	sll %o1,3,%o2
	add %o1,%o2,%o1
	add %o1,%o0,%o1
	mov %o1,%o0
	call .rem,0
	mov 1007,%o1
	ld [%l0+4],%l0
	cmp %l0,0
	bne L316
	mov %o0,%i0
	b,a L327
L312:
	bne L318
	add %o1,-63,%o0
	mov %i0,%o0
	add %fp,-24,%l0
	call _decode_addr_const,0
	mov %l0,%o1
	b L305
	mov 8,%g2
L318:
	cmp %o0,1
	bgu L320
	add %o1,-110,%o0
	call _const_hash,0
	ld [%i0+20],%o0
	mov %o0,%l0
	call _const_hash,0
	ld [%i0+24],%o0
	sll %l0,3,%i0
L348:
	add %i0,%l0,%i0
	b L327
	add %i0,%o0,%i0
L320:
	cmp %o0,1
	bgu,a L349
	mov %g2,%i0
	call _const_hash,0
	ld [%i0+20],%o0
	sll %o0,3,%i0
	sub %i0,%o0,%i0
	b L327
	add %i0,2,%i0
L305:
	mov %g2,%i0
L349:
	mov 0,%o5
	cmp %o5,%i0
	bge L350
	sethi %hi(-1073741824),%o0
	andcc %i0,3,%o0
	be L351
	sll %i0,3,%o3
	cmp %o0,1
	ble L352
	cmp %o0,2
	ble L331
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	add %o3,%i0,%o3
	ldsb [%l0],%o4
	mov 1,%o5
	add %o3,%o4,%i0
L331:
	sll %i0,3,%o3
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	ldsb [%l0+%o5],%o4
	add %o3,%i0,%o3
	add %o5,1,%o5
	add %o3,%o4,%i0
	sll %i0,3,%o3
L352:
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	ldsb [%l0+%o5],%o4
	add %o3,%i0,%o3
	add %o5,1,%o5
	cmp %o5,%g2
	bge L324
	add %o3,%o4,%i0
L326:
	sll %i0,3,%o3
L351:
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	add %o3,%i0,%o3
	add %o5,1,%o0
	add %o5,2,%o1
	ldsb [%l0+%o5],%o4
	add %o5,3,%o2
	add %o5,4,%o5
	cmp %o5,%g2
	add %o3,%o4,%i0
	sll %i0,3,%o3
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	ldsb [%l0+%o0],%o4
	add %o3,%i0,%o3
	add %o3,%o4,%i0
	sll %i0,3,%o3
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	ldsb [%l0+%o1],%o4
	add %o3,%i0,%o3
	add %o3,%o4,%i0
	sll %i0,3,%o3
	add %o3,%i0,%o3
	sll %o3,4,%o4
	add %o3,%o4,%o3
	sll %o3,2,%o3
	ldsb [%l0+%o2],%o4
	add %o3,%i0,%o3
	bl L326
	add %o3,%o4,%i0
L324:
	sethi %hi(-1073741824),%o0
L350:
	andn %i0,%o0,%i0
	mov %i0,%o0
	call .rem,0
	mov 1007,%o1
	mov %o0,%i0
L327:
	ret
	restore
	.align 4
	.proc	0102
_compare_constant_1:
	!#PROLOGUE# 0
	save %sp,-128,%sp
	!#PROLOGUE# 1
	ldsb [%i1],%o0
	ldub [%i0+12],%o2
	cmp %o2,%o0
	bne L390
	add %i1,1,%i1
	cmp %o2,35
	bne L356
	cmp %o2,36
	add %i0,16,%l1
	b L357
	mov 8,%l0
L356:
	bne L358
	cmp %o2,38
	ld [%i0+8],%o0
	ldsb [%i1],%o1
	ldub [%o0+31],%o0
	cmp %o1,%o0
	bne L390
	add %i1,1,%i1
	add %i0,24,%l1
	b L357
	mov 8,%l0
L358:
	bne L361
	cmp %o2,37
	sethi %hi(_flag_writable_strings),%o0
	ld [%o0+%lo(_flag_writable_strings)],%o0
	cmp %o0,0
	bne,a L388
	mov 0,%i0
	add %i0,20,%o0
	ld [%i0+24],%l1
	mov %i1,%o1
	ld [%i0+20],%l0
	call _memcmp,0
	mov 4,%o2
	cmp %o0,0
	bne L388
	mov 0,%i0
	b L357
	add %i1,4,%i1
L361:
	bne L365
	cmp %o2,53
	ld [%i0+20],%o0
	call _compare_constant_1,0
	mov %i1,%o1
	orcc %o0,%g0,%i1
	be,a L388
	mov 0,%i0
	b L394
	ld [%i0+24],%o0
L365:
	bne L368
	cmp %o2,114
	call _list_length,0
	ld [%i0+24],%o0
	st %o0,[%fp-20]
	add %fp,-20,%o0
	mov %i1,%o1
	call _memcmp,0
	mov 4,%o2
	cmp %o0,0
	bne,a L388
	mov 0,%i0
	ld [%i0+24],%i0
	cmp %i0,0
	be L385
	add %i1,4,%i1
	ld [%i0+20],%o0
L395:
	call _compare_constant_1,0
	mov %i1,%o1
	orcc %o0,%g0,%i1
	be,a L388
	mov 0,%i0
	ld [%i0+4],%i0
	cmp %i0,0
	bne,a L395
	ld [%i0+20],%o0
	b L388
	mov %i1,%i0
L368:
	bne L376
	add %o2,-63,%o0
	mov %i0,%o0
	add %fp,-32,%l0
	call _decode_addr_const,0
	mov %l0,%o1
	mov %l0,%l1
	b L357
	mov 8,%l0
L376:
	cmp %o0,1
	bgu L378
	sll %o2,24,%o0
	ldsb [%i1],%o1
	sra %o0,24,%o0
	cmp %o1,%o0
	bne L390
	add %i1,1,%i1
	ld [%i0+20],%o0
	call _compare_constant_1,0
	mov %i1,%o1
	orcc %o0,%g0,%i1
	be,a L388
	mov 0,%i0
	b L394
	ld [%i0+24],%o0
L378:
	add %o2,-110,%o0
	cmp %o0,1
	bgu L357
	sll %o2,24,%o0
	ldsb [%i1],%o1
	sra %o0,24,%o0
	cmp %o1,%o0
	bne L390
	add %i1,1,%i1
	ld [%i0+20],%o0
L394:
	call _compare_constant_1,0
	mov %i1,%o1
	b L388
	mov %o0,%i0
L390:
	b L388
	mov 0,%i0
L357:
	addcc %l0,-1,%l0
	bneg L388
	mov %i1,%i0
	ldsb [%l1],%o1
L396:
	ldsb [%i1],%o0
	add %l1,1,%l1
	cmp %o0,%o1
	bne L390
	add %i1,1,%i1
	addcc %l0,-1,%l0
	bpos,a L396
	ldsb [%l1],%o1
L385:
	mov %i1,%i0
L388:
	ret
	restore
	.align 4
	.proc	0110
_record_constant:
	!#PROLOGUE# 0
	save %sp,-120,%sp
	!#PROLOGUE# 1
	sethi %hi(_permanent_obstack),%o0
	or %o0,%lo(_permanent_obstack),%l0
	st %g0,[%fp-20]
	ld [%l0+12],%o0
	ld [%l0+16],%o1
	add %o0,4,%o0
	cmp %o0,%o1
	bleu L399
	mov 4,%l1
	mov %l0,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L399:
	add %fp,-20,%o1
	ld [%l0+12],%o0
	call _memcpy,0
	mov %l1,%o2
	ld [%l0+12],%o0
	add %o0,%l1,%o0
	st %o0,[%l0+12]
	sethi %hi(_permanent_obstack),%o0
	or %o0,%lo(_permanent_obstack),%l0
	ld [%l0+12],%o0
	ld [%l0+16],%o1
	add %o0,4,%o0
	cmp %o0,%o1
	bleu L401
	mov 4,%l1
	mov %l0,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L401:
	add %fp,-24,%o1
	ld [%l0+12],%o0
	call _memcpy,0
	mov %l1,%o2
	ld [%l0+12],%o1
	mov %i0,%o0
	add %o1,%l1,%o1
	call _record_constant_1,0
	st %o1,[%l0+12]
	sethi %hi(_permanent_obstack),%o0
	or %o0,%lo(_permanent_obstack),%o2
	ld [%o2+12],%o0
	ld [%o2+24],%o1
	ld [%o2+16],%o3
	ld [%o2+8],%i0
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o2+4],%o0
	st %o1,[%o2+12]
	sub %o1,%o0,%o1
	sub %o3,%o0,%o0
	cmp %o1,%o0
	bg,a L403
	st %o3,[%o2+12]
L403:
	ld [%o2+12],%o0
	st %o0,[%o2+8]
	ret
	restore
	.align 4
	.proc	020
_record_constant_1:
	!#PROLOGUE# 0
	save %sp,-128,%sp
	!#PROLOGUE# 1
	sethi %hi(_permanent_obstack),%o0
	or %o0,%lo(_permanent_obstack),%l1
	ld [%l1+12],%o0
	ld [%l1+16],%o1
	add %o0,1,%o0
	cmp %o0,%o1
	bleu L406
	ldub [%i0+12],%l0
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 1,%o1
L406:
	ld [%l1+12],%o1
	cmp %l0,35
	mov %l0,%l2
	add %o1,1,%o0
	st %o0,[%l1+12]
	bne L407
	stb %l2,[%o1]
	add %i0,16,%l4
	b L408
	mov 8,%l3
L407:
	cmp %l0,36
	bne L409
	cmp %l0,38
	ld [%l1+12],%o0
	ld [%l1+16],%o1
	add %o0,1,%o0
	cmp %o0,%o1
	bleu L411
	add %i0,24,%l4
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 1,%o1
L411:
	ld [%l1+12],%o1
	add %o1,1,%o0
	st %o0,[%l1+12]
	ld [%i0+8],%o0
	ldub [%o0+31],%o0
	mov 8,%l3
	b L408
	stb %o0,[%o1]
L409:
	bne L413
	cmp %l0,37
	sethi %hi(_flag_writable_strings),%o0
	ld [%o0+%lo(_flag_writable_strings)],%o0
	cmp %o0,0
	bne L404
	mov %l1,%l0
	ld [%i0+24],%l4
	ld [%i0+20],%l3
	ld [%l1+12],%o0
	ld [%l1+16],%o1
	add %o0,4,%o0
	cmp %o0,%o1
	bleu L416
	mov 4,%l2
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L416:
	add %i0,20,%o1
	ld [%l0+12],%o0
	call _memcpy,0
	mov %l2,%o2
	ld [%l0+12],%o0
	add %o0,%l2,%o0
	b L408
	st %o0,[%l0+12]
L413:
	be L440
	cmp %l0,53
	bne L420
	cmp %l0,114
	call _list_length,0
	ld [%i0+24],%o0
	st %o0,[%fp-20]
	mov %l1,%l0
	ld [%l1+12],%o1
	ld [%l1+16],%o0
	add %o1,4,%o1
	cmp %o1,%o0
	bleu L422
	mov 4,%l2
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L422:
	add %fp,-20,%o1
	ld [%l0+12],%o0
	call _memcpy,0
	mov %l2,%o2
	ld [%l0+12],%o0
	add %o0,%l2,%o0
	st %o0,[%l0+12]
	ld [%i0+24],%i0
	cmp %i0,0
	be L404
	nop
L426:
	call _record_constant_1,0
	ld [%i0+20],%o0
	ld [%i0+4],%i0
	cmp %i0,0
	be L404
	nop
	b,a L426
L420:
	bne L428
	add %l0,-63,%o0
	mov %i0,%o0
	add %fp,-32,%l0
	call _decode_addr_const,0
	mov %l0,%o1
	mov %l0,%l4
	b L408
	mov 8,%l3
L428:
	cmp %o0,1
	bgu L430
	add %l0,-110,%o0
	ld [%l1+12],%o0
	ld [%l1+16],%o1
	add %o0,1,%o0
	cmp %o0,%o1
	bleu L432
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 1,%o1
L432:
	ld [%l1+12],%o1
	add %o1,1,%o0
	st %o0,[%l1+12]
	stb %l2,[%o1]
L440:
	call _record_constant_1,0
	ld [%i0+20],%o0
	call _record_constant_1,0
	ld [%i0+24],%o0
	b,a L404
L430:
	cmp %o0,1
	bgu L441
	sethi %hi(_permanent_obstack),%o0
	ld [%l1+12],%o0
	ld [%l1+16],%o1
	add %o0,1,%o0
	cmp %o0,%o1
	bleu L436
	mov %l1,%o0
	call __obstack_newchunk,0
	mov 1,%o1
L436:
	ld [%l1+12],%o1
	add %o1,1,%o0
	st %o0,[%l1+12]
	stb %l2,[%o1]
	call _record_constant_1,0
	ld [%i0+20],%o0
	b,a L404
L408:
	sethi %hi(_permanent_obstack),%o0
L441:
	or %o0,%lo(_permanent_obstack),%i0
	ld [%i0+12],%o0
	mov %l3,%l1
	ld [%i0+16],%o1
	add %o0,%l1,%o0
	cmp %o0,%o1
	bleu L438
	mov %i0,%o0
	call __obstack_newchunk,0
	mov %l1,%o1
L438:
	mov %l4,%o1
	ld [%i0+12],%o0
	call _memcpy,0
	mov %l1,%o2
	ld [%i0+12],%o0
	add %o0,%l1,%o0
	st %o0,[%i0+12]
L404:
	ret
	restore
	.align 8
LC30:
	.ascii "%s%d:\12\0"
	.align 8
LC31:
	.ascii "LC\0"
	.align 4
	.proc	0102
_get_or_assign_label:
	!#PROLOGUE# 0
	save %sp,-128,%sp
	!#PROLOGUE# 1
	call _output_addressed_constants,0
	mov %i0,%o0
	call _const_hash,0
	mov %i0,%o0
	call .rem,0
	mov 1007,%o1
	mov %o0,%l0
	sethi %hi(_const_hash_table),%o0
	or %o0,%lo(_const_hash_table),%o0
	sll %l0,2,%o1
	ld [%o1+%o0],%l1
	cmp %l1,0
	be L444
	mov %i0,%o0
L474:
	call _compare_constant_1,0
	add %l1,8,%o1
	cmp %o0,0
	bne,a L470
	ld [%l1+4],%i0
	ld [%l1],%l1
	cmp %l1,0
	bne L474
	mov %i0,%o0
L444:
	call _record_constant,0
	mov %i0,%o0
	sethi %hi(_const_hash_table),%o1
	or %o1,%lo(_const_hash_table),%o1
	sll %l0,2,%o3
	ld [%o3+%o1],%o2
	mov %o0,%l1
	st %o2,[%l1]
	st %l1,[%o3+%o1]
	ldub [%i0+12],%o0
	cmp %o0,38
	bne,a L475
	sethi %hi(_in_section),%l0
	sethi %hi(_flag_writable_strings),%o0
	ld [%o0+%lo(_flag_writable_strings)],%o0
	cmp %o0,0
	be L449
	sethi %hi(_in_section),%o0
	ld [%o0+%lo(_in_section)],%o0
	cmp %o0,2
	be L454
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC2),%o2
	call _fprintf,0
	or %o2,%lo(LC2),%o2
	mov 2,%o0
	sethi %hi(_in_section),%o1
	b L454
	st %o0,[%o1+%lo(_in_section)]
L449:
	sethi %hi(_in_section),%l0
L475:
	ld [%l0+%lo(_in_section)],%o0
	cmp %o0,1
	be L454
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC1),%o2
	call _fprintf,0
	or %o2,%lo(LC1),%o2
	mov 1,%o0
	st %o0,[%l0+%lo(_in_section)]
L454:
	ld [%i0+8],%o0
	ldub [%o0+30],%o0
	cmp %o0,16
	bl L458
	mov 0,%o1
	mov %o0,%o2
	mov 8,%o3
	add %o1,1,%o1
L476:
	add %o1,1,%o0
	sll %o3,%o0,%o0
	cmp %o2,%o0
	bge,a L476
	add %o1,1,%o1
L458:
	cmp %o1,1
	bne L461
	cmp %o1,0
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC13),%o1
	call _fprintf,0
	or %o1,%lo(LC13),%o1
	b L477
	sethi %hi(LC30),%o1
L461:
	be L477
	sethi %hi(LC30),%o1
	call _abort,0
	nop
L477:
	or %o1,%lo(LC30),%o1
	sethi %hi(_const_labelno),%o2
	ld [%o2+%lo(_const_labelno)],%o3
	sethi %hi(_asm_out_file),%o0
	sethi %hi(LC31),%o2
	ld [%o0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o2,%lo(LC31),%o2
	ldub [%i0+12],%o0
	cmp %o0,38
	bne L464
	nop
	b L465
	ld [%i0+20],%o1
L464:
	call _int_size_in_bytes,0
	ld [%i0+8],%o0
	mov %o0,%o1
L465:
	call _output_constant,0
	mov %i0,%o0
	add %fp,-32,%l2
	mov %l2,%o0
	sethi %hi(LC28),%o1
	or %o1,%lo(LC28),%o1
	sethi %hi(_const_labelno),%l0
	sethi %hi(LC31),%o2
	ld [%l0+%lo(_const_labelno)],%o3
	call _sprintf,0
	or %o2,%lo(LC31),%o2
	ld [%l0+%lo(_const_labelno)],%o1
	mov %l2,%o0
	add %o1,1,%o1
	st %o1,[%l0+%lo(_const_labelno)]
	sethi %hi(_permanent_obstack),%o1
	call _strlen,0
	or %o1,%lo(_permanent_obstack),%i0
	ld [%i0+12],%o1
	mov %o0,%l0
	ld [%i0+16],%o0
	add %o1,%l0,%o1
	add %o1,1,%o1
	cmp %o1,%o0
	bleu L467
	mov %i0,%o0
	call __obstack_newchunk,0
	add %l0,1,%o1
L467:
	mov %l2,%o1
	ld [%i0+12],%o0
	call _memcpy,0
	mov %l0,%o2
	ld [%i0+12],%o0
	mov %i0,%o2
	add %o0,%l0,%o0
	add %o0,1,%o1
	st %o1,[%i0+12]
	stb %g0,[%o0]
	ld [%o2+12],%o0
	ld [%o2+24],%o1
	ld [%o2+16],%o3
	ld [%o2+8],%i0
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o2+4],%o0
	st %o1,[%o2+12]
	sub %o1,%o0,%o1
	sub %o3,%o0,%o0
	cmp %o1,%o0
	bg,a L469
	st %o3,[%o2+12]
	b L478
	ld [%o2+12],%o0
L469:
	ld [%o2+12],%o0
L478:
	st %o0,[%o2+8]
	st %i0,[%l1+4]
L470:
	ret
	restore
	.align 4
	.global _output_constant_def
	.proc	0110
_output_constant_def:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	call _allocation_temporary_p,0
	mov %i0,%l0
	ldub [%l0+12],%o1
	cmp %o1,35
	bne L485
	mov %o0,%l1
	call _abort,0
	nop
L485:
	ld [%l0+16],%i0
	cmp %i0,0
	bne L489
	nop
	ld [%l0+12],%o0
	sethi %hi(32768),%i0
	andcc %o0,%i0,%g0
	be L487
	nop
	call _end_temporary_allocation,0
	nop
L487:
	call _get_or_assign_label,0
	mov %l0,%o0
	mov %o0,%o2
	mov 39,%o0
	call _gen_rtx,0
	mov 4,%o1
	ld [%l0+8],%o1
	mov %o0,%o2
	ldub [%o1+28],%o1
	call _gen_rtx,0
	mov 37,%o0
	st %o0,[%l0+16]
	ld [%o0],%o1
	cmp %l1,0
	or %o1,32,%o1
	be L488
	st %o1,[%o0]
	ld [%l0+12],%o0
	andcc %o0,%i0,%g0
	be,a L489
	ld [%l0+16],%i0
	call _resume_temporary_allocation,0
	nop
L488:
	ld [%l0+16],%i0
L489:
	ret
	restore
	.align 4
	.global _init_const_rtx_hash_table
	.proc	020
_init_const_rtx_hash_table:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	sethi %hi(_const_rtx_hash_table),%o0
	or %o0,%lo(_const_rtx_hash_table),%o0
	mov 0,%o1
	call _memset,0
	mov 244,%o2
	ret
	restore
	.align 4
	.proc	020
_decode_rtx_const:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	add %i2,16,%o1
	cmp %i2,%o1
	bgeu L494
	mov %i2,%o0
	st %g0,[%o0]
L517:
	add %o0,4,%o0
	cmp %o0,%o1
	blu,a L517
	st %g0,[%o0]
L494:
	mov 1,%o0
	sth %o0,[%i2]
	sth %i0,[%i2+2]
	lduh [%i1],%o1
	sethi %hi(65506),%o0
	or %o0,%lo(65506),%o0
	add %o1,%o0,%o1
	sll %o1,16,%o1
	srl %o1,16,%o1
	cmp %o1,9
	bgu L508
	sethi %hi(L509),%o0
	or %o0,%lo(L509),%o0
	sll %o1,2,%o1
	ld [%o1+%o0],%o0
	jmp %o0
	nop
L509:
	.word	L498
	.word	L497
	.word	L501
	.word	L508
	.word	L508
	.word	L508
	.word	L508
	.word	L508
	.word	L500
	.word	L499
L497:
	sth %g0,[%i2]
	ldub [%i1+2],%o0
	sth %o0,[%i2+2]
	ld [%i1+12],%o0
	st %o0,[%i2+8]
	ld [%i1+16],%o0
	b L496
	st %o0,[%i2+12]
L498:
	ld [%i1+4],%o0
	b L496
	st %o0,[%i2+12]
L499:
L500:
	b L496
	st %i1,[%i2+8]
L501:
	ld [%i1+4],%i1
	lduh [%i1],%o0
	cmp %o0,44
	bne L502
	cmp %o0,45
	ld [%i1+4],%o0
	ld [%o0+4],%o0
	st %o0,[%i2+8]
	ld [%i1+8],%o1
	lduh [%o1],%o0
	cmp %o0,30
	be,a L503
	ld [%o1+4],%o0
	call _abort,0
	nop
L503:
	b L496
	st %o0,[%i2+12]
L502:
	bne L505
	nop
	ld [%i1+4],%o0
	st %o0,[%i2+8]
	ld [%i1+8],%o1
	lduh [%o1],%o0
	cmp %o0,30
	be,a L506
	ld [%o1+4],%o0
	call _abort,0
	nop
L506:
	sub %g0,%o0,%o0
	b L496
	st %o0,[%i2+12]
L505:
	call _abort,0
	nop
L508:
	call _abort,0
	nop
L496:
	lduh [%i2],%o0
	cmp %o0,1
	bne L510
	nop
	ld [%i2+8],%o0
	cmp %o0,0
	be L510
	nop
	lduh [%o0],%o1
	cmp %o1,39
	bgu L510
	cmp %o1,38
	blu L510
	nop
	ld [%o0+4],%o0
	st %o0,[%i2+8]
L510:
	ret
	restore
	.align 4
	.global _const_hash_rtx
	.proc	04
_const_hash_rtx:
	!#PROLOGUE# 0
	save %sp,-128,%sp
	!#PROLOGUE# 1
	mov %i0,%o0
	mov %i1,%o1
	call _decode_rtx_const,0
	add %fp,-32,%o2
	ld [%fp-32],%i0
	ld [%fp-28],%o0
	add %i0,%o0,%i0
	ld [%fp-24],%o0
	add %i0,%o0,%i0
	ld [%fp-20],%o0
	mov 61,%o1
	add %i0,%o0,%i0
	sethi %hi(-1073741824),%o0
	andn %i0,%o0,%i0
	call .rem,0
	mov %i0,%o0
	ret
	restore %g0,%o0,%o0
	.align 4
	.proc	0110
_record_constant_rtx:
	!#PROLOGUE# 0
	save %sp,-136,%sp
	!#PROLOGUE# 1
	st %g0,[%fp-36]
	mov %i0,%o0
	mov %i1,%o1
	call _decode_rtx_const,0
	add %fp,-32,%o2
	sethi %hi(_saveable_obstack),%o0
	ld [%o0+%lo(_saveable_obstack)],%i0
	ld [%i0+12],%o0
	ld [%i0+16],%o1
	add %o0,4,%o0
	cmp %o0,%o1
	bleu L543
	mov 4,%i1
	mov %i0,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L543:
	add %fp,-36,%o1
	ld [%i0+12],%o0
	call _memcpy,0
	mov %i1,%o2
	ld [%i0+12],%o0
	sethi %hi(_saveable_obstack),%o1
	add %o0,%i1,%o0
	ld [%o1+%lo(_saveable_obstack)],%i1
	st %o0,[%i0+12]
	ld [%i1+12],%o0
	ld [%i1+16],%o1
	add %o0,4,%o0
	cmp %o0,%o1
	bleu L545
	mov 4,%i0
	mov %i1,%o0
	call __obstack_newchunk,0
	mov 4,%o1
L545:
	add %fp,-40,%o1
	ld [%i1+12],%o0
	call _memcpy,0
	mov %i0,%o2
	ld [%i1+12],%o0
	sethi %hi(_saveable_obstack),%o1
	add %o0,%i0,%o0
	ld [%o1+%lo(_saveable_obstack)],%i0
	st %o0,[%i1+12]
	ld [%i0+12],%o0
	ld [%i0+16],%o1
	add %o0,16,%o0
	cmp %o0,%o1
	bleu L547
	mov 16,%i1
	mov %i0,%o0
	call __obstack_newchunk,0
	mov 16,%o1
L547:
	add %fp,-32,%o1
	ld [%i0+12],%o0
	call _memcpy,0
	mov %i1,%o2
	ld [%i0+12],%o0
	sethi %hi(_saveable_obstack),%o1
	ld [%o1+%lo(_saveable_obstack)],%o2
	add %o0,%i1,%o0
	st %o0,[%i0+12]
	ld [%o2+12],%o0
	ld [%o2+24],%o1
	ld [%o2+16],%o3
	ld [%o2+8],%i0
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o2+4],%o0
	st %o1,[%o2+12]
	sub %o1,%o0,%o1
	sub %o3,%o0,%o0
	cmp %o1,%o0
	bg,a L549
	st %o3,[%o2+12]
L549:
	ld [%o2+12],%o0
	st %o0,[%o2+8]
	ret
	restore
	.align 8
LC32:
	.ascii "\11.double 0r%.20e\12\0"
	.align 8
LC33:
	.ascii "\11.single 0r%.20e\12\0"
	.align 8
LC34:
	.ascii "\11.word \0"
	.align 8
LC35:
	.ascii "\11.byte \0"
	.align 4
	.global _force_const_mem
	.proc	0110
_force_const_mem:
	!#PROLOGUE# 0
	save %sp,-152,%sp
	!#PROLOGUE# 1
	mov %i0,%l4
	lduh [%i1],%o0
	cmp %o0,31
	bne L551
	mov 0,%l0
	ld [%i1+4],%i0
	lduh [%i0],%o0
	cmp %o0,37
	be L598
	nop
L551:
	mov %l4,%o0
	mov %i1,%o1
	call _decode_rtx_const,0
	add %fp,-48,%o2
	ld [%fp-48],%o0
	ld [%fp-44],%o2
	add %o0,%o2,%o0
	ld [%fp-40],%o2
	add %o0,%o2,%o0
	ld [%fp-36],%o2
	mov 61,%o1
	add %o0,%o2,%o0
	sethi %hi(-1073741824),%o2
	call .rem,0
	andn %o0,%o2,%o0
	mov %o0,%l1
	sethi %hi(_const_rtx_hash_table),%o0
	or %o0,%lo(_const_rtx_hash_table),%o0
	sll %l1,2,%o1
	ld [%o1+%o0],%l2
	cmp %l2,0
	be L617
	cmp %l0,0
	mov %l4,%o0
L618:
	mov %i1,%o1
	call _decode_rtx_const,0
	add %fp,-48,%o2
	ld [%l2+8],%o1
	ld [%fp-48],%o0
	cmp %o1,%o0
	bne L599
	ld [%fp-44],%o0
	ld [%l2+12],%o1
	cmp %o1,%o0
	bne L599
	ld [%fp-40],%o0
	ld [%l2+16],%o1
	cmp %o1,%o0
	bne L599
	ld [%fp-36],%o0
	ld [%l2+20],%o1
	cmp %o1,%o0
	bne L599
	mov 1,%o0
L561:
	cmp %o0,0
	be,a L559
	ld [%l2],%l2
	b L558
	ld [%l2+4],%l0
L599:
	b L561
	mov 0,%o0
L559:
	cmp %l2,0
	bne L618
	mov %l4,%o0
L558:
	cmp %l0,0
L617:
	bne L619
	mov 39,%o0
	mov %l4,%o0
	call _record_constant_rtx,0
	mov %i1,%o1
	sethi %hi(_const_rtx_hash_table),%o1
	or %o1,%lo(_const_rtx_hash_table),%o1
	sll %l1,2,%o3
	mov %o0,%l2
	ld [%o3+%o1],%o2
	sethi %hi(_in_section),%l0
	ld [%l0+%lo(_in_section)],%o0
	st %o2,[%l2]
	cmp %o0,1
	be L568
	st %l2,[%o3+%o1]
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC0),%o1
	or %o1,%lo(LC0),%o1
	sethi %hi(LC1),%o2
	call _fprintf,0
	or %o2,%lo(LC1),%o2
	mov 1,%o0
	st %o0,[%l0+%lo(_in_section)]
L568:
	cmp %l4,0
	be L570
	sethi %hi(_mode_size),%o0
	or %o0,%lo(_mode_size),%o0
	sll %l4,2,%o1
	b L571
	ld [%o1+%o0],%l0
L570:
	mov 4,%l0
L571:
	cmp %l0,2
	bg,a L572
	mov 2,%l0
L572:
	call _exact_log2,0
	mov %l0,%o0
	cmp %o0,1
	bne L573
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC13),%o1
	call _fprintf,0
	or %o1,%lo(LC13),%o1
	b L620
	sethi %hi(LC30),%o1
L573:
	call _exact_log2,0
	mov %l0,%o0
	cmp %o0,0
	be L620
	sethi %hi(LC30),%o1
	call _abort,0
	nop
L620:
	or %o1,%lo(LC30),%o1
	sethi %hi(_const_labelno),%o2
	ld [%o2+%lo(_const_labelno)],%o3
	sethi %hi(_asm_out_file),%l3
	sethi %hi(LC31),%o2
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o2,%lo(LC31),%o2
	lduh [%i1],%o0
	cmp %o0,31
	bne L576
	cmp %l4,2
	ld [%i1+12],%o0
	st %o0,[%fp-56]
	ld [%i1+16],%o0
	cmp %l4,10
	be L580
	st %o0,[%fp-52]
	cmp %l4,10
	bgu L583
	cmp %l4,6
	be L578
	sethi %hi(LC17),%l1
	b L621
	add %fp,-32,%l3
L583:
	cmp %l4,11
	be L579
	ld [%l3+%lo(_asm_out_file)],%o0
	b L621
	add %fp,-32,%l3
L578:
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l1,%lo(LC17),%o1
	mov 30,%o0
	ld [%fp-56],%o2
	call _gen_rtx,0
	mov 0,%o1
	mov %o0,%o1
	call _output_addr_const,0
	ld [%l3+%lo(_asm_out_file)],%o0
	sethi %hi(LC15),%l0
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l0,%lo(LC15),%o1
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l1,%lo(LC17),%o1
	mov 30,%o0
	ld [%fp-52],%o2
	call _gen_rtx,0
	mov 0,%o1
	mov %o0,%o1
	call _output_addr_const,0
	ld [%l3+%lo(_asm_out_file)],%o0
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l0,%lo(LC15),%o1
	b L621
	add %fp,-32,%l3
L579:
	sethi %hi(LC32),%o1
	ldd [%fp-56],%o2
	call _fprintf,0
	or %o1,%lo(LC32),%o1
	b L621
	add %fp,-32,%l3
L580:
	ld [%l3+%lo(_asm_out_file)],%o0
	sethi %hi(LC33),%o1
	ldd [%fp-56],%o2
	call _fprintf,0
	or %o1,%lo(LC33),%o1
	b L621
	add %fp,-32,%l3
L576:
	be L587
	cmp %l4,2
	bgu L591
	cmp %l4,1
	be L588
	sethi %hi(LC35),%o1
	b L621
	add %fp,-32,%l3
L591:
	cmp %l4,4
	bne,a L621
	add %fp,-32,%l3
	sethi %hi(LC17),%o1
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC17),%o1
	b L623
	ld [%l3+%lo(_asm_out_file)],%o0
L587:
	sethi %hi(LC34),%o1
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC34),%o1
	b L623
	ld [%l3+%lo(_asm_out_file)],%o0
L588:
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC35),%o1
	ld [%l3+%lo(_asm_out_file)],%o0
L623:
	call _output_addr_const,0
	mov %i1,%o1
	sethi %hi(LC15),%o1
	ld [%l3+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC15),%o1
	add %fp,-32,%l3
L621:
	mov %l3,%o0
	sethi %hi(LC28),%o1
	or %o1,%lo(LC28),%o1
	sethi %hi(_const_labelno),%l0
	sethi %hi(LC31),%o2
	ld [%l0+%lo(_const_labelno)],%o3
	call _sprintf,0
	or %o2,%lo(LC31),%o2
	ld [%l0+%lo(_const_labelno)],%o1
	mov %l3,%o0
	add %o1,1,%o1
	st %o1,[%l0+%lo(_const_labelno)]
	sethi %hi(_permanent_obstack),%o1
	call _strlen,0
	or %o1,%lo(_permanent_obstack),%l1
	ld [%l1+12],%o1
	mov %o0,%l0
	ld [%l1+16],%o0
	add %o1,%l0,%o1
	add %o1,1,%o1
	cmp %o1,%o0
	bleu L593
	mov %l1,%o0
	call __obstack_newchunk,0
	add %l0,1,%o1
L593:
	mov %l3,%o1
	ld [%l1+12],%o0
	call _memcpy,0
	mov %l0,%o2
	ld [%l1+12],%o0
	mov %l1,%o2
	add %o0,%l0,%o0
	add %o0,1,%o1
	st %o1,[%l1+12]
	stb %g0,[%o0]
	ld [%o2+12],%o0
	ld [%o2+24],%o1
	ld [%o2+16],%o3
	ld [%o2+8],%o4
	add %o0,%o1,%o0
	andn %o0,%o1,%o1
	ld [%o2+4],%o0
	st %o1,[%o2+12]
	sub %o1,%o0,%o1
	sub %o3,%o0,%o0
	cmp %o1,%o0
	bg,a L595
	st %o3,[%o2+12]
L595:
	ld [%l1+12],%o0
	st %o0,[%l1+8]
	st %o4,[%l2+4]
	mov 39,%o0
L619:
	ld [%l2+4],%o2
	call _gen_rtx,0
	mov 4,%o1
	mov %o0,%o2
	mov 37,%o0
	call _gen_rtx,0
	mov %l4,%o1
	mov %o0,%i0
	ld [%i0],%o0
	ld [%i0+4],%o1
	or %o0,32,%o0
	st %o0,[%i0]
	ld [%o1],%o0
	or %o0,32,%o0
	st %o0,[%o1]
	lduh [%i1],%o0
	cmp %o0,31
	bne L598
	sethi %hi(_cc0_rtx),%o1
	ld [%i1+4],%o0
	ld [%o1+%lo(_cc0_rtx)],%o1
	cmp %o0,%o1
	bne,a L598
	st %i0,[%i1+4]
	sethi %hi(_real_constant_chain),%o0
	ld [%o0+%lo(_real_constant_chain)],%o1
	st %i1,[%o0+%lo(_real_constant_chain)]
	st %o1,[%i1+8]
	st %i0,[%i1+4]
L598:
	ret
	restore
	.align 4
	.global _output_addressed_constants
	.proc	020
_output_addressed_constants:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ldub [%i0+12],%o0
	cmp %o0,64
	bgu L649
	cmp %o0,63
	bgeu L637
	cmp %o0,0
	be L625
	cmp %o0,53
	be,a L640
	ld [%i0+24],%i0
	b L652
	ld [%i0+12],%o1
L649:
	cmp %o0,110
	blu L646
	cmp %o0,111
	bleu L639
	cmp %o0,114
	bne,a L652
	ld [%i0+12],%o1
	ld [%i0+20],%i0
L653:
	ldub [%i0+12],%o0
	cmp %o0,48
	be,a L653
	ld [%i0+20],%i0
	ld [%i0+12],%o1
	sethi %hi(131072),%o0
	andcc %o1,%o0,%g0
	be L625
	nop
	call _allocation_temporary_p,0
	nop
	ldub [%i0+12],%o1
	cmp %o1,35
	bne L632
	mov %o0,%l0
	call _abort,0
	nop
L632:
	ld [%i0+16],%o0
	cmp %o0,0
	bne L625
	sethi %hi(32768),%l1
	ld [%i0+12],%o0
	andcc %o0,%l1,%g0
	be L634
	nop
	call _end_temporary_allocation,0
	nop
L634:
	call _get_or_assign_label,0
	mov %i0,%o0
	mov %o0,%o2
	mov 39,%o0
	call _gen_rtx,0
	mov 4,%o1
	ld [%i0+8],%o1
	mov %o0,%o2
	ldub [%o1+28],%o1
	call _gen_rtx,0
	mov 37,%o0
	st %o0,[%i0+16]
	ld [%o0],%o1
	cmp %l0,0
	or %o1,32,%o1
	be L625
	st %o1,[%o0]
	ld [%i0+12],%o0
	andcc %o0,%l1,%g0
	be L625
	nop
	call _resume_temporary_allocation,0
	add %o7,(L625-.-4),%o7
L637:
	call _output_addressed_constants,0
	ld [%i0+20],%o0
	call _output_addressed_constants,0
	ld [%i0+24],%o0
	b,a L625
L639:
	call _output_addressed_constants,0
	ld [%i0+20],%o0
	b,a L625
L640:
	cmp %i0,0
	be L625
	nop
L644:
	call _output_addressed_constants,0
	ld [%i0+20],%o0
	ld [%i0+4],%i0
	cmp %i0,0
	be L625
	nop
	b,a L644
L646:
	ld [%i0+12],%o1
L652:
	sethi %hi(131072),%o0
	andcc %o1,%o0,%g0
	bne L625
	nop
	call _abort,0
	nop
L625:
	ret
	restore
	.align 8
LC36:
	.ascii "8-byte integer constant expression too complicated\0"
	.align 8
LC37:
	.ascii "initializer for floating value is not a floating constant\0"
	.align 4
	.global _output_constant
	.proc	020
_output_constant:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	ld [%i0+8],%o3
	cmp %i1,0
	be L654
	ldub [%o3+12],%o1
	ldub [%i0+12],%o0
	cmp %o0,111
	bne L656
	add %o1,-5,%o1
	ld [%i0+20],%o2
	ld [%o2+8],%o0
	cmp %o3,%o0
	be,a L656
	mov %o2,%i0
L656:
	cmp %o1,15
	bgu L657
	sethi %hi(L691),%o0
	or %o0,%lo(L691),%o0
	sll %o1,2,%o1
	ld [%o1+%o0],%o0
	jmp %o0
	nop
L691:
	.word	L695
	.word	L674
	.word	L680
	.word	L695
	.word	L657
	.word	L657
	.word	L695
	.word	L657
	.word	L695
	.word	L657
	.word	L657
	.word	L681
	.word	L657
	.word	L657
	.word	L688
	.word	L688
L695:
	ldub [%i0+12],%o0
	add %o0,-110,%o0
	and %o0,0xff,%o0
	cmp %o0,1
	bleu,a L695
	ld [%i0+20],%i0
	ld [%i0+8],%o0
	ldub [%o0+28],%o0
	cmp %o0,6
	bne L665
	mov %i0,%o0
	ldub [%i0+12],%o0
	cmp %o0,35
	bne L666
	sethi %hi(LC36),%o0
	sethi %hi(LC17),%l2
	sethi %hi(_asm_out_file),%l0
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l2,%lo(LC17),%o1
	mov 30,%o0
	ld [%i0+16],%o2
	call _gen_rtx,0
	mov 0,%o1
	mov %o0,%o1
	call _output_addr_const,0
	ld [%l0+%lo(_asm_out_file)],%o0
	sethi %hi(LC15),%l1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l1,%lo(LC15),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l2,%lo(LC17),%o1
	mov 30,%o0
	ld [%i0+20],%o2
	call _gen_rtx,0
	mov 0,%o1
	mov %o0,%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _output_addr_const,0
	add %i1,-8,%i1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %l1,%lo(LC15),%o1
	b L698
	cmp %i1,0
L666:
	call _error,0
	or %o0,%lo(LC36),%o0
	b L698
	cmp %i1,0
L665:
	mov 0,%o1
	mov 0,%o2
	call _expand_expr,0
	mov 1,%o3
	cmp %i1,1
	bne L668
	mov %o0,%l1
	sethi %hi(LC35),%o1
	b L696
	or %o1,%lo(LC35),%o1
L668:
	cmp %i1,2
	bne L670
	cmp %i1,4
	sethi %hi(LC34),%o1
	b L696
	or %o1,%lo(LC34),%o1
L670:
	bne L672
	sethi %hi(LC17),%o1
	or %o1,%lo(LC17),%o1
L696:
	sethi %hi(_asm_out_file),%l0
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	mov 0,%i1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _output_addr_const,0
	mov %l1,%o1
	sethi %hi(LC15),%o1
	ld [%l0+%lo(_asm_out_file)],%o0
	call _fprintf,0
	or %o1,%lo(LC15),%o1
	b L698
	cmp %i1,0
L672:
	call _abort,0
	nop
L674:
	ldub [%i0+12],%o0
	cmp %o0,36
	be L699
	cmp %i1,3
	sethi %hi(LC37),%o0
	call _error,0
	or %o0,%lo(LC37),%o0
	cmp %i1,3
L699:
	ble L657
	cmp %i1,7
	bg L678
	sethi %hi(_asm_out_file),%o0
	sethi %hi(LC33),%o1
	ld [%o0+%lo(_asm_out_file)],%o0
	or %o1,%lo(LC33),%o1
	ldd [%i0+24],%o2
	call _fprintf,0
	add %i1,-4,%i1
	b L698
	cmp %i1,0
L678:
	sethi %hi(LC32),%o1
	ld [%o0+%lo(_asm_out_file)],%o0
	or %o1,%lo(LC32),%o1
	ldd [%i0+24],%o2
	call _fprintf,0
	add %i1,-8,%i1
	b L698
	cmp %i1,0
L680:
	srl %i1,31,%l0
	add %i1,%l0,%l0
	sra %l0,1,%l0
	ld [%i0+20],%o0
	call _output_constant,0
	mov %l0,%o1
	ld [%i0+24],%o0
	call _output_constant,0
	mov %l0,%o1
	sll %l0,1,%l0
	b L657
	sub %i1,%l0,%i1
L681:
	ldub [%i0+12],%o0
	cmp %o0,53
	be L697
	cmp %o0,38
	bne L684
	nop
	ld [%i0+20],%o0
	cmp %i1,%o0
	ble L685
	mov 0,%l0
	sub %i1,%o0,%l0
	mov %o0,%i1
L685:
	ld [%i0+24],%o0
	mov %i1,%o1
	call _assemble_string,0
	mov %l0,%i1
	b L698
	cmp %i1,0
L684:
	call _abort,0
	nop
L688:
	ldub [%i0+12],%o0
	cmp %o0,53
	bne L689
	nop
L697:
	mov %i0,%o0
	call _output_constructor,0
	mov %i1,%o1
	b,a L654
L689:
	call _abort,0
	nop
L657:
	cmp %i1,0
L698:
	ble L654
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC26),%o1
	or %o1,%lo(LC26),%o1
	call _fprintf,0
	mov %i1,%o2
L654:
	ret
	restore
	.align 8
LC38:
	.ascii "\11.byte 0x%x\12\0"
	.align 8
LC39:
	.ascii "invalid initial value for member `%s'\0"
	.align 4
	.global _output_constructor
	.proc	020
_output_constructor:
	!#PROLOGUE# 0
	save %sp,-112,%sp
	!#PROLOGUE# 1
	mov %i0,%i3
	mov %i1,%i5
	mov 0,%l4
	ld [%i3+8],%o1
	mov 0,%l6
	ldub [%o1+12],%o0
	add %o0,-19,%o0
	and %o0,0xff,%o0
	cmp %o0,1
	bgu L701
	mov -1,%l5
	ld [%o1+16],%l4
L701:
	ld [%i3+24],%i0
	cmp %i0,0
	be L736
	cmp %l5,0
	sethi %hi(_asm_out_file),%i1
	sethi %hi(LC38),%i4
L731:
	ld [%i0+20],%l7
	ldub [%l7+12],%o0
	cmp %o0,111
	bne L737
	cmp %l4,0
	ld [%l7+20],%o2
	ld [%l7+8],%o1
	ld [%o2+8],%o0
	cmp %o1,%o0
	be,a L705
	mov %o2,%l7
L705:
	cmp %l4,0
L737:
	be L738
	cmp %l5,0
	ld [%l4+28],%o0
	cmp %o0,25
	be L706
	cmp %l5,0
L738:
	bl L708
	ld [%i1+%lo(_asm_out_file)],%o0
	or %i4,%lo(LC38),%o1
	mov %i2,%o2
	add %l6,1,%l6
	call _fprintf,0
	mov -1,%l5
L708:
	cmp %l4,0
	be L739
	sll %l6,3,%o0
	ldub [%l4+33],%l0
	call .rem,0
	mov %l0,%o1
	cmp %o0,0
	be L739
	cmp %l4,0
	sra %l0,3,%l0
	add %l6,%l0,%o0
	add %o0,-1,%o0
	call .div,0
	mov %l0,%o1
	call .umul,0
	mov %l0,%o1
	mov %o0,%o3
	ld [%i1+%lo(_asm_out_file)],%o0
	sethi %hi(LC26),%o1
	or %o1,%lo(LC26),%o1
	sub %o3,%l6,%o2
	call _fprintf,0
	mov %o3,%l6
	cmp %l4,0
L739:
	be,a L711
	ld [%i3+8],%o0
	ld [%l4+24],%o2
	ld [%o2+12],%o1
	sethi %hi(131072),%o0
	andcc %o1,%o0,%g0
	bne,a L712
	ld [%o2+16],%o0
	call _abort,0
	nop
L712:
	call .umul,0
	ldub [%l4+32],%o1
	mov %o0,%l0
	addcc %l0,7,%l0
	bneg,a L713
	add %l0,7,%l0
L713:
	b L714
	sra %l0,3,%l0
L711:
	call _int_size_in_bytes,0
	ld [%o0+8],%o0
	mov %o0,%l0
L714:
	mov %l7,%o0
	call _output_constant,0
	mov %l0,%o1
	b L704
	add %l6,%l0,%l6
L706:
	ldub [%l7+12],%o0
	cmp %o0,35
	be,a L716
	ld [%l4+24],%o0
	ld [%l4+36],%o0
	ld [%o0+20],%o1
	sethi %hi(LC39),%o0
	call _error,0
	or %o0,%lo(LC39),%o0
	b L740
	ld [%i0+4],%i0
L716:
	ldub [%l4+32],%o1
	ld [%o0+16],%o0
	call .umul,0
	ld [%l4+44],%l0
	add %l0,%o0,%l3
	cmp %l0,%l3
	bge,a L740
	ld [%i0+4],%i0
L728:
	cmp %l0,0
	bge L720
	mov %l0,%l1
	add %l0,7,%l1
L720:
	sra %l1,3,%l1
	cmp %l0,0
	bge L721
	mov %l0,%l2
	add %l0,7,%l2
L721:
	and %l2,-8,%o0
	cmp %l5,0
	bge L735
	sub %l0,%o0,%l2
	mov %l1,%l5
	b L723
	mov 0,%i2
L726:
	or %i4,%lo(LC38),%o1
	mov %i2,%o2
	add %l5,1,%l5
	add %l6,1,%l6
	call _fprintf,0
	mov 0,%i2
L735:
	cmp %l1,%l5
	bne L726
	ld [%i1+%lo(_asm_out_file)],%o0
L723:
	sub %l3,%l0,%o2
	mov 8,%o0
	sub %o0,%l2,%o3
	cmp %o2,%o3
	bg,a L727
	mov %o3,%o2
L727:
	sub %l3,%l0,%o0
	add %l0,%o2,%l0
	cmp %l0,%l3
	ld [%l7+16],%o1
	sub %o0,%o2,%o0
	sra %o1,%o0,%o1
	mov 1,%o0
	sll %o0,%o2,%o0
	add %o0,-1,%o0
	and %o1,%o0,%o1
	sub %o3,%o2,%o0
	sll %o1,%o0,%o1
	bl L728
	or %i2,%o1,%i2
L704:
	ld [%i0+4],%i0
L740:
	cmp %l4,0
	be L729
	mov 0,%o0
	ld [%l4+4],%o0
L729:
	cmp %i0,0
	bne L731
	mov %o0,%l4
	cmp %l5,0
L736:
	bl L732
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC38),%o1
	or %o1,%lo(LC38),%o1
	mov %i2,%o2
	call _fprintf,0
	add %l6,1,%l6
L732:
	cmp %l6,%i5
	bge L733
	sethi %hi(_asm_out_file),%o0
	ld [%o0+%lo(_asm_out_file)],%o0
	sethi %hi(LC26),%o1
	or %o1,%lo(LC26),%o1
	call _fprintf,0
	sub %i5,%l6,%o2
L733:
	ret
	restore
	.global _const_labelno
	.common _const_labelno,8,"bss"
	.global _var_labelno
	.common _var_labelno,8,"bss"

	.reserve _function_defined,8,"bss"

	.reserve _real_constant_chain,8,"bss"

	.reserve _const_hash_table,4032,"bss"

	.reserve _const_rtx_hash_table,248,"bss"
