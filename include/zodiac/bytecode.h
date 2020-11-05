#pragma once

#include "allocator.h"
#include "build_data.h"
#include "stack.h"

#include <inttypes.h>

namespace Zodiac
{
    enum Bytecode_Opcode : uint8_t
    {
        NOP         = 0x0000,

        ALLOCL      = 0x0001,

        STOREL      = 0x0002,
        STORE_ARG   = 0x0003,
        STORE_PTR   = 0x0004,

        LOADL       = 0x0005,
        LOAD_PARAM  = 0x0006,
        LOAD_PTR    = 0x0007,

        ADD_S       = 0x0008,
        SUB_S       = 0x0009,
        REM_S       = 0x000a,
        MUL_S       = 0x000b,
        DIV_S       = 0x000c,

        EQ_S        = 0x000d,
        NEQ_S       = 0x000e,
        LT_S        = 0x000f,
        LTEQ_S      = 0x0010,
        GT_S        = 0x0011,
        GTEQ_S      = 0x0012,

        ADD_U       = 0x0013,
        SUB_U       = 0x0014,
        REM_U       = 0x0015,
        MUL_U       = 0x0016,
        DIV_U       = 0x0017,

        ADD_F       = 0x0018,
        SUB_F       = 0x0019,
        MUL_F       = 0x001a,
        DIV_F       = 0x001b,

        EQ_F        = 0x001c,
        NEQ_F       = 0x001d,
        LT_F        = 0x001e,
        LTEQ_F      = 0x001f,
        GT_F        = 0x0020,
        GTEQ_F      = 0x0021,

        PUSH_ARG    = 0x0022,
        CALL        = 0x0023,
        RETURN      = 0x0024,
        RETURN_VOID = 0x0025,

        JUMP        = 0x0026,
        JUMP_IF     = 0x0027,

        PTR_OFFSET  = 0x0028,
        AGG_OFFSET  = 0x0029,

        ZEXT        = 0x002a,
        SEXT        = 0x002b,
        TRUNC       = 0x002c,
        F_TO_S      = 0x002d,
        S_TO_F      = 0x002e,
        U_TO_F      = 0x002f,
        F_TO_F      = 0x0030,

        EXIT        = 0x0031,
        SYSCALL     = 0x0032,
    };

    enum class Bytecode_Value_Kind
    {
        INVALID,

        INTEGER_LITERAL,
        FLOAT_LITERAL,
        STRING_LITERAL,

        TEMP,
        ALLOCL,
        PARAM,

        FUNCTION,
        BLOCK,
    };

    struct Bytecode_Function;
    struct Bytecode_Block;
    struct Bytecode_Value
    {
        Bytecode_Value_Kind kind = Bytecode_Value_Kind::INVALID;
        AST_Type *type = nullptr;

        union
        {
            struct
            {
                Atom name;
                int64_t index;               // Used in llvm generation
                int64_t byte_offset_from_fp; // Used in interpreter
            } allocl = {};

            struct
            {
                int64_t index;
                int64_t byte_offset_from_fp; // Used in interpreter
            } temp;

            struct
            {
                Atom name;
                int64_t index;               // Used in llvm generation
                int64_t byte_offset_from_fp; // Used in interpreter
            } parameter;

            void *pointer;
            Integer_Literal integer_literal;
            Float_Literal float_literal;
            Atom string_literal;

            Bytecode_Function *function;
            Bytecode_Block *block;

        };
    };

    struct Bytecode_Instruction
    {
        Bytecode_Opcode op = NOP;

        Bytecode_Value *a = nullptr;
        Bytecode_Value *b = nullptr;
        Bytecode_Value *result = nullptr;
    };

    typedef uint64_t Bytecode_Function_Flags;

    enum Bytecode_Function_Flags_ : Bytecode_Function_Flags
    {
        BC_FUNC_FLAG_NONE      = 0,
        BC_FUNC_FLAG_CRT_ENTRY = 1,
        BC_FUNC_FLAG_FOREIGN   = 2,
        BC_FUNC_FLAG_EMITTED   = 4,
        BC_FUNC_FLAG_NORETURN  = 8,
    };

    struct Bytecode_Function;
    struct Bytecode_Block
    {
        const char *name = nullptr;
        Bytecode_Function *function = nullptr;

        //@@TODO: @@CLEANUP: These might not need to be pointers
        Array<Bytecode_Instruction *> instructions = {};
    };

    struct Bytecode_Function
    {
        Bytecode_Function_Flags flags = BC_FUNC_FLAG_NONE;
        AST_Type *type = nullptr;
        Atom name = {};

        Array<Bytecode_Value *> parameters = {};
        Array<Bytecode_Value *> locals = {};
        Array<Bytecode_Value *> temps = {};

        Array<Bytecode_Block  *> blocks = {};
    };

    struct Bytecode_Function_Info
    {
        AST_Declaration *declaration = nullptr;
        Bytecode_Function *bc_func = nullptr;

        int64_t index = -1;
    };

    struct Bytecode_Local_Variable_Info
    {
        AST_Declaration *declaration = nullptr;
        Bytecode_Value *allocl_value = nullptr;
    };

    struct Bytecode_Builder
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        Bytecode_Block *insert_block = nullptr;

        Array<Bytecode_Function_Info> functions = {};

        Bytecode_Function *current_function = nullptr;
        // Holds parameters for the function currently being emitted
        Array<Bytecode_Local_Variable_Info> parameters = {};
        // Holds local variables for the function currently being emitted
        Array<Bytecode_Local_Variable_Info> locals = {};
        int64_t next_temp_index = 0; // Reset for each function

        Stack<Bytecode_Block *> break_block_stack = {};
    };

    Bytecode_Builder bytecode_builder_create(Allocator *allocator, Build_Data *build_data);

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder, AST_Declaration *decl);
    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl);
    Bytecode_Value *bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl);

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name);
    void bytecode_append_block(Bytecode_Function *function, Bytecode_Block *block);

    void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block);

    Bytecode_Function *bytecode_find_function(Bytecode_Builder *builder, AST_Declaration *decl);
    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Type *type, Atom name);

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    void bytecode_emit_if_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_lvalue(Bytecode_Builder *builder, AST_Expression *expr);

    Bytecode_Value *bytecode_emit_call(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_builtin_call(Bytecode_Builder *builder, AST_Expression *expr);

    Bytecode_Value *bytecode_emit_cast(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_cast(Bytecode_Builder *builder, AST_Expression *operand,
                                       AST_Type *target_type);
    Bytecode_Value *bytecode_emit_cast_to_int(Bytecode_Builder *builder,
                                              AST_Expression *operand_expr, AST_Type *target_type);
    Bytecode_Value *bytecode_emit_cast_to_float(Bytecode_Builder *builder,
                                                AST_Expression *operand_expr,
                                                AST_Type *target_type);

    void bytecode_emit_jump(Bytecode_Builder *builder, Bytecode_Block *dest);
    void bytecode_emit_jump_if(Bytecode_Builder *builder, Bytecode_Value *cond_val,
                               Bytecode_Block *then_dest, Bytecode_Block *else_dest);

    void bytecode_emit_store(Bytecode_Builder *builder, Bytecode_Value *dest,
                             Bytecode_Value *source);

    Bytecode_Value *bytecode_emit_load(Bytecode_Builder *builder, Bytecode_Value *source);

    Bytecode_Value *bytecode_emit_float_literal(Bytecode_Builder *builder,
                                                AST_Expression *literal_expr);

    Bytecode_Value *bytecode_emit_zero_value(Bytecode_Builder *builder, AST_Type *type);

    Bytecode_Instruction *bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op,
                                                    Bytecode_Value *a, Bytecode_Value *b,
                                                    Bytecode_Value *result);

    void bytecode_push_break_block(Bytecode_Builder *builder, Bytecode_Block *block);
    void bytecode_pop_break_block(Bytecode_Builder *builder);

    bool bytecode_block_ends_with_terminator(Bytecode_Block *block);

    Bytecode_Value *bytecode_find_parameter(Bytecode_Builder *builder, AST_Declaration *decl);
    Bytecode_Value *bytecode_find_variable(Bytecode_Builder *builder, AST_Declaration *decl);

    Bytecode_Value *bytecode_value_new(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type);
    Bytecode_Value *bytecode_integer_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal);
    Bytecode_Value *bytecode_float_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                               float r32, double r64);
    Bytecode_Value *bytecode_string_literal_new(Bytecode_Builder *builder, Atom string_literal);
    Bytecode_Value *bytecode_local_alloc_new(Bytecode_Builder *builder, AST_Type *type, Atom name);
    Bytecode_Value *bytecode_parameter_new(Bytecode_Builder *builder, Bytecode_Function *func,
                                           AST_Type *type, Atom name);
    Bytecode_Value *bytecode_temporary_new(Bytecode_Builder *builder, AST_Type *type);
    Bytecode_Value *bytecode_function_value_new(Bytecode_Builder *builder,
                                                Bytecode_Function *func);
    Bytecode_Value *bytecode_block_value_new(Bytecode_Builder *builder, Bytecode_Block *block);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func);
    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Instruction *inst);
    void bytecode_print_value(String_Builder *sb, Bytecode_Value *value);

}
