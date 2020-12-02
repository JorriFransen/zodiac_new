#pragma once

#include "allocator.h"
#include "build_data.h"
#include "stack.h"

#include <inttypes.h>

namespace Zodiac
{
    enum Bytecode_Opcode : uint8_t
    {
        NOP          = 0x00,

        ALLOCL       = 0x01,

        STOREL       = 0x02,
        STORE_ARG    = 0x03,
        STORE_GLOBAL = 0x04,
        STORE_PTR    = 0x05,

        LOADL        = 0x06,
        LOAD_PARAM   = 0x07,
        LOAD_GLOBAL  = 0x08,
        LOAD_PTR     = 0x09,

        ADD_S        = 0x0a,
        SUB_S        = 0x0b,
        REM_S        = 0x0c,
        MUL_S        = 0x0d,
        DIV_S        = 0x0e,

        EQ_S         = 0x0f,
        NEQ_S        = 0x10,
        LT_S         = 0x11,
        LTEQ_S       = 0x12,
        GT_S         = 0x13,
        GTEQ_S       = 0x14,

        ADD_U        = 0x15,
        SUB_U        = 0x16,
        REM_U        = 0x17,
        MUL_U        = 0x18,
        DIV_U        = 0x19,

        EQ_U         = 0x1a,
        NEQ_U        = 0x1b,
        LT_U         = 0x1c,
        LTEQ_U       = 0x1d,
        GT_U         = 0x1e,
        GTEQ_U       = 0x1f,

        ADD_F        = 0x20,
        SUB_F        = 0x21,
        MUL_F        = 0x22,
        DIV_F        = 0x23,

        EQ_F         = 0x24,
        NEQ_F        = 0x25,
        LT_F         = 0x26,
        LTEQ_F       = 0x27,
        GT_F         = 0x28,
        GTEQ_F       = 0x29,

        PUSH_ARG     = 0x2a,
        CALL         = 0x2b,
        RETURN       = 0x2c,
        RETURN_VOID  = 0x2d,

        JUMP         = 0x2e,
        JUMP_IF      = 0x2f,
        SWITCH       = 0x30,

        PTR_OFFSET   = 0x31,
        AGG_OFFSET   = 0x32,

        ZEXT         = 0x33,
        SEXT         = 0x34,
        TRUNC        = 0x35,
        F_TO_S       = 0x36,
        S_TO_F       = 0x37,
        U_TO_F       = 0x38,
        F_TO_F       = 0x39,
        PTR_TO_INT   = 0x3a,

        SIZEOF       = 0x3b,
        OFFSETOF     = 0x3c,

        EXIT         = 0x3d,
        SYSCALL      = 0x3e,
    };

    enum class Bytecode_Value_Kind
    {
        INVALID,

        INTEGER_LITERAL,
        FLOAT_LITERAL,
        STRING_LITERAL,
        BOOL_LITERAL,
        NULL_LITERAL,

        TEMP,
        ALLOCL,
        PARAM,
        GLOBAL,

        FUNCTION,
        BLOCK,
        TYPE,

        SWITCH_DATA,
    };

    struct Bytecode_Value;
    struct Bytecode_Block;
    struct Bytecode_Switch_Case
    {
        Bytecode_Value *case_value = nullptr;
        Bytecode_Block *target_block = nullptr;
    };

    struct Bytecode_Switch_Data
    {
        Array<Bytecode_Switch_Case> cases = {};
        Bytecode_Block *default_block = nullptr;
    };

    struct Bytecode_Function;
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
            } allocl;

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

            struct
            {
                Atom name;
                int64_t index;       // Used in llvm generation
                int64_t byte_offset; // Used in interpreter
            } global;

            void *pointer;
            Integer_Literal integer_literal;
            Float_Literal float_literal;
            Atom string_literal;
            bool bool_literal;

            Bytecode_Function *function;
            Bytecode_Block *block;

            Bytecode_Switch_Data switch_data = {};

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
        BC_FUNC_FLAG_NONE           = 0x00,
        BC_FUNC_FLAG_CRT_ENTRY      = 0x01,
        BC_FUNC_FLAG_BYTECODE_ENTRY = 0x02,
        BC_FUNC_FLAG_FOREIGN        = 0x04,
        BC_FUNC_FLAG_EMITTED        = 0x08,
        BC_FUNC_FLAG_NORETURN       = 0x10,
    };

    struct Bytecode_Function;
    struct Bytecode_Block
    {
        Atom name = {};
        Bytecode_Function *function = nullptr;

        int64_t first_instruction_index = -1;
        int64_t instruction_count = -1;
    };

    const int64_t BC_INSTRUCTIONS_PER_BUCKET = 64;
    struct BC_Instruction_Bucket
    {
        BC_Instruction_Bucket *next_bucket = nullptr;
        Bytecode_Instruction instructions[BC_INSTRUCTIONS_PER_BUCKET];
        int16_t count = 0;
    };

    struct Bytecode_Function
    {
        Bytecode_Function_Flags flags = BC_FUNC_FLAG_NONE;
        AST_Type *type = nullptr;
        Atom name = {};

        Array<Bytecode_Value *> parameters = {};
        Array<Bytecode_Value *> locals = {};
        Array<Bytecode_Value *> temps = {};

        //@@TODO: @@CLEANUP: These might not need to be pointers
        Array<Bytecode_Block  *> blocks = {};

        BC_Instruction_Bucket *first_bucket = nullptr;
        BC_Instruction_Bucket *last_bucket = nullptr;
        int64_t instruction_count = 0;
        
        // Array<Bytecode_Instruction *> instructions = {};
    };

    struct Bytecode_Function_Info
    {
        AST_Declaration *declaration = nullptr;
        Bytecode_Function *bc_func = nullptr;

        int64_t index = -1;
    };

    struct Bytecode_Global_Info
    {
        AST_Declaration *declaration = nullptr;
        Bytecode_Value *global_value = nullptr;
        Const_Value init_const_val = {};
        bool has_initializer = false;
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
        Array<Bytecode_Function *> foreign_functions = {};
        Array<Bytecode_Global_Info> globals = {};

        int64_t global_data_size = 0;
        int64_t run_wrapper_count = 0;

        Bytecode_Function *current_function = nullptr;
        // Holds parameters for the function currently being emitted
        Array<Bytecode_Local_Variable_Info> parameters = {};
        // Holds local variables for the function currently being emitted
        Array<Bytecode_Local_Variable_Info> locals = {};
        int64_t next_temp_index = 0; // Reset for each function

        Stack<Bytecode_Block *> break_block_stack = {};
    };

    Bytecode_Builder bytecode_builder_create(Allocator *allocator, Build_Data *build_data);

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder,
                                                  AST_Declaration *decl);
    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl);
    Bytecode_Global_Info bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl);
    Bytecode_Function *bytecode_emit_run_wrapper(Bytecode_Builder *builder, AST_Declaration *decl,
                                                 Bytecode_Function *pre_main_func);

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name);
    void bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function *function,
                               Bytecode_Block *block);
    Atom bytecode_get_unique_block_name(Bytecode_Builder *builder, Atom name);

    void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block);

    Bytecode_Function *bytecode_find_function(Bytecode_Builder *builder, AST_Declaration *decl);
    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Type *type, Atom name);

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    void bytecode_emit_for_statement(Bytecode_Builder *builder, AST_Statement *stmt);
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
                                                    Bytecode_Value *result_value);

    Bytecode_Instruction *get_instruction_by_index(Bytecode_Function *func, int64_t index);

    void bytecode_add_default_switch_case(Bytecode_Instruction *inst, Bytecode_Block *block);
    void bytecode_add_switch_case(Bytecode_Instruction *inst, Bytecode_Value *case_value,
                                  Bytecode_Block *case_block);

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
    Bytecode_Value *bytecode_bool_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                              bool value);
    Bytecode_Value *bytecode_null_literal_new(Bytecode_Builder *builder, AST_Type *type);
    Bytecode_Value *bytecode_local_alloc_new(Bytecode_Builder *builder, AST_Type *type, Atom name);
    Bytecode_Value *bytecode_parameter_new(Bytecode_Builder *builder, Bytecode_Function *func,
                                           AST_Type *type, Atom name);
    Bytecode_Value *bytecode_global_new(Bytecode_Builder *builder, AST_Type *type, Atom name);
    Bytecode_Value *bytecode_temporary_new(Bytecode_Builder *builder, AST_Type *type);
    Bytecode_Value *bytecode_function_value_new(Bytecode_Builder *builder,
                                                Bytecode_Function *func);
    Bytecode_Value *bytecode_block_value_new(Bytecode_Builder *builder, Bytecode_Block *block);
    Bytecode_Value *bytecode_type_value_new(Bytecode_Builder *builder, AST_Type *type);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func);
    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Instruction *inst);
    void bytecode_print_value(String_Builder *sb, Bytecode_Value *value);

}
