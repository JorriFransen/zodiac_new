#pragma once

#include "array_ref.h"
#include "build_data.h"

#include <inttypes.h>

namespace Zodiac
{
    enum BC_Opcode : uint8_t
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

        NEG_LOG      = 0x2a,

        PUSH_ARG     = 0x2b,
        CALL         = 0x2c,
        RETURN       = 0x2d,
        RETURN_VOID  = 0x2e,

        JUMP         = 0x2f,
        JUMP_IF      = 0x30,
        SWITCH       = 0x31,

        PTR_OFFSET   = 0x32,
        AGG_OFFSET   = 0x33,

        ZEXT         = 0x34,
        SEXT         = 0x35,
        TRUNC        = 0x36,
        F_TO_S       = 0x37,
        S_TO_F       = 0x38,
        U_TO_F       = 0x39,
        F_TO_F       = 0x3a,
        PTR_TO_INT   = 0x3b,
        PTR_TO_PTR   = 0x3c,

        SIZEOF       = 0x3d,
        OFFSETOF     = 0x3e,

        EXIT         = 0x3f,
        SYSCALL      = 0x40,
    };

    enum class BC_Value_Kind
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

    struct BC_Block;
    struct BC_Value;
    struct BC_Function_Info;
    struct BC_Local_Variable_Info;
    struct BC_Global_Info;
    struct BC_Instruction;

    struct BC_Builder
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        BC_Function *current_function = nullptr;

        Array<BC_Local_Variable_Info> parameters = {};
        Array<BC_Local_Variable_Info> locals = {};

        Array<BC_Global_Info> globals = {};
        int64_t global_data_size = 0;

        Array<BC_Value *> string_literals = {};

        int64_t next_temp_index = 0;
        int64_t run_wrapper_count = 0;

        BC_Block *insert_block = nullptr;

        Array<BC_Function_Info> functions = {};
        Array<BC_Function *> foreign_functions = {};

        Stack<BC_Block *> break_block_stack = {};
    };

    typedef uint64_t Bytecode_Function_Flags;

    enum Bytecode_Function_Flags_ : Bytecode_Function_Flags
    {
        BC_FUNC_FLAG_NONE            = 0x000,
        BC_FUNC_FLAG_CRT_ENTRY       = 0x001,
        BC_FUNC_FLAG_BYTECODE_ENTRY  = 0x002,
        BC_FUNC_FLAG_FOREIGN         = 0x004,
        BC_FUNC_FLAG_EMITTED         = 0x008,
        BC_FUNC_FLAG_NORETURN        = 0x010,
        BC_FUNC_FLAG_LLVM_REGISTERED = 0x020,
        BC_FUNC_FLAG_COMPILER_FUNC   = 0x040,
        BC_FUNC_FLAG_IS_TEST         = 0x080,
    };


    struct BC_Function
    {
        Bytecode_Function_Flags flags = BC_FUNC_FLAG_NONE;
        AST_Type *type = nullptr;

        Atom name = {};
        Atom name_prefix = {};

        Array<BC_Block *> blocks = {};

        Array<BC_Value *> parameters = {};
        Array<BC_Value *> locals = {};
        Array<BC_Value *> temps = {};
    };

    struct BC_Function_Info
    {
        AST_Declaration *declaration = nullptr;
        BC_Function *bc_func = nullptr;
        int64_t index = -1;
    };

    struct BC_Block
    {
        Atom name = {};
        BC_Function *function = nullptr;

        Array<BC_Instruction> instructions = {};
    };

    struct BC_Switch_Case
    {
        BC_Value *case_value = nullptr;
        BC_Block *target_block = nullptr;
    };

    struct BC_Switch_Data
    {
        Array<BC_Switch_Case> cases = {};
    };

    struct BC_Local_Variable_Info
    {
        AST_Declaration *declaration = nullptr;
        BC_Value *allocl_value = nullptr;
    };

    struct BC_Value
    {
        BC_Value_Kind kind = BC_Value_Kind::INVALID;

        AST_Type *type = nullptr;

        union
        {
            struct
            {
                int64_t index;
            } temp;

            struct
            {
                Atom name;
                int64_t index;
            } parameter, allocl, global;

            BC_Function *function;
            BC_Block *block;
            Integer_Literal integer_literal;
            Float_Literal float_literal;
            Atom string_literal;
            bool bool_literal;

            BC_Switch_Data switch_data = {};
        };
    };

    struct BC_Instruction
    {
        BC_Opcode op = NOP;

        BC_Value *a = nullptr;
        BC_Value *b = nullptr;
        BC_Value *result = nullptr;
    };

    struct BC_Global_Info
    {
        AST_Declaration *declaration = nullptr;
        bool has_initializer = false;

        Const_Value init_const_val;
        BC_Value *global_value;
    };

    BC_Builder bc_builder_create(Allocator *allocator, Build_Data *build_data);

    BC_Function *bc_register_function(BC_Builder *builder, AST_Declaration *decl);
    BC_Function *bc_emit_function_declaration(BC_Builder *builder, AST_Declaration *decl);

    BC_Global_Info bc_emit_global_variable(BC_Builder *builder, AST_Declaration *decl);

    BC_Function *bc_emit_run_wrapper(BC_Builder *builder, AST_Declaration *decl,
                                     BC_Function *pre_main_func);

    BC_Function *bc_emit_test_wrapper(BC_Builder *builder, Array<BC_Function *> test_functions);

    BC_Block *bc_new_block(BC_Builder *builder, const char *name);
    void bc_append_block(BC_Builder *builder, BC_Function *function, BC_Block *block);
    Atom bc_get_unique_block_name(BC_Builder *builder, Atom name);

    void bc_set_insert_point(BC_Builder *builder, BC_Block *block);

    BC_Function *bc_find_function(BC_Builder *builder, AST_Declaration *decl);
    BC_Function *bc_new_function(BC_Builder *builder, AST_Type *type, Atom name_prefix, Atom name);

    void bc_emit_declaration(BC_Builder *builder, AST_Declaration *decl);
    void bc_emit_statement(BC_Builder *builder, AST_Statement *stmt);
    void bc_emit_for_statement(BC_Builder *builder, AST_Statement *stmt);
    void bc_emit_if_statement(BC_Builder *builder, AST_Statement *stmt);
    BC_Value *bc_emit_expression(BC_Builder *builder, AST_Expression *expr);
    BC_Value *bc_emit_lvalue(BC_Builder *builder, AST_Expression *expr);

    BC_Value *bc_emit_struct_dereference(BC_Builder *builder,
                                         AST_Type *struct_type,
                                         BC_Value *parent_lvalue,
                                         AST_Declaration *import_link,
                                         AST_Type *result_type);

    BC_Value *bc_emit_identifier(BC_Builder *builder, AST_Identifier *identifier);
    BC_Value *bc_emit_identifier_lvalue(BC_Builder *builder, AST_Identifier *identifier);

    BC_Value *bc_emit_call(BC_Builder *builder, BC_Function *callee,
                           const Array_Ref<BC_Value *> args);
    BC_Value *bc_emit_call(BC_Builder *builder, AST_Expression *expr);

    BC_Value *bc_emit_builtin_call(BC_Builder *builder, AST_Expression *expr);

    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *expr);
    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *operand, AST_Type *target_type);
    BC_Value *bc_emit_cast_to_int(BC_Builder *builder, AST_Expression *operand_expr,
                                  AST_Type *target_type);

    BC_Value *bc_emit_cast_to_float(BC_Builder *builder, AST_Expression *operand_expr,
                                    AST_Type *target_type);

    void bc_emit_jump(BC_Builder *builder, BC_Block *dest);
    void bc_emit_jump_if(BC_Builder *builder, BC_Value *cond_val, BC_Block *then_dest,
                         BC_Block *else_dest);

    void bc_emit_store(BC_Builder *builder, BC_Value *dest,
                             BC_Value *source);

    BC_Value *bc_emit_load(BC_Builder *builder, BC_Value *source);

    BC_Value *bc_emit_float_literal(BC_Builder *builder,
                                                AST_Expression *literal_expr);

    BC_Value *bc_emit_zero_value(BC_Builder *builder, AST_Type *type);

    BC_Instruction *bc_emit_instruction(BC_Builder *builder, BC_Opcode op,
                                        BC_Value *a, BC_Value *b,
                                        BC_Value *result_value);

    void bc_add_default_switch_case(BC_Instruction *inst, BC_Block *block);
    void bc_add_switch_case(BC_Instruction *inst, BC_Value *case_value,
                                  BC_Block *case_block);

    void bc_push_break_block(BC_Builder *builder, BC_Block *block);
    void bc_pop_break_block(BC_Builder *builder);

    bool bc_block_ends_with_terminator(BC_Block *block);

    BC_Value *bc_find_parameter(BC_Builder *builder, AST_Declaration *decl);
    BC_Value *bc_find_variable(BC_Builder *builder, AST_Declaration *decl);

    BC_Value *bc_value_new(BC_Builder *builder, BC_Value_Kind kind,
                                       AST_Type *type);
    BC_Value *bc_integer_literal_new(BC_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal);
    BC_Value *bc_float_literal_new(BC_Builder *builder, AST_Type *type,
                                               float r32, double r64);
    BC_Value *bc_string_literal_new(BC_Builder *builder, Atom string_literal);
    BC_Value *bc_bool_literal_new(BC_Builder *builder, AST_Type *type,
                                              bool value);
    BC_Value *bc_null_literal_new(BC_Builder *builder, AST_Type *type);
    BC_Value *bc_local_alloc_new(BC_Builder *builder, AST_Type *type, Atom name);
    BC_Value *bc_parameter_new(BC_Builder *builder, BC_Function *func,
                                           AST_Type *type, Atom name);
    BC_Value *bc_global_new(BC_Builder *builder, AST_Type *type, Atom name);
    BC_Value *bc_temporary_new(BC_Builder *builder, AST_Type *type);
    BC_Value *bc_function_value_new(BC_Builder *builder,
                                                BC_Function *func);
    BC_Value *bc_block_value_new(BC_Builder *builder, BC_Block *block);
    BC_Value *bc_type_value_new(BC_Builder *builder, AST_Type *type);

    BC_Value *bc_get_string_literal(BC_Builder *builder, const String &str);
    BC_Value *bc_get_string_literal(BC_Builder *builder, const Atom &atom);

    bool bc_ready_to_run(BC_Builder *builder);

    void bc_print(Allocator *allocator, BC_Builder *builder);
    void bc_print_function(String_Builder *sb, BC_Function *func);
    void bc_print_instruction(String_Builder *sb, BC_Instruction *inst);
    void bc_print_value(String_Builder *sb, BC_Value *value);

}
