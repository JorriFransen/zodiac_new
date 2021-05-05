#pragma once

#include "allocator.h"
#include "array_ref.h"
#include "bucket_array.h"
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

    const int16_t BC_INSTRUCTIONS_PER_BUCKET = 64;
    typedef Bucket_Locator<Bytecode_Instruction, BC_INSTRUCTIONS_PER_BUCKET> Instruction_Locator;

    struct Bytecode_Function;
    struct Bytecode_Block
    {
        Atom name = {};
        Bytecode_Function *function = nullptr;

        int64_t instruction_count = -1;

        Instruction_Locator first_instruction = {};
        Bytecode_Instruction *last_instruction = nullptr;
    };


    struct Bytecode_Function
    {
        Bytecode_Function_Flags flags = BC_FUNC_FLAG_NONE;
        AST_Type *type = nullptr;

        Atom name_prefix = {};
        Atom name = {};

        Array<Bytecode_Value *> parameters = {};
        Array<Bytecode_Value *> locals = {};
        Array<Bytecode_Value *> temps = {};

        //@@TODO: @@CLEANUP: These might not need to be pointers
        Array<Bytecode_Block  *> blocks = {};

        Bucket_Array<Bytecode_Instruction, BC_INSTRUCTIONS_PER_BUCKET> instructions = {};
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

        // @SPEED: @CLEANUP: We might want some or all of these arrays to be
        //    hash tables for speed purposes.
        Array<Bytecode_Function_Info> functions = {};
        Array<Bytecode_Function *> foreign_functions = {};
        Array<Bytecode_Global_Info> globals = {};
        Array<Bytecode_Value *> string_literals = {};

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
    Bytecode_Function *bytecode_emit_test_wrapper(Bytecode_Builder *builder,
                                                  Array<Bytecode_Function *> test_functions);

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name);
    void bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function *function,
                               Bytecode_Block *block);
    Atom bytecode_get_unique_block_name(Bytecode_Builder *builder, Atom name);

    void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block);

    Bytecode_Function *bytecode_find_function(Bytecode_Builder *builder, AST_Declaration *decl);
    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Type *type,
                                             Atom name_prefix, Atom name);

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    void bytecode_emit_for_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    void bytecode_emit_if_statement(Bytecode_Builder *builder, AST_Statement *stmt);
    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_lvalue(Bytecode_Builder *builder, AST_Expression *expr);

    Bytecode_Value *bytecode_emit_struct_dereference(Bytecode_Builder *builder,
                                                     AST_Type *struct_type,
                                                     Bytecode_Value *parent_lvalue,
                                                     AST_Declaration *import_link,
                                                     AST_Type *result_type);

    Bytecode_Value *bytecode_emit_identifier(Bytecode_Builder *builder,
                                             AST_Identifier *identifier);
    Bytecode_Value *bytecode_emit_identifier_lvalue(Bytecode_Builder *builder,
                                                    AST_Identifier *identifier);

    Bytecode_Value *bytecode_emit_call(Bytecode_Builder *builder,
                                       Bytecode_Function *callee,
                                       const Array_Ref<Bytecode_Value *> args);
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

    Bytecode_Value *bytecode_get_string_literal(Bytecode_Builder *builder, const String &str);
    Bytecode_Value *bytecode_get_string_literal(Bytecode_Builder *builder, const Atom &atom);

    bool bytecode_ready_to_run(Bytecode_Builder *builder);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Instruction *inst);
    void bytecode_print_value(String_Builder *sb, Bytecode_Value *value);

}
