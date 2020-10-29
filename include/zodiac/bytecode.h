#pragma once

#include "allocator.h"
#include "build_data.h"

#include <inttypes.h>

namespace Zodiac
{
    enum Bytecode_Opcode : uint8_t
    {
        NOP    = 0x0000,

        ALLOCL = 0x0001,

        STOREL = 0x0002,

        LOADL  = 0x0003,

        CALL   = 0x0004,
        RETURN = 0x0005,

        EXIT   = 0x0006,
    };

    enum class Bytecode_Value_Kind
    {
        INVALID,

        TEMP,
        INTEGER_LITERAL,
        ALLOCL,
        FUNCTION,
    };

    struct Bytecode_Function;
    struct Bytecode_Value
    {
        Bytecode_Value_Kind kind = Bytecode_Value_Kind::INVALID;
        AST_Type *type = nullptr;

        union
        {
            Atom name = {};
            int64_t temp_index;

            union
            {
                Integer_Literal integer_literal;
            } value;

            Bytecode_Function *function;

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

        Array<Bytecode_Value *> locals = {};

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

        // Holds local variables for the function currently being emitted
        Array<Bytecode_Local_Variable_Info> locals = {};
        int64_t next_temp_index = 0; // Reset for each function
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
    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr);

    Bytecode_Value *bytecode_emit_call(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_builtin_call(Bytecode_Builder *builder, AST_Expression *expr);

    void bytecode_emit_store(Bytecode_Builder *builder, Bytecode_Value *dest, Bytecode_Value *source);
    Bytecode_Value *bytecode_emit_load(Bytecode_Builder *builder, Bytecode_Value *source);

    Bytecode_Instruction *bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op,
                                                    Bytecode_Value *a, Bytecode_Value *b,
                                                    Bytecode_Value *result);

    Bytecode_Value *bytecode_find_variable(Bytecode_Builder *builder, AST_Declaration *decl);

    Bytecode_Value *bytecode_value_new(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type);
    Bytecode_Value *bytecode_integer_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal);
    Bytecode_Value *bytecode_local_alloc_new(Bytecode_Builder *builder, AST_Type *type, Atom name);
    Bytecode_Value *bytecode_temporary_new(Bytecode_Builder *builder, AST_Type *type);
    Bytecode_Value *bytecode_function_value_new(Bytecode_Builder *builder, Bytecode_Function *func);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func);
    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Instruction *inst);
    void bytecode_print_value(String_Builder *sb, Bytecode_Value *value);

}
