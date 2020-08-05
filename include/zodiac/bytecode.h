#pragma once

#include "build_data.h"
#include "string_builder.h"

namespace Zodiac
{
    enum class Bytecode_Instruction : uint8_t
    {
        NOP      = 0x00,

        EXIT     = 0x01,
        CALL     = 0x02,
        LOAD_IM  = 0x03,
    };

    enum class Bytecode_Size_Specifier : uint8_t
    {
        INVALID = 0,
        SIGN_FLAG = 0x80, // 10000000

        U8        = 0x08, // 00001000
        S8        = 0x88, // 10001000

        U16       = 0x10, // 00010000
        S16       = 0x90, // 10010000

        U32       = 0x20, // 00100000
        S32       = 0xA0, // 10100000

        U64       = 0x40, // 01000000
        S64       = 0xC0, // 11000000

    };

    enum class Bytecode_Value_Kind : uint32_t
    {
        INVALID,

        NUMBER_LITERAL,
    };

    struct Bytecode_Value
    {
        Bytecode_Value_Kind kind = Bytecode_Value_Kind::INVALID; 

        uint32_t local_index = 0;

        AST_Type *type = nullptr;

        struct 
        {
            uint64_t value = 0;
        } integer_literal;
    };

    struct Bytecode_Block
    {
        String name = {};
        Array<uint8_t> instructions = {};
    };

    struct Bytecode_Function
    {
        uint32_t index = 0;
        Array<Bytecode_Value*> local_values = {};
        Array<Bytecode_Block*> blocks = {};
        Bytecode_Block *last_block = nullptr;

        AST_Declaration *ast_decl = nullptr;
    };

    struct Bytecode_Builder
    {
        Allocator *allocator = nullptr;
        Array<Bytecode_Function*> functions = {};

        Bytecode_Block *insert_block = nullptr;
        Bytecode_Function *current_function = nullptr;
    };

    void bytecode_builder_init(Allocator *allocator, Bytecode_Builder *builder);

    void bytecode_emit_node(Bytecode_Builder *builder, AST_Node *node);

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_function_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *statement);
    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expression);
    Bytecode_Value *bytecode_emit_call_expression(Bytecode_Builder *builder,
                                                  AST_Expression *expression);
    Bytecode_Value *bytecode_emit_builtin_call_expression(Bytecode_Builder *builder,
                                                          AST_Expression *expression);

    void bytecode_push_local_temporary(Bytecode_Builder *builder, Bytecode_Value *value);
    void bytecode_emit_local_temp(Bytecode_Builder *builder, uint32_t index); 

    void bytecode_emit_load_im(Bytecode_Builder *builder, bool sign, uint8_t size);

    Bytecode_Value *bytecode_emit_number_literal(Bytecode_Builder *builder, AST_Expression *expr);
    void bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Instruction op);
    uint64_t bytecode_emit_16(Bytecode_Builder *builder, uint16_t val);
    uint64_t bytecode_emit_32(Bytecode_Builder *builder, uint32_t val);
    uint64_t bytecode_emit_64(Bytecode_Builder *builder, uint64_t val);
    uint64_t bytecode_emit_byte(Bytecode_Builder *builder, uint8_t byte);

    void bytecode_builder_set_insert_point(Bytecode_Builder *builder, Bytecode_Function *func);
    void bytecode_builder_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block);

    void bytecode_builder_append_block(Bytecode_Builder *builder, Bytecode_Function *func,
                                       const char *name);

    Bytecode_Function *bytecode_find_function_for_decl(Bytecode_Builder *builder,
                                                       AST_Declaration *decl);

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Declaration* decl);
    Bytecode_Block *bytecode_new_block(Allocator *allocator, const char* name);

    Bytecode_Value *bytecode_new_value(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type);
    Bytecode_Value *bytecode_new_integer_literal(Bytecode_Builder *builder, AST_Type *type);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print(String_Builder *sb, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func);
    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block, int64_t *local_indexp);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Block *block, 
                                    Bytecode_Instruction inst, int64_t *ipp, int64_t *local_indexp);
    void bytecode_print_im(String_Builder *sb, Bytecode_Block *block, int64_t *ipp);
    
}
