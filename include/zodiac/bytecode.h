#pragma once

#include "build_data.h"
#include "common.h"
#include "stack.h"
#include "string_builder.h"

namespace Zodiac
{
    enum class Bytecode_Instruction : uint8_t
    {
        NOP        = 0x00,

        EXIT       = 0x01,
        CALL       = 0x02,
        RETURN     = 0x03,
        ALLOCL     = 0x04,
        LOAD_IM    = 0x05,
        LOADL      = 0x06,
        LOAD_PARAM = 0x07,
        STOREL     = 0x08,

        PUSH_ARG   = 0x09,

        ADD        = 0x0A,
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
        TEMPORARY,
        ALLOCL,
        PARAMETER,
    };

    struct Bytecode_Value
    {
        Bytecode_Value_Kind kind = Bytecode_Value_Kind::INVALID; 

        Atom name = {};

        union
        {
            uint32_t local_index = 0;
            uint32_t alloc_index;
            uint32_t param_index;
        };

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

    struct Bytecode_Parameter
    {
        Bytecode_Value *value = nullptr;
        AST_Declaration *ast_decl = nullptr;
    };

    struct Bytecode_Local_Alloc
    {
        Bytecode_Value *value = nullptr;
        AST_Declaration *ast_decl = nullptr;
    };

    struct Bytecode_Function
    {
        uint32_t index = 0;
        Array<Bytecode_Parameter> parameters = {};
        Array<Bytecode_Value*> local_temps = {};
        Array<Bytecode_Local_Alloc> local_allocs = {};
        Array<Bytecode_Block*> blocks = {};
        Bytecode_Block *last_block = nullptr;

        AST_Declaration *ast_decl = nullptr;
    };

    struct Bytecode_Program
    {
        Array<Bytecode_Function*> functions = {};
        Bytecode_Function *entry_function = nullptr;
    };

    struct Bytecode_Builder
    {
        Allocator *allocator = nullptr;

        Bytecode_Program program = {};

        Array<AST_Type*> emitted_types = {};

        Bytecode_Block *insert_block = nullptr;
        Bytecode_Function *current_function = nullptr;
    };

    struct Bytecode_Iterator
    {
        Bytecode_Builder *builder = nullptr;
        Stack<Bytecode_Value*> arg_stack = {};

        int64_t function_index    = -1;
        int64_t block_index       = -1;
        int64_t instruction_index = -1;

        int32_t local_temp_index = -1;
        int32_t local_alloc_index = -1;
    };

    void bytecode_builder_init(Allocator *allocator, Bytecode_Builder *builder);

    void bytecode_emit_node(Bytecode_Builder *builder, AST_Node *node);

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_function_declaration(Bytecode_Builder *builder, AST_Declaration *decl);
    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *statement);
    void bytecode_emit_return_statement(Bytecode_Builder *builder, Bytecode_Value *ret_val);
    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expression);
    Bytecode_Value *bytecode_emit_call_expression(Bytecode_Builder *builder,
                                                  AST_Expression *expression);
    Bytecode_Value *bytecode_emit_builtin_call_expression(Bytecode_Builder *builder,
                                                          AST_Expression *expression);
    Bytecode_Value *bytecode_emit_binary_expression(Bytecode_Builder *builder, AST_Expression *expr);
    Bytecode_Value *bytecode_emit_identifier(Bytecode_Builder *builder, AST_Identifier *ident);
    Bytecode_Value *bytecode_emit_lvalue(Bytecode_Builder *builder, AST_Expression *lvalue_expr);
    Bytecode_Value *bytecode_emit_allocl(Bytecode_Builder *builder, AST_Declaration *decl, Atom name);
    
    void bytecode_emit_call_arg(Bytecode_Builder *builder, AST_Expression *arg_expr);

    void bytecode_push_local_temporary(Bytecode_Builder *builder, Bytecode_Value *value);
    void bytecode_push_local_alloc(Bytecode_Builder *builder, Bytecode_Value *value,
                                   AST_Declaration *decl);

    void bytecode_emit_size_spec(Bytecode_Builder *builder, bool sign, uint8_t size);

    void bytecode_emit_load_im(Bytecode_Builder *builder, bool sign, uint8_t size);
    void bytecode_emit_loadl(Bytecode_Builder *builder, Bytecode_Value *allocl);
    void bytecode_emit_load_param(Bytecode_Builder *builder, Bytecode_Value *param);
    void bytecode_emit_storel(Bytecode_Builder *builder, Bytecode_Value *dest, Bytecode_Value *value);

    Bytecode_Value *bytecode_emit_number_literal(Bytecode_Builder *builder, AST_Expression *expr);
    void bytecode_emit_type_index(Bytecode_Builder *builder, AST_Type *type);
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
    Bytecode_Value *bytecode_find_value_for_parameter(Bytecode_Builder *builder,
                                                      AST_Declaration *decl);
    Bytecode_Value *bytecode_find_value_for_variable(Bytecode_Builder *builder,
                                                     AST_Declaration *decl);

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Declaration* decl);
    Bytecode_Block *bytecode_new_block(Allocator *allocator, const char* name);

    Bytecode_Value *bytecode_new_value(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type);
    Bytecode_Value *bytecode_new_integer_literal(Bytecode_Builder *builder, AST_Type *type);

    Bytecode_Iterator bytecode_iterator_create(Bytecode_Builder *builder);
    void bytecode_iterator_free(Bytecode_Iterator *bci);
    void bytecode_iterator_advance_function(Bytecode_Iterator *bci);
    Bytecode_Function *bytecode_iterator_get_function(Bytecode_Iterator *bci);
    void bytecode_iterator_advance_block(Bytecode_Iterator *bci);
    Bytecode_Block *bytecode_iterator_get_block(Bytecode_Iterator *bci);
    void bytecode_iterator_advance_ip(Bytecode_Iterator *bci, int64_t adv = 1);
    uint8_t bytecode_iterator_fetch_byte(Bytecode_Iterator *bci);
    uint32_t bytecode_iterator_fetch_32(Bytecode_Iterator *bci);
    uint64_t bytecode_iterator_fetch_64(Bytecode_Iterator *bci);

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder);
    void bytecode_print(String_Builder *sb, Bytecode_Builder *builder);
    void bytecode_print_function(String_Builder *sb, Bytecode_Iterator *bci);
    void bytecode_print_block(String_Builder *sb, Bytecode_Iterator *bci);
    void bytecode_print_instruction(String_Builder *sb, Bytecode_Iterator *bci);
    void bytecode_print_size_spec(String_Builder *sb, Bytecode_Size_Specifier size_spec);
    void bytecode_print_im(String_Builder *sb, Bytecode_Iterator *bci);
    
}
