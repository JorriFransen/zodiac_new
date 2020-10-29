#include "bytecode.h"

namespace Zodiac
{
    Bytecode_Builder bytecode_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        Bytecode_Builder result = {};
        result.allocator = allocator;
        result.build_data = build_data;
        result.insert_block = nullptr;
        array_init(allocator, &result.functions);
        return result;
    }

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);
        assert(decl->type);

        auto ex_func = bytecode_find_function(builder, decl);

        if (ex_func)
        {
            assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);
            assert(false);
        }

        assert(!(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE));

        assert(decl->identifier);
        auto name = decl->identifier->atom;

        Bytecode_Function *result = bytecode_new_function(builder, decl->type, name);

        auto index = builder->functions.count;
        array_append(&builder->functions, { decl, result, index });

        decl->decl_flags |= AST_DECL_FLAG_REGISTERED_BYTECODE;

        return result;
    }

    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bytecode_find_function(builder, decl);
        assert(func);

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
        {
            auto param_decl = decl->function.parameter_declarations[i];
            assert(param_decl->kind == AST_Declaration_Kind::PARAMETER);

            assert(false);
        }

        for (int64_t i = 0; i < decl->function.variable_declarations.count; i++)
        {
            auto var_decl = decl->function.variable_declarations[i];
            assert(var_decl->kind == AST_Declaration_Kind::VARIABLE);

            assert(false);
        }

        Bytecode_Block *entry_block = bytecode_new_block(builder, "entry");
        bytecode_append_block(func, entry_block);

        bytecode_set_insert_point(builder, entry_block);

        bytecode_emit_statement(builder, decl->function.body);

        return func;
    }

    Bytecode_Value *bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(false);
    }

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name)
    {
        Bytecode_Block *result = alloc_type<Bytecode_Block>(builder->allocator);
        result->name = name;
        result->function = nullptr;
        array_init(builder->allocator, &result->instructions);
        return result;
    }

    void bytecode_append_block(Bytecode_Function *function, Bytecode_Block *block)
    {
        assert(!block->function);

        array_append(&function->blocks, block);
        block->function = function;
    }

    void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block)
    {
        builder->insert_block = block;
    }

    Bytecode_Function *bytecode_find_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            if (builder->functions[i].declaration == decl) return builder->functions[i].bc_func;
        }

        return nullptr;
    }

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        assert(type->kind == AST_Type_Kind::FUNCTION);

        Bytecode_Function *result = alloc_type<Bytecode_Function>(builder->allocator);

        result->type = type;
        result->name = name;

        array_init(builder->allocator, &result->blocks, 4);

        return result;
    }

    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(builder->insert_block);

        switch (stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                for (int64_t i = 0; i < stmt->block.statements.count; i++)
                {
                    bytecode_emit_statement(builder, stmt->block.statements[i]);
                }
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false); //@@TODO: Implement!

            case AST_Statement_Kind::RETURN:
            {
                if (stmt->expression)
                {
                    auto ret_val = bytecode_emit_expression(builder, stmt->expression);
                    bytecode_emit_instruction(builder, RETURN, ret_val, nullptr, nullptr);
                }
                else
                {
                    assert(false); //@@TODO: Implement!
                }
                break;
            }

            case AST_Statement_Kind::BREAK: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::DECLARATION: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::EXPRESSION: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::WHILE: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::FOR: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::IF: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::SWITCH: assert(false); //@@TODO: Implement!
        }
    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr)
    {
        Bytecode_Value *result = nullptr;

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::IDENTIFIER: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::DOT: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::BINARY: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::UNARY: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::POST_FIX: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::PRE_FIX: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::CALL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::BUILTIN_CALL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::ADDROF: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::COMPOUND: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::SUBSCRIPT: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::CAST: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::INTEGER_LITERAL:
            {
                result = bytecode_integer_literal_new(builder, expr->type, expr->integer_literal);
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::STRING_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::CHAR_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::BOOL_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::NULL_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::RANGE: assert(false); //@TODO: Implement!
        }

        assert(result);
        assert(result->type = expr->type);

        return result;
    }

    Bytecode_Instruction *bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op,
                                                    Bytecode_Value *a, Bytecode_Value *b,
                                                    Bytecode_Value *result)
    {
        auto inst = alloc_type<Bytecode_Instruction>(builder->allocator);
        inst->op = op;
        inst->a = a;
        inst->b = b;
        inst->result = result;

        assert(builder->insert_block);
        array_append(&builder->insert_block->instructions, inst);
        return inst;
    }

    Bytecode_Value *bytecode_value_new(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type)
    {
        assert(kind != Bytecode_Value_Kind::INVALID);

        auto result = alloc_type<Bytecode_Value>(builder->allocator);
        result->kind = kind;
        result->type = type;

        return result;
    }

    Bytecode_Value *bytecode_integer_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal)
    {
        assert(type->kind == AST_Type_Kind::INTEGER);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::INTEGER_LITERAL, type);
        result->value.integer_literal = integer_literal;
        return result;
    }
}
