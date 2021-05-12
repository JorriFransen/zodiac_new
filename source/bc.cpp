#include "bc.h"

namespace Zodiac
{
    BC_Builder bc_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        BC_Builder result = {};

        result.allocator = allocator;
        result.build_data = build_data;

        return result;
    }

    BC_Function *bc_register_function(BC_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(false && "bc_register_function is not implemented!!!");
        return nullptr;
    }

    BC_Function *bc_emit_function_declaration(BC_Builder *builder,
                                                          AST_Declaration *decl)
    {
        assert(false && "bc_emit_function_declaration is not implemented!!!");
        return nullptr;
    }

    BC_Global_Info bc_emit_global_variable(BC_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(false && "bc_emit_global_variable is not implemented!!!");
        return {};
    }

    BC_Function *bc_emit_run_wrapper(BC_Builder *builder, AST_Declaration *decl,
                                                 BC_Function *pre_main_func)
    {
            assert(false && "bc_emit_run_wrapper is not implemented!!!");
            return nullptr;
    }

    BC_Function *bc_emit_test_wrapper(BC_Builder *builder,
                                                  Array<BC_Function *> test_functions)
    {
        assert(false && "bc_emit_test_wrapper is not implemented!!!");
        return  nullptr;
    }

    BC_Block *bc_new_block(BC_Builder *builder, const char *name)
    {
        assert(false && "bc_new_block is not implemented!!!");
        return nullptr;
    }

    void bc_append_block(BC_Builder *builder, BC_Function *function,
                               BC_Block *block)
    {
        assert(false && "bc_append_block is not implemented!!!");
    }

    Atom bc_get_unique_block_name(BC_Builder *builder, Atom name)
    {
        assert(false && "bc_get_unique_block_name is not implemented!!!");
        return {};
    }

    void bc_set_insert_point(BC_Builder *builder, BC_Block *block)
    {
        assert(false && "bc_set_insert_point is not implemented!!!");
    }

    BC_Function *bc_find_function(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(false && "bc_find_function is not implemented!!!");
        return nullptr;
    }

    BC_Function *bc_new_function(BC_Builder *builder, AST_Type *type,
                                             Atom name_prefix, Atom name)
    {
        assert(false && "bc_new_function is not implemented!!!");
        return nullptr;
    }

    void bc_emit_declaration(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(false && "bc_emit_declaration is not implemented!!!");
    }

    void bc_emit_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(false && "bc_emit_statement is not implemented!!!");
    }

    void bc_emit_for_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(false && "bc_emit_for_statement is not implemented!!!");
    }

    void bc_emit_if_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(false && "bc_emit_if_statement is not implemented!!!");
    }

    BC_Value *bc_emit_expression(BC_Builder *builder, AST_Expression *expr)
    {
        assert(false && "bc_emit_expression is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_lvalue(BC_Builder *builder, AST_Expression *expr)
    {
        assert(false && "bc_emit_lvalue is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_struct_dereference(BC_Builder *builder,
                                                     AST_Type *struct_type,
                                                     BC_Value *parent_lvalue,
                                                     AST_Declaration *import_link,
                                                     AST_Type *result_type)
    {
        assert(false && "bc_emit_struct_dereference is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_identifier(BC_Builder *builder,
                                             AST_Identifier *identifier)
    {
        assert(false && "bc_emit_identifier is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_identifier_lvalue(BC_Builder *builder,
                                                    AST_Identifier *identifier)
    {
        assert(false && "bc_emit_identifier_lvalue is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_call(BC_Builder *builder,
                                       BC_Function *callee,
                                       const Array_Ref<BC_Value *> args)
    {
        assert(false && "bc_emit_call is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_call(BC_Builder *builder, AST_Expression *expr)
    {
        assert(false && "bc_emit_call is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_builtin_call(BC_Builder *builder, AST_Expression *expr)
    {
        assert(false && "bc_emit_builtin_call is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *expr)
    {
        assert(false && "bc_emit_cast is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *operand,
                                       AST_Type *target_type)
    {
        assert(false && "bc_emit_cast is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_cast_to_int(BC_Builder *builder,
                                              AST_Expression *operand_expr, AST_Type *target_type)
    {
        assert(false && "bc_emit_cast_to_int is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_cast_to_float(BC_Builder *builder,
                                                AST_Expression *operand_expr,
                                                AST_Type *target_type)
    {
        assert(false && "bc_emit_cast_to_float is not implemented!!!");
        return nullptr;
    }

    void bc_emit_jump(BC_Builder *builder, BC_Block *dest)
    {
        assert(false && "bc_emit_jump is not implemented!!!");
    }

    void bc_emit_jump_if(BC_Builder *builder, BC_Value *cond_val,
                               BC_Block *then_dest, BC_Block *else_dest)
    {
        assert(false && "bc_emit_jump_if is not implemented!!!");
    }

    void bc_emit_store(BC_Builder *builder, BC_Value *dest,
                             BC_Value *source)
    {
        assert(false && "bc_emit_store is not implemented!!!");
    }

    BC_Value *bc_emit_load(BC_Builder *builder, BC_Value *source)
    {
        assert(false && "bc_emit_load is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_float_literal(BC_Builder *builder,
                                                AST_Expression *literal_expr)
    {
        assert(false && "bc_emit_float_literal is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_zero_value(BC_Builder *builder, AST_Type *type)
    {
        assert(false && "bc_emit_zero_value is not implemented!!!");
        return nullptr;
    }

    BC_Instruction *bc_emit_instruction(BC_Builder *builder, BC_Opcode op,
                                                    BC_Value *a, BC_Value *b,
                                                    BC_Value *result_value)
    {
        assert(false && "bc_emit_instruction is not implemented!!!");
        return nullptr;
    }

    void bc_add_default_switch_case(BC_Instruction *inst, BC_Block *block)
    {
        assert(false && "bc_add_default_switch_case is not implemented!!!");
    }

    void bc_add_switch_case(BC_Instruction *inst, BC_Value *case_value,
                                  BC_Block *case_block)
    {
        assert(false && "bc_add_switch_case is not implemented!!!");
    }

    void bc_push_break_block(BC_Builder *builder, BC_Block *block)
    {
        assert(false && "bc_push_break_block is not implemented!!!");
    }

    void bc_pop_break_block(BC_Builder *builder)
    {
        assert(false && "bc_pop_break_block is not implemented!!!");
    }

    bool bc_block_ends_with_terminator(BC_Block *block)
    {
        assert(false && "bc_block_ends_with_terminator is not implemented!!!");
        return false;
    }

    BC_Value *bc_find_parameter(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(false && "bc_find_parameter is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_find_variable(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(false && "bc_find_variable is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_value_new(BC_Builder *builder, BC_Value_Kind kind,
                                       AST_Type *type)
    {
        assert(false && "bc_value_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_integer_literal_new(BC_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal)
    {
        assert(false && "bc_integer_literal_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_float_literal_new(BC_Builder *builder, AST_Type *type,
                                               float r32, double r64)
    {
        assert(false && "bc_float_literal_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_string_literal_new(BC_Builder *builder, Atom string_literal)
    {
        assert(false && "bc_string_literal_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_bool_literal_new(BC_Builder *builder, AST_Type *type,
                                              bool value)
    {
        assert(false && "bc_bool_literal_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_null_literal_new(BC_Builder *builder, AST_Type *type)
    {
        assert(false && "bc_null_literal_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_local_alloc_new(BC_Builder *builder, AST_Type *type, Atom name)
    {
        assert(false && "bc_local_alloc_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_parameter_new(BC_Builder *builder, BC_Function *func,
                                           AST_Type *type, Atom name)
    {
        assert(false && "bc_parameter_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_global_new(BC_Builder *builder, AST_Type *type, Atom name)
    {
        assert(false && "bc_global_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_temporary_new(BC_Builder *builder, AST_Type *type)
    {
        assert(false && "bc_temporary_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_function_value_new(BC_Builder *builder,
                                                BC_Function *func)
    {
        assert(false && "bc_function_value_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_block_value_new(BC_Builder *builder, BC_Block *block)
    {
        assert(false && "bc_block_value_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_type_value_new(BC_Builder *builder, AST_Type *type)
    {
        assert(false && "bc_type_value_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_get_string_literal(BC_Builder *builder, const String &str)
    {
        assert(false && "bc_get_string_literal is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_get_string_literal(BC_Builder *builder, const Atom &atom)
    {
        assert(false && "bc_get_string_literal is not implemented!!!");
        return nullptr;
    }

    bool bc_ready_to_run(BC_Builder *builder)
    {
        assert(false && "bc_ready_to_run is not implemented!!!");
        return false;
    }

    void bc_print(Allocator *allocator, BC_Builder *builder)
    {
        assert(false && "bc_print is not implemented!!!");
    }

    void bc_print_function(String_Builder *sb, BC_Function *func)
    {
        assert(false && "bc_print_function is not implemented!!!");
    }

    void bc_print_instruction(String_Builder *sb, BC_Instruction *inst)
    {
        assert(false && "bc_print_instruction is not implemented!!!");
    }

    void bc_print_value(String_Builder *sb, BC_Value *value)
    {
        assert(false && "bc_print_value is not implemented!!!");
    }

}
