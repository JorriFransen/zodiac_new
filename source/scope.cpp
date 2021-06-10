
#include "scope.h"

#include "ast.h"
#include "build_data.h"
#include "string_builder.h"
#include "temp_allocator.h"

#include <stdio.h>

namespace Zodiac
{
    AST_Declaration *scope_find_declaration(Scope *scope, Atom atom)
    {
        assert(scope);

        auto scope_block = &scope->first_block;
        while (scope_block) {

            for (int64_t i = 0; i < scope_block->decl_count; i++) {
                AST_Declaration *decl = scope_block->declarations[i];

                if (decl->identifier && decl->identifier->atom == atom) {
                    return decl;
                }
            }

            scope_block = scope_block->next_block;
        }

        if (scope->parent) {
            return scope_find_declaration(scope->parent, atom);
        }

        return nullptr;
    }

    AST_Declaration *scope_find_declaration(Scope *scope, AST_Identifier *identifier)
    {
        assert(scope);
        assert(identifier);

        if (identifier->declaration) return identifier->declaration;

        AST_Declaration *result = scope_find_declaration(scope, identifier->atom);
        if (result) {
            identifier->declaration = result;
        }

        return result;
    }

    AST_Declaration *scope_find_declaration(Build_Data *build_data, Scope *scope, const char *cstr)
    {
        Atom atom = atom_get(&build_data->atom_table, cstr);
        return scope_find_declaration(scope, atom);
    }

    void scope_add_declaration(Scope *scope, AST_Declaration *adecl)
    {
        if (scope->current_block->decl_count >= scope->current_block->decl_cap)
        {
            scope_grow(scope->allocator, scope);
        }

        scope->current_block->declarations[scope->current_block->decl_count] = adecl;
        scope->current_block->decl_count++;
    }

    void scope_grow(Allocator *allocator, Scope *scope)
    {
        assert(scope);
        assert(scope->current_block);
        assert(scope->current_block->next_block == nullptr);

        auto new_cap = scope->current_block->decl_cap * 2;

        auto mem = (uint8_t*)alloc(allocator, sizeof(Scope_Block) +
                                                     (new_cap  *sizeof(AST_Declaration*)));
        assert(mem);

        Scope_Block *new_block = (Scope_Block*)mem;
        new_block->declarations = (AST_Declaration**)(mem + sizeof(Scope_Block));
        new_block->decl_count = 0;
        new_block->decl_cap = new_cap;
        new_block->next_block = nullptr;

        scope->current_block->next_block = new_block;
        scope->current_block = new_block;
    }

    Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent,
                     int64_t initial_cap /*=4*/)
    {
        auto mem = (uint8_t*)alloc(allocator,
                                   sizeof(Scope) + (initial_cap * sizeof(AST_Declaration*)));
        assert(mem);

        if (parent == nullptr) assert(kind == Scope_Kind::GLOBAL);

        Scope *result = (Scope*)mem;
        result->kind = kind;
        result->first_block.declarations = (AST_Declaration**)(mem + sizeof(Scope));
        result->first_block.decl_count = 0;
        result->first_block.decl_cap = initial_cap;
        result->first_block.next_block = nullptr;
        result->current_block = &result->first_block;
        result->parent = parent;
        result->allocator = allocator;
        result->function_declaration = nullptr;

        return result;
    }

    void scope_print_indent(String_Builder *sb, int64_t indent)
    {
        for (int64_t i = 0; i < indent; i++)
        {
            string_builder_append(sb, "    ");
        }
    }

    void scope_print(Scope *scope)
    {
        auto ta = temp_allocator_get();

        String_Builder sb = {};
        string_builder_init(ta, &sb);

        scope_print(&sb, scope, 0);

        auto str = string_builder_to_string(ta, &sb);
        printf("%s\n", str.data);
    }

    void scope_print(String_Builder *sb, Scope *scope, int64_t indent/*=0*/)
    {
        assert(sb);

        if (scope->first_block.decl_count <= 0) return;

        scope_print_indent(sb, indent);

        switch (scope->kind) {
            case Scope_Kind::INVALID: assert(false);

            case Scope_Kind::GLOBAL: {
                string_builder_append(sb, "GLOBAL:\n");
                break;
            }

            case Scope_Kind::MODULE: {
                string_builder_append(sb, "MODULE:\n");
                break;
            }

            case Scope_Kind::PARAMETER: {
                string_builder_append(sb, "PARAMETER:\n");
                break;
            }

            case Scope_Kind::BLOCK: {
                string_builder_append(sb, "BLOCK:\n");
                break;
            }

            case Scope_Kind::AGGREGATE: {
                string_builder_append(sb, "AGGREGATE:\n");
                break;
            }

            case Scope_Kind::STATIC_IF: {
                string_builder_append(sb, "STATIC_IF:\n");
                break;
            }
        }

        indent += 1;

        auto block = &scope->first_block;
        while (block)
        {
            for (int64_t i = 0; i < block->decl_count; i++)
            {
                AST_Declaration *decl = block->declarations[i];
                assert(decl->identifier);
                auto &atom = decl->identifier->atom;
                scope_print_indent(sb, indent);
                string_builder_append(sb, atom.data, atom.length);
                ast_print_scope(sb, decl, indent + 1);
                string_builder_append(sb, "\n");
            }

            block = block->next_block;
        }

        if (scope->kind == Scope_Kind::GLOBAL)
        {
            assert(false); // Print children
        }
    }
}
