
#include "llvm_builder.h"

#include "builtin.h"
#include "bytecode.h"
#include "os.h"
#include "string_builder.h"
#include "temp_allocator.h"

#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/FileSystem.h>

#include <tracy/Tracy.hpp>

namespace Zodiac
{

    LLVM_Builder llvm_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        auto _target_triple = llvm::sys::getDefaultTargetTriple();
        auto target_triple = string_copy(allocator,
                _target_triple.data(), _target_triple.length());

        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;

        if (string_contains(target_triple, "windows"))
        {
            target_platform = Zodiac_Target_Platform::WINDOWS;
        }
        else if (string_contains(target_triple, "linux"))
        {
            target_platform = Zodiac_Target_Platform::LINUX;
        }
        else assert(false);

        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();

        LLVM_Builder result = {};
        result.allocator = allocator;
        result.build_data = build_data;
        result.target_triple = target_triple;
        result.target_platform = target_platform;

        result.llvm_context = new llvm::LLVMContext();
        result.llvm_module = new llvm::Module("root_module", *result.llvm_context);
        result.llvm_builder = new llvm::IRBuilder<>(*result.llvm_context);
        result.llvm_datalayout = new llvm::DataLayout(result.llvm_module);

        stack_init(allocator, &result.arg_stack);

        array_init(allocator, &result.struct_types_to_finalize);
        array_init(allocator, &result.union_types_to_finalize);
        array_init(allocator, &result.registered_functions);
        array_init(allocator, &result.globals);
        array_init(allocator, &result.string_literals);
        array_init(allocator, &result.blocks);
        array_init(allocator, &result.parameters);
        array_init(allocator, &result.locals);
        array_init(allocator, &result.temps);

        return result;
    }

    void llvm_register_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        if (bc_func->flags & BC_FUNC_FLAG_LLVM_REGISTERED) return;

        assert(!llvm_find_function(builder, bc_func));

        // if (builder->build_data->options->verbose)
        //     printf("LLVM Registering function: %s\n", bc_func->name.data);

        auto llvm_func_type = llvm_type_from_ast<llvm::FunctionType>(builder, bc_func->type);

        String name;
        if (bc_func->name_prefix.length) {
            auto ta = temp_allocator_get();
            name = string_append(ta, string_ref(bc_func->name_prefix), string_ref(bc_func->name));
        } else {
            name = string_ref(bc_func->name);
        }


        llvm::Function *llvm_func = llvm::Function::Create(
                llvm_func_type,
                llvm::GlobalValue::ExternalLinkage,
                name.data,
                builder->llvm_module);

        array_append(&builder->registered_functions, { bc_func, llvm_func });
        bc_func->flags |= BC_FUNC_FLAG_LLVM_REGISTERED;
    }

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        // if (builder->build_data->options->verbose)
        //     printf("LLVM Emitting function: %s\n", bc_func->name.data);

        auto llvm_func = llvm_find_function(builder, bc_func);

        if (builder->build_data->options->verbose)
            if (!llvm_func) printf("LLVM Did not find function: %s!!\n", bc_func->name.data);

        assert(llvm_func);

        if (bc_func->flags & BC_FUNC_FLAG_FOREIGN) {
            bc_func->flags |= BC_FUNC_FLAG_EMITTED;
            return;
        }

        builder->blocks.count = 0;
        builder->parameters.count = 0;
        builder->locals.count = 0;
        builder->temps.count = 0;

        for (int64_t i = 0; i < bc_func->blocks.count; i++) {
            auto block = bc_func->blocks[i];;
            auto llvm_block = llvm::BasicBlock::Create(*builder->llvm_context, block->name.data,
                                                       llvm_func);

            array_append(&builder->blocks, { block, llvm_block });
        }

        auto llvm_block_it = llvm_func->begin();

        builder->llvm_builder->SetInsertPoint(&*llvm_block_it);

        for (int64_t i = 0; i < bc_func->parameters.count; i++) {
            auto param = bc_func->parameters[i];
            auto llvm_param_type = llvm_type_from_ast(builder, param->type->pointer.base);
            auto name = param->parameter.name;

            auto param_alloca = builder->llvm_builder->CreateAlloca(llvm_param_type, nullptr,
                    name.data);
            array_append(&builder->parameters, param_alloca);
        }

        for (int64_t i = 0; i < bc_func->locals.count; i++) {
            auto local = bc_func->locals[i];
            auto name = local->allocl.name;
            llvm::Type *ty = llvm_type_from_ast(builder, local->type->pointer.base);
            llvm::AllocaInst *alloca = builder->llvm_builder->CreateAlloca(ty, nullptr,
                                                                           name.data);

            array_append(&builder->locals, alloca);
        }

        for (int64_t i = 0; i < bc_func->parameters.count; i++) {
            auto param_val = llvm_func->getArg(i);
            auto param_alloca = builder->parameters[i];
            builder->llvm_builder->CreateStore(param_val, param_alloca);
        }

        auto inst_loc = bucket_array_first(&bc_func->instructions);

        for (int64_t i = 0; i < bc_func->blocks.count; i++) {
            builder->llvm_builder->SetInsertPoint(&*llvm_block_it);

            auto block = bc_func->blocks[i];

            for (int64_t j = 0; j < block->instruction_count; j++) {

                Bytecode_Instruction *inst = bucket_locator_get_ptr(inst_loc);
                llvm_emit_instruction(builder, inst);
                bucket_locator_advance(&inst_loc);
            }

            llvm_block_it++;
        }

        bc_func->flags |= BC_FUNC_FLAG_EMITTED;
    }

    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Global_Info global_info)
    {

        llvm::Type *llvm_type = llvm_type_from_ast(builder, global_info.declaration->type);

        llvm::Constant *llvm_init_val = nullptr;
        if (global_info.has_initializer) {
            llvm_init_val = llvm_emit_constant(builder, global_info.init_const_val);
        } else {
            llvm_init_val = llvm::Constant::getNullValue(llvm_type);
        }

        auto name = global_info.global_value->global.name.data;

        llvm::GlobalVariable *llvm_glob =
            new llvm::GlobalVariable(*builder->llvm_module, llvm_type,
                                     false, // Constant
                                     llvm::GlobalVariable::PrivateLinkage,
                                     llvm_init_val,
                                     name);

        global_info.global_value->global.index = builder->globals.count;
        array_append(&builder->globals, llvm_glob);
    }

    llvm::Constant *llvm_emit_constant(LLVM_Builder *builder, Const_Value const_val)
    {
        auto type = const_val.type;

        assert(type->kind == AST_Type_Kind::INTEGER);

        llvm::Type *llvm_type = llvm_type_from_ast(builder, type);

        bool sign = type->integer.sign;

        switch (type->bit_size) {
            case 8:  return llvm::ConstantInt::get(llvm_type, const_val.integer.u8, sign);
            case 16:  return llvm::ConstantInt::get(llvm_type, const_val.integer.u16, sign);
            case 32:  return llvm::ConstantInt::get(llvm_type, const_val.integer.u32, sign);
            case 64:  return llvm::ConstantInt::get(llvm_type, const_val.integer.u64, sign);
            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void llvm_emit_instruction(LLVM_Builder *builder, Bytecode_Instruction *inst)
    {
        llvm::Value *result = nullptr;

        switch (inst->op) {
            case NOP: assert(false);

            case ALLOCL: {
                break;
            }

            case STOREL: {
                auto alloca = llvm_emit_value<llvm::AllocaInst>(builder, inst->a);
                llvm::Value *new_val = llvm_emit_value(builder, inst->b);
                builder->llvm_builder->CreateStore(new_val, alloca);
                break;
            }

            case STORE_ARG: {
                auto alloca = llvm_emit_value<llvm::AllocaInst>(builder, inst->a);
                llvm::Value *new_val = llvm_emit_value(builder, inst->b);
                builder->llvm_builder->CreateStore(new_val, alloca);
                break;
            }

            case STORE_GLOBAL: {
                auto glob = llvm_emit_value<llvm::GlobalVariable>(builder, inst->a);
                llvm::Value *new_val = llvm_emit_value(builder, inst->b);
                builder->llvm_builder->CreateStore(new_val, glob);
                break;
            }

            case STORE_PTR: {
                llvm::Value *ptr_val = llvm_emit_value(builder, inst->a);
                llvm::Value *new_val = llvm_emit_value(builder, inst->b);
                builder->llvm_builder->CreateStore(new_val, ptr_val);
                break;
            }

            case LOADL: {
                auto alloca = llvm_emit_value<llvm::AllocaInst>(builder, inst->a);
                result = builder->llvm_builder->CreateLoad(alloca);
                break;
            }

            case LOAD_PARAM: {
                auto param = llvm_emit_value<llvm::AllocaInst>(builder, inst->a);
                result = builder->llvm_builder->CreateLoad(param);
                break;
            }

            case LOAD_GLOBAL: {
                auto glob = llvm_emit_value<llvm::GlobalVariable>(builder, inst->a);
                result = builder->llvm_builder->CreateLoad(glob);
                break;
            }

            case LOAD_PTR: {
                auto ptr = llvm_emit_value(builder, inst->a);
                result = builder->llvm_builder->CreateLoad(ptr);
                break;
            }

            case ADD_S:
            case ADD_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateAdd(lhs, rhs, "");
                break;
            }

            case ADD_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateFAdd(lhs, rhs, "");
                break;
            }

            case SUB_S:
            case SUB_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateSub(lhs, rhs, "");
                break;
            }

            case SUB_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateFSub(lhs, rhs, "");
                break;
            }

            case REM_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateSRem(lhs, rhs, "");
                break;
            }

            case REM_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateURem(lhs, rhs, "");
                break;
            }

            case MUL_S:
            case MUL_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateMul(lhs, rhs, "");
                break;
            }

            case MUL_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateFMul(lhs, rhs, "");
                break;
            }

            case DIV_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateSDiv(lhs, rhs, "");
                break;
            }

            case DIV_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateUDiv(lhs, rhs, "");
                break;
            }

            case DIV_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);

                result = builder->llvm_builder->CreateFDiv(lhs, rhs, "");
                break;
            }

            case EQ_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpEQ(lhs, rhs, "");
                break;
            }

            case NEQ_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpNE(lhs, rhs, "");
                break;
            }

            case LT_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpSLT(lhs, rhs, "");
                break;
            }

            case LTEQ_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpSLE(lhs, rhs, "");
                break;
            }

            case GT_S: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpSGT(lhs, rhs, "");
                break;
            }

            case GTEQ_S: assert(false);

            case EQ_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpEQ(lhs, rhs, "");
                break;
            }

            case NEQ_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpNE(lhs, rhs);
                break;
            }

            case LT_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpULT(lhs, rhs);
                break;
            }

            case LTEQ_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpULE(lhs, rhs);
                break;
            }

            case GT_U: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateICmpUGT(lhs, rhs);
                break;
            }

            case GTEQ_U: assert(false);

            case EQ_F:
            {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateFCmpOEQ(lhs, rhs);
                break;
            }

            case NEQ_F: assert(false);

            case LT_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateFCmpOLT(lhs, rhs);
                break;
            }

            case LTEQ_F: assert(false);

            case GT_F: {
                auto lhs = llvm_emit_value(builder, inst->a);
                auto rhs = llvm_emit_value(builder, inst->b);
                result = builder->llvm_builder->CreateFCmpOGT(lhs, rhs);
                break;
            }

            case GTEQ_F: assert(false);

            case NEG_LOG: {
                llvm::Value *op_val = llvm_emit_value(builder, inst->a);
                if (op_val->getType()->isPointerTy())  {
                    llvm::Type *int_type = llvm::Type::getIntNTy(*builder->llvm_context,
                                                                 Builtin::pointer_size);
                    op_val = builder->llvm_builder->CreatePtrToInt(op_val, int_type);
                    llvm::Value *zero_val = llvm::Constant::getNullValue(op_val->getType());
                    result = builder->llvm_builder->CreateICmpEQ(op_val, zero_val);
                } else if (op_val->getType()->isIntegerTy()){
                    llvm::Value *zero_val = llvm::Constant::getNullValue(op_val->getType());
                    result = builder->llvm_builder->CreateICmpEQ(op_val, zero_val);
                } else {
                    assert(false && !"untested...");
                    result = builder->llvm_builder->CreateNot(op_val);
                }
                break;
            }

            case PUSH_ARG: {
                llvm::Value *arg_val = llvm_emit_value(builder, inst->a);
                stack_push(&builder->arg_stack, arg_val);
                break;
            }

            case CALL: {
                Bytecode_Function *bc_func = inst->a->function;
                llvm::Function *callee = llvm_find_function(builder, bc_func);
                assert(callee);

                auto bc_arg_count = inst->b;
                assert(bc_arg_count->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                assert(bc_arg_count->type == Builtin::type_s64);

                uint64_t arg_count = bc_arg_count->integer_literal.u64;

                Array<llvm::Value *> _llvm_args = {};
                array_init(builder->allocator, &_llvm_args, arg_count);

                for (uint64_t i = 0; i < arg_count; i++) {
                    int64_t offset = (arg_count - 1) - i;
                    llvm::Value *arg = stack_peek(&builder->arg_stack, offset);
                    array_append(&_llvm_args, arg);
                }

                for (uint64_t i = 0; i < arg_count; i++) stack_pop(&builder->arg_stack);

                llvm::ArrayRef<llvm::Value *> llvm_args(_llvm_args.data, arg_count);

                llvm::Value *return_val = builder->llvm_builder->CreateCall(callee, llvm_args, "");

                if (inst->result) {
                    result = return_val;
                }

                if (bc_func->flags & BC_FUNC_FLAG_NORETURN) {
                    builder->llvm_builder->CreateUnreachable();
                }
                break;
            }

            case RETURN: {
                llvm::Value *ret_val = llvm_emit_value(builder, inst->a);
                builder->llvm_builder->CreateRet(ret_val);
                break;
            }

            case RETURN_VOID: {
                builder->llvm_builder->CreateRetVoid();
                break;
            }

            case JUMP: {
                auto block_val = inst->a;
                assert(block_val->kind == Bytecode_Value_Kind::BLOCK);

                auto block = block_val->block;
                llvm::BasicBlock *llvm_block = llvm_find_block(builder, block);

                builder->llvm_builder->CreateBr(llvm_block);
                break;
            }

            case JUMP_IF:
            {
                llvm::Value *cond_val = llvm_emit_value(builder, inst->a);

                assert(inst->b->kind == Bytecode_Value_Kind::BLOCK);
                assert(inst->result->kind == Bytecode_Value_Kind::BLOCK);

                llvm::BasicBlock *then_block = llvm_find_block(builder, inst->b->block);
                llvm::BasicBlock *else_block = llvm_find_block(builder, inst->result->block);

                builder->llvm_builder->CreateCondBr(cond_val, then_block, else_block);
                break;
            }

            case SWITCH: {
                llvm::Value *switch_val = llvm_emit_value(builder, inst->a);

                assert(inst->b->kind == Bytecode_Value_Kind::SWITCH_DATA);
                Bytecode_Switch_Data *switch_data = &inst->b->switch_data;

                assert(switch_data->default_block);
                assert(switch_val);

                llvm::BasicBlock *default_block = llvm_find_block(builder,
                                                                  switch_data->default_block);

                auto switch_inst = builder->llvm_builder->CreateSwitch(switch_val, default_block,
                                                                       switch_data->cases.count);

                for (int64_t i = 0; i < switch_data->cases.count; i++) {
                    Bytecode_Switch_Case switch_case = switch_data->cases[i];
                    if (switch_case.target_block == switch_data->default_block) continue;

                    llvm::Value *_case_value = llvm_emit_value(builder, switch_case.case_value);
                    assert(_case_value->getType()->isIntegerTy());
                    llvm::ConstantInt *case_value =
                        llvm::dyn_cast<llvm::ConstantInt>(_case_value);

                    llvm::BasicBlock *dest_block = llvm_find_block(builder,
                                                                   switch_case.target_block);

                    switch_inst->addCase(case_value, dest_block);
                }
                break;
            }

            case PTR_OFFSET: {
                llvm::Value *ptr_val = llvm_emit_value(builder, inst->a);
                llvm::Value *offset_val = llvm_emit_value(builder, inst->b);

                size_t index_count = 1;
                llvm::Value *indices[2] = {};

                auto ptr_type = static_cast<llvm::PointerType*>(ptr_val->getType());
                if (ptr_type->getElementType()->isArrayTy()) {
                    llvm::Value *zero_val = llvm::Constant::getNullValue(offset_val->getType());
                    indices[0] = zero_val;
                    indices[1] = offset_val;
                    index_count = 2;
                } else {
                    indices[0] = offset_val;
                }

                result = builder->llvm_builder->CreateGEP(ptr_val, { indices, index_count }, "");
                break;
            }

            case AGG_OFFSET: {
                llvm::Value *ptr_val = llvm_emit_value(builder, inst->a);
                llvm::Value *index_val = llvm_emit_value(builder, inst->b);

                llvm::Value *zero_val = llvm::Constant::getNullValue(index_val->getType());
                llvm::Value *indices[2] = { zero_val, index_val };

                result = builder->llvm_builder->CreateGEP(ptr_val, { indices, 2 }, "");
                break;
            }

            case ZEXT: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);
                result = builder->llvm_builder->CreateCast(llvm::Instruction::CastOps::ZExt,
                                                           operand_value, dest_type, "");
                break;
            }

            case SEXT: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);
                result = builder->llvm_builder->CreateCast(llvm::Instruction::CastOps::SExt,
                                                           operand_value, dest_type, "");
                break;
            }

            case TRUNC: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);
                result = builder->llvm_builder->CreateCast(llvm::Instruction::CastOps::Trunc,
                                                           operand_value, dest_type);
                break;
            }

            case F_TO_S: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isFloatingPointTy());
                assert(dest_type->isIntegerTy());

                result = builder->llvm_builder->CreateFPToSI(operand_value, dest_type);
                break;
            }

            case S_TO_F: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isIntegerTy());
                assert(dest_type->isFloatingPointTy());

                result = builder->llvm_builder->CreateSIToFP(operand_value, dest_type);
                break;
            }

            case U_TO_F: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isIntegerTy());
                assert(dest_type->isFloatingPointTy());

                result = builder->llvm_builder->CreateUIToFP(operand_value, dest_type);
                break;
            }

            case F_TO_F: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isFloatingPointTy());
                assert(dest_type->isFloatingPointTy());

                result = builder->llvm_builder->CreateFPCast(operand_value, dest_type);
                break;
            }

            case PTR_TO_INT: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isPointerTy());
                assert(dest_type->isIntegerTy());

                result = builder->llvm_builder->CreatePtrToInt(operand_value, dest_type);
                break;
            }

            case PTR_TO_PTR: {
                llvm::Value *operand_value = llvm_emit_value(builder, inst->a);
                llvm::Type *dest_type = llvm_type_from_ast(builder, inst->result->type);

                assert(operand_value->getType()->isPointerTy());
                assert(dest_type->isPointerTy());

                result = builder->llvm_builder->CreatePointerCast(operand_value, dest_type);
                break;
            }

            case SIZEOF: {
                assert(inst->a->kind == Bytecode_Value_Kind::TYPE);

                assert(inst->a->type->bit_size % 8 == 0);
                auto bc_size = inst->a->type->bit_size / 8;

                llvm::Type *llvm_type = llvm_type_from_ast(builder, inst->a->type);
                auto llvm_size = builder->llvm_datalayout->getTypeAllocSize(llvm_type);
                assert(llvm_size == bc_size);
                if (llvm_size != bc_size) {
                    fprintf(stderr, "Bytecode and llvm sizeof don't match!!!\n");
                    exit(-1);
                }

                llvm::Type *result_type = llvm_type_from_ast(builder, inst->result->type);

                result = llvm::ConstantInt::get(result_type, llvm_size, true);
                break;
            }

            case OFFSETOF: {
                assert(inst->a->kind == Bytecode_Value_Kind::TYPE);
                AST_Type *struct_type = inst->a->type;
                assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                assert(inst->b->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                assert(inst->b->type == Builtin::type_s64);

                auto llvm_type = llvm_type_from_ast<llvm::StructType>(builder, struct_type);

                auto index = inst->b->integer_literal.s64;

                const llvm::StructLayout *struct_layout =
                    builder->llvm_datalayout->getStructLayout(llvm_type);
                int64_t offset = struct_layout->getElementOffset(index);

#ifndef NDEBUG
                int64_t bc_offset = 0;
                for (int64_t i = 0; i < index; i++) {
                    auto bit_size = struct_type->structure.member_types[i]->bit_size;
                    assert(bit_size % 8 == 0);
                    bc_offset += (bit_size / 8);
                }
                assert(offset == bc_offset);
#endif

                llvm::Type *result_type = llvm_type_from_ast(builder, inst->result->type);
                result = llvm::ConstantInt::get(result_type, offset, true);
                break;
            }

            case EXIT: {
                llvm::Value *exit_code_val = llvm_emit_value(builder, inst->a);
                llvm_emit_exit(builder, exit_code_val);
                break;
            }

            case SYSCALL: {
                assert(inst->a->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                assert(inst->a->type == Builtin::type_u64);

                llvm::Value *syscall_ret = llvm_emit_syscall(builder,
                                                             inst->a->integer_literal.u64);
                if (inst->result) {
                    assert(syscall_ret);
                    result = syscall_ret;
                }
                break;
            }
        }

        if (inst->result) {
            if (inst->op == ALLOCL) {
                assert(inst->result->kind == Bytecode_Value_Kind::ALLOCL);
            } else if (inst->op == JUMP_IF) {
                assert(inst->result->kind == Bytecode_Value_Kind::BLOCK);
            } else {
                assert(inst->result->kind == Bytecode_Value_Kind::TEMP);
                assert(result);

                assert(builder->temps.count == inst->result->temp.index);
                array_append(&builder->temps, result);
            }
        }
    }

    llvm::Value *llvm_emit_value(LLVM_Builder *builder, Bytecode_Value *bc_value)
    {
        switch (bc_value->kind) {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::INTEGER_LITERAL:
            {
                auto type = bc_value->type;
                llvm::Type *llvm_type = llvm_type_from_ast(builder, type);
                if (type->integer.sign)
                    return llvm::ConstantInt::get(llvm_type, bc_value->integer_literal.s64, true);
                else
                    return llvm::ConstantInt::get(llvm_type, bc_value->integer_literal.s64, false);
                break;
            }

            case Bytecode_Value_Kind::FLOAT_LITERAL: {
                auto type = bc_value->type;
                llvm::Type *llvm_type = llvm_type_from_ast(builder, type);
                if (type == Builtin::type_float) {
                    return llvm::ConstantFP::get(llvm_type, bc_value->float_literal.r32);
                } else if (type == Builtin::type_double) {
                    return llvm::ConstantFP::get(llvm_type, bc_value->float_literal.r64);
                }
                else {
                    assert(false);
                }
                break;
            }

            case Bytecode_Value_Kind::STRING_LITERAL:
            {
                llvm::GlobalValue *llvm_str_glob = nullptr;

                for (int64_t i = 0; i < builder->string_literals.count; i++) {
                    auto &sl_info = builder->string_literals[i];

                    if (sl_info.bc_value == bc_value) {
                        llvm_str_glob = sl_info.llvm_global;
                        break;
                    }
                }

                if (!llvm_str_glob) {
                    Atom str = bc_value->string_literal;
                    llvm::Constant *llvm_str =
                        llvm::ConstantDataArray::getString(*builder->llvm_context,
                                                           { str.data, str.length },
                                                           true);

                    llvm_str_glob = new llvm::GlobalVariable(*builder->llvm_module,
                                                             llvm_str->getType(),
                                                             true, // Constant
                                                             llvm::GlobalVariable::PrivateLinkage,
                                                             llvm_str,
                                                             "_string_const");

                    llvm_str_glob->setUnnamedAddr(llvm::GlobalVariable::UnnamedAddr::Global);
                    auto alignment = llvm::MaybeAlign(1);
                    static_cast<llvm::GlobalObject*>(llvm_str_glob)->setAlignment(alignment);

                    array_append(&builder->string_literals, { bc_value, llvm_str_glob });
                }

                llvm::Type *dest_type = llvm_type_from_ast(builder, Builtin::type_ptr_u8);
                llvm::Value *llvm_str_ptr = llvm::ConstantExpr::getPointerCast(llvm_str_glob,
                                                                               dest_type);

                return llvm_str_ptr;
                break;
            }

            case Bytecode_Value_Kind::BOOL_LITERAL: {
                llvm::Type *llvm_type = llvm_type_from_ast(builder, bc_value->type);
                return llvm::ConstantInt::get(llvm_type, bc_value->bool_literal, false);
                break;
            }

            case Bytecode_Value_Kind::NULL_LITERAL: {
                auto type = llvm_type_from_ast<llvm::PointerType>(builder, bc_value->type);
                return llvm::ConstantPointerNull::get(type);
                break;
            }

            case Bytecode_Value_Kind::TEMP: {
                assert(bc_value->temp.index < builder->temps.count);
                return builder->temps[bc_value->temp.index];
                break;
            }

            case Bytecode_Value_Kind::ALLOCL: {
                assert(bc_value->allocl.index < builder->locals.count);
                return builder->locals[bc_value->allocl.index];
                break;
            }

            case Bytecode_Value_Kind::PARAM: {
                assert(bc_value->allocl.index < builder->parameters.count);
                return builder->parameters[bc_value->parameter.index];
                break;
            }

            case Bytecode_Value_Kind::GLOBAL: {
                assert(bc_value->global.index < builder->globals.count);
                return builder->globals[bc_value->global.index];
                break;
            }

            case Bytecode_Value_Kind::FUNCTION: assert(false);
            case Bytecode_Value_Kind::BLOCK: assert(false);
            case Bytecode_Value_Kind::TYPE: assert(false);
            case Bytecode_Value_Kind::SWITCH_DATA: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void llvm_emit_exit(LLVM_Builder *builder, llvm::Value *exit_code_val)
    {
        switch (builder->target_platform)
        {
            case Zodiac_Target_Platform::INVALID: assert(false);

            case Zodiac_Target_Platform::LINUX:
            {
                auto ta = temp_allocator_get();

                auto asm_string = string_ref("syscall");

                String_Builder sb = {};
                string_builder_init(ta, &sb);

                string_builder_append(&sb, "=r,{rax},{rdi}");

                llvm::FunctionType *asm_fn_type = llvm_asm_function_type(builder, 2);

                String constraint_string = string_builder_to_string(sb.allocator, &sb);

                llvm::Value *asm_val =
                    llvm::InlineAsm::get(asm_fn_type,
                                         {asm_string.data, (size_t)asm_string.length},
                                         {constraint_string.data, (size_t)constraint_string.length},
                                         true, false, llvm::InlineAsm::AD_ATT);

                llvm::Type *arg_type = llvm_type_from_ast(builder, Builtin::type_s64);
                auto syscall_num = llvm::ConstantInt::get(arg_type, 60, true);

                llvm::Value *args[2] = { syscall_num, exit_code_val };

                builder->llvm_builder->CreateCall(asm_fn_type, asm_val, { args, 2 });
                builder->llvm_builder->CreateUnreachable();
                break;
            }

            case Zodiac_Target_Platform::WINDOWS:
            {
                llvm::Value *exitprocess_func = builder->llvm_module->getFunction("ExitProcess");
                assert(exitprocess_func);

                auto fn_ptr_type = exitprocess_func->getType();
                assert(fn_ptr_type->isPointerTy());
                auto fn_type =
                    static_cast<llvm::FunctionType *>(fn_ptr_type->getPointerElementType());

                auto arg_type = llvm_type_from_ast(builder, Builtin::type_u32);
                exit_code_val = builder->llvm_builder->CreateIntCast(exit_code_val, arg_type,
                                                                     false);
                builder->llvm_builder->CreateCall(fn_type, exitprocess_func,
                                                  { &exit_code_val, 1});
                builder->llvm_builder->CreateUnreachable();
                break;
            }
        }
    }

    llvm::Value *llvm_emit_syscall(LLVM_Builder *builder, uint64_t arg_count)
    {
        assert(arg_count >= 1 && arg_count <= 7);

        auto asm_string = string_ref("syscall");

        String_Builder sb = {};
        string_builder_init(builder->allocator, &sb);

        string_builder_append(&sb, "=r,{rax}");

        if (arg_count >= 2) string_builder_append(&sb, ",{rdi}");
        if (arg_count >= 3) string_builder_append(&sb, ",{rsi}");
        if (arg_count >= 4) string_builder_append(&sb, ",{rdx}");
        if (arg_count >= 5) string_builder_append(&sb, ",{r10}");
        if (arg_count >= 6) string_builder_append(&sb, ",{r9}");
        if (arg_count >= 7) string_builder_append(&sb, ",{r8}");

        llvm::FunctionType *asm_fn_type = llvm_asm_function_type(builder, arg_count);

        Array<llvm::Value *> llvm_args = {};
        array_init(builder->allocator, &llvm_args, arg_count);

        for (uint64_t i = 0 ; i < arg_count; i++) {
            llvm::Value *arg_val = stack_peek(&builder->arg_stack, (arg_count - 1) - i);

            llvm::Type *arg_type = arg_val->getType();
            llvm::Type *dest_type = llvm_type_from_ast(builder, Builtin::type_s64);

            if (arg_type != dest_type) {
                if (arg_type->isPointerTy()) {
                    arg_val = builder->llvm_builder->CreatePtrToInt(arg_val, dest_type,
                                                                     "");
                } else {
                    assert(false);
                }

            }
            array_append(&llvm_args, arg_val);
        }

        for (uint64_t i = 0; i < arg_count; i++) stack_pop(&builder->arg_stack);

        auto constraint_str = string_builder_to_string(builder->allocator, &sb);

        llvm::Value *asm_val =
            llvm::InlineAsm::get(asm_fn_type,
                                 { asm_string.data, (size_t)asm_string.length },
                                 { constraint_str.data, (size_t)constraint_str.length },
                                 true, false, llvm::InlineAsm::AD_ATT);

        llvm::Value *result =
            builder->llvm_builder->CreateCall(asm_fn_type, asm_val,
                                               { llvm_args.data,
                                                 (size_t)llvm_args.count });

        assert(result);

        free(builder->allocator, constraint_str.data);

        array_free(&llvm_args);


        string_builder_free(&sb);

        return result;
    }

    bool llvm_emit_binary(LLVM_Builder *builder, const char * output_file_name)
    {
        ZoneScoped

        assert(builder);
        assert(output_file_name);

        auto options = builder->build_data->options;

        // if (options->verbose) {
        //     printf("Emitting binary: %s\n", output_file_name);
        // }

        // @TODO: @CLEANUP: This could be done by comparing a count I think?
        for (int64_t i = 0; i < builder->registered_functions.count; i++)
        {
            auto &func = builder->registered_functions[i].bytecode_function;

            if (!(func->flags & BC_FUNC_FLAG_EMITTED))
            {
                if (func->flags & BC_FUNC_FLAG_FOREIGN) {
                    continue;
                }

                if ((func->flags & BC_FUNC_FLAG_CRT_ENTRY) &&
                    options->link_c) {
                    continue;
                }

                printf("Trying to emit binary, but bytecode for function '%s' has not been emitted!!!\n",
                                func->name.data);
                return false;
            }
        }

        if (options->print_llvm) {
            builder->llvm_module->print(llvm::outs(), nullptr, false, true);
        }

        bool verify_error = llvm::verifyModule(*builder->llvm_module, &llvm::errs());

        if (verify_error) {
            assert(false);
        }

        builder->llvm_module->setTargetTriple(builder->target_triple.data);
        std::string error;
        const llvm::Target *llvm_target =
            llvm::TargetRegistry::lookupTarget(builder->target_triple.data, error);
        assert(llvm_target);

        auto cpu = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto rm = llvm::Optional<llvm::Reloc::Model>();

        llvm::TargetMachine *llvm_target_machine =
            llvm_target->createTargetMachine(builder->target_triple.data,
                                             cpu, features, opt, rm);

        builder->llvm_module->setDataLayout( llvm_target_machine->createDataLayout());

        String_Builder sb = {};
        string_builder_init(builder->allocator, &sb);
        string_builder_appendf(&sb, "%s.o", output_file_name);
        auto obj_file_name = string_builder_to_string(builder->allocator, &sb);
        string_builder_free(&sb);


        { ZoneScopedN("LLVMTargetMachineEmitToFile")

        std::error_code err_code;
        llvm::raw_fd_ostream dest(obj_file_name.data, err_code, llvm::sys::fs::OF_None);
        if (err_code)
        {
            fprintf(stderr, "Could not open file: %s\n", obj_file_name.data);
            assert(false);
        }

        llvm::legacy::PassManager pass;
        auto filetype = llvm::CGFT_ObjectFile;
        if (llvm_target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype))
        {
            fprintf(stderr, "TargetMachine can't emit a file of this type");
            assert(false);
        }

        pass.run(*builder->llvm_module);


        }

        free(builder->allocator, obj_file_name.data);

        delete llvm_target_machine;

        return llvm_run_linker(builder, output_file_name);
    }

    bool llvm_run_linker(LLVM_Builder *builder, const char *output_file_name)
    {
        ZoneScopedNCS("llvm_run_linker", 0x00ffff, 32);

        assert(output_file_name);

        String_Builder _sb = {};
        auto sb = &_sb;
        string_builder_init(builder->allocator, sb);

        auto options = builder->build_data->options;
        bool print_command = options->print_link_command || options->verbose;

#if linux

        String crt_path;

        if (options->link_c) {
            string_builder_append(sb, "ld ");
            string_builder_append(sb, "-dynamic-linker /lib64/ld-linux-x86-64.so.2 ");

            bool crt_found = linux_find_crt_path(&crt_path);
            assert(crt_found);
            if (!crt_found) return false;

            string_builder_append(sb, crt_path.data);
            string_builder_append(sb, "crt1.o ");
            string_builder_append(sb, crt_path.data);
            string_builder_appendf(sb, "crti.o ");

            string_builder_append(sb, "-lc ");
        } else {

            string_builder_appendf(sb, "ld -static -nostdlib ");
        }

        string_builder_appendf(sb, " %s.o -o %s ", output_file_name, output_file_name);

        if (options->link_c) {
            string_builder_append(sb, crt_path.data);
            string_builder_append(sb, "crtn.o ");
        }

        auto link_cmd = string_builder_to_string(builder->allocator, sb);
        if (print_command) printf("Running linker: %s\n", link_cmd.data);

        char out_buf[1024];
        FILE *link_process_handle = popen(link_cmd.data, "r");
        assert(link_process_handle);

        bool result = true;
        while (fgets(out_buf, sizeof(out_buf), link_process_handle) != nullptr) {
            fprintf(stderr, "%s", out_buf);
        }
        assert(feof(link_process_handle));

        int close_ret = pclose(link_process_handle);
        close_ret = WEXITSTATUS(close_ret);
        assert(close_ret >= 0);

        if (close_ret != 0) {
            result = false;
            fprintf(stderr, "Link command failed with exit code: %d\n", close_ret);
            builder->build_data->link_error = true;
        }

        string_builder_free(sb);
        return result;

#elif _WIN32

        auto ta = temp_allocator_get();

        auto &sdk_info = builder->build_data->sdk_info;

        auto vs_exe_path = unicode_string_ref(sdk_info.vs_exe_path);
        auto wide_linker_path = string_append(ta, vs_exe_path, L"\\link.exe");
        auto linker_path = narrow(ta, wide_linker_path);

        string_builder_append(sb, linker_path.data);

        // string_builder_append(sb, " /nologo /wx /subsystem:CONSOLE ");
        string_builder_append(sb, " /nologo /wx /subsystem:CONSOLE /nodefaultlib");

        auto wide_um_lib_path = unicode_string_ref(sdk_info.windows_sdk_um_library_path);
        auto um_lib_path = narrow(ta, wide_um_lib_path);

        // printf("um_lib_path: %s\n", um_lib_path.data);
        // printf("strlen(um_lib_path.data): %llu\n", strlen(um_lib_path.data));
        // assert(um_lib_path.data[um_lib_path.length] == 0);
        assert(um_lib_path.length == strlen(um_lib_path.data));

        // @TODO: @FIXME: Find out why we crash when we don't pass a length here,
        //  um_lib_path.data should be a null terminated cstring.
        string_builder_appendf(sb, " /libpath:\"%.*s\"", (int)um_lib_path.length,
                               um_lib_path.data);

        auto wide_ucrt_lib_path = unicode_string_ref(sdk_info.windows_sdk_ucrt_library_path);
        auto ucrt_lib_path = narrow(ta, wide_ucrt_lib_path);
        assert(ucrt_lib_path.length == strlen(ucrt_lib_path.data));
        string_builder_appendf(sb, " /libpath:\"%.*s\"", (int)ucrt_lib_path.length,
                               ucrt_lib_path.data);

        auto wide_vs_lib_path = unicode_string_ref(sdk_info.vs_library_path);
        auto vs_lib_path = narrow(ta, wide_vs_lib_path);
        assert(vs_lib_path.length == strlen(vs_lib_path.data));
        string_builder_appendf(sb, " /libpath:\"%.*s\"", (int)vs_lib_path.length,
                               vs_lib_path.data);

        string_builder_append(sb, " kernel32.lib");
        string_builder_append(sb, " ucrt.lib");
        string_builder_append(sb, " msvcrt.lib");

        string_builder_appendf(sb, " %s.o", output_file_name);

        if (options->link_c) {
            // string_builder_append(sb, " libcmt.lib");
            string_builder_append(sb, " libvcruntime.lib");
            // string_builder_append(sb, " libucrt.lib");
            string_builder_append(sb, " legacy_stdio_definitions.lib");
            string_builder_append(sb, " legacy_stdio_wide_specifiers.lib");
        }

        auto arg_str = string_builder_to_string(builder->allocator, sb);
        if (print_command) printf("Running link command: %s\n", arg_str.data);

        auto result = execute_process(builder->allocator, {}, arg_str);
        free(builder->allocator, arg_str.data);
        string_builder_free(sb);

        if (!result.success)
        {
            builder->build_data->link_error = true;
        }
        return result.success;
#endif
    }

    llvm::Function *llvm_find_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        for (int64_t i = 0; i < builder->registered_functions.count; i++)
        {
            auto fi = builder->registered_functions[i];
            if (fi.bytecode_function == bc_func)
                return fi.llvm_function;
        }

        return nullptr;
    }

    llvm::BasicBlock *llvm_find_block(LLVM_Builder *builder, Bytecode_Block *bc_block)
    {
        for (int64_t i = 0; i < builder->blocks.count; i++) {
            auto bi = builder->blocks[i];
            if (bi.bytecode_block == bc_block) return bi.llvm_block;
        }

        assert(false);
        return nullptr;
    }

    llvm::Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        assert(ast_type);

        bool is_resolved = true;

        if (!(ast_type->flags & AST_NODE_FLAG_RESOLVED_ID)) {
            is_resolved = false;
            if (ast_type->kind != AST_Type_Kind::STRUCTURE) {
                assert(false && "Cannot create a LLVM type from an unresolved ast type");
            }
        }

        if (is_resolved) assert(ast_type->flags & AST_NODE_FLAG_TYPED);
        if (is_resolved)
            assert(ast_type->flags & AST_NODE_FLAG_SIZED ||
                   ast_type->kind == AST_Type_Kind::FUNCTION);

        auto &c = *builder->llvm_context;

        switch (ast_type->kind) {
            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::VOID: {
                return llvm::Type::getVoidTy(c);
                break;
            }

            case AST_Type_Kind::INTEGER: {
                return llvm::Type::getIntNTy(c, ast_type->bit_size);
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (ast_type->bit_size == 32)
                    return llvm::Type::getFloatTy(c);
                else if (ast_type->bit_size == 64)
                    return llvm::Type::getDoubleTy(c);
                break;
            }

            case AST_Type_Kind::BOOL: {
                return llvm::Type::getIntNTy(c, 1);
                break;
            }

            case AST_Type_Kind::POINTER: {
                if (ast_type->pointer.base == Builtin::type_void) {
                    return llvm_type_from_ast(builder, Builtin::type_ptr_u8);
                } else {
                    llvm::Type *base_type = llvm_type_from_ast(builder, ast_type->pointer.base);
                    return base_type->getPointerTo();
                }
                break;
            }

            case AST_Type_Kind::FUNCTION: {
                llvm::Type *llvm_ret_type = llvm_type_from_ast(builder,
                                                               ast_type->function.return_type);
                Array<llvm::Type *> llvm_arg_types = {};
                if (ast_type->function.param_types.count) {
                    array_init(builder->allocator, &llvm_arg_types,
                               ast_type->function.param_types.count);

                    for (int64_t i = 0; i < ast_type->function.param_types.count; i++) {
                        llvm::Type *arg_type =
                            llvm_type_from_ast(builder, ast_type->function.param_types[i]);
                        array_append(&llvm_arg_types, arg_type);
                    }
                }

                bool is_vararg = false;
                llvm::Type *result =
                    llvm::FunctionType::get(llvm_ret_type,
                                            { llvm_arg_types.data,
                                              (size_t)llvm_arg_types.count },
                                            is_vararg);
                if (llvm_arg_types.count) {
                    array_free(&llvm_arg_types);
                }
                return result;
                break;
            }

            case AST_Type_Kind::STRUCTURE: {
                auto name = ast_type->structure.declaration->identifier->atom;
                llvm::StructType *result = builder->llvm_module->getTypeByName(name.data);

                bool just_created = false;
                if (result) {
                    assert(result->isStructTy());
                } else {
                    result = llvm::StructType::create(c, name.data);
                    just_created = true;
                }

                if (is_resolved) {

                    if (just_created) {
                        llvm_finalize_struct_type(builder, result, ast_type);
                    }

                } else {
                    LLVM_Struct_Type_Info sti = { ast_type, result };
                    array_append(&builder->struct_types_to_finalize, sti);
                }

                return result;
                break;
            }

            case AST_Type_Kind::UNION: {
                auto name = ast_type->union_type.declaration->identifier->atom;
                llvm::StructType *result = builder->llvm_module->getTypeByName(name.data);

                bool just_created = false;
                if (result) {
                    assert(result->isStructTy());
                } else {
                    result = llvm::StructType::create(c, name.data);
                    just_created = true;
                }

                if (is_resolved) {

                    if (just_created) {
                        llvm_finalize_union_type(builder, result, ast_type);
                    }

                } else {
                    LLVM_Struct_Type_Info sti = { ast_type, result };
                    array_append(&builder->union_types_to_finalize, sti);
                }

                return result;
                break;
                break;
            }

            case AST_Type_Kind::ENUM: {
                return llvm_type_from_ast(builder, ast_type->enum_type.base_type);
                break;
            }

            case AST_Type_Kind::ARRAY: {
                llvm::Type *llvm_elem_type = llvm_type_from_ast(builder,
                                                                ast_type->array.element_type);
                return llvm::ArrayType::get(llvm_elem_type,
                                            ast_type->array.element_count);
                break;
            }
        }

        assert(false);
        return {};
    }

    void llvm_finalize_struct_type(LLVM_Builder *builder, llvm::StructType *llvm_type,
                                   AST_Type *ast_type)
    {
        assert(ast_type->kind == AST_Type_Kind::STRUCTURE);

        auto ast_mem_types = ast_type->structure.member_types;
        assert(ast_mem_types.count);
        Array<llvm::Type *> mem_types = {};

        // @FIXME @LEAK: LLVM might need this memory, but i'm not sure atm...
        array_init(builder->allocator, &mem_types, ast_mem_types.count);

        for (int64_t i = 0; i < ast_mem_types.count; i++) {
            array_append(&mem_types, llvm_type_from_ast(builder,
                                                        ast_mem_types[i]));
        }
        llvm_type->setBody({ mem_types.data, (size_t)mem_types.count }, false);
    }

    void llvm_finalize_union_type(LLVM_Builder *builder, llvm::StructType *llvm_type,
                                  AST_Type *ast_type)
    {
        assert(ast_type->kind == AST_Type_Kind::UNION);

        assert(ast_type->union_type.biggest_member_type);

        llvm::Type *llvm_mem_type =
            llvm_type_from_ast(builder, ast_type->union_type.biggest_member_type);

        llvm_type->setBody({ &llvm_mem_type, 1 }, false);
    }

    llvm::FunctionType *llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count)
    {
        assert(arg_count >= 0);
        assert(arg_count < 7);

        auto ret_type = llvm_type_from_ast(builder, Builtin::type_s64);
        llvm::Type *param_types[7] = { ret_type, ret_type, ret_type, ret_type, ret_type, ret_type, ret_type };
        return llvm::FunctionType::get(ret_type, { param_types, (size_t)arg_count },
                                       false);;
    }

    bool llvm_ready_to_emit(LLVM_Builder *builder)
    {

#define REQUIRE_LLVM_FUNC(name) { \
    auto f = builder->llvm_module->getFunction(name); \
    if (!f) return false; \
}
        REQUIRE_LLVM_FUNC("entry.default_assert_handler");

        if (builder->target_platform == Zodiac_Target_Platform::WINDOWS) {
            REQUIRE_LLVM_FUNC("ExitProcess");
        }

#undef REQUIRE_LLVM_FUNC

        return true;
    }
}
