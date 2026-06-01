package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

// Various helpers for reducing the amount of boilerplate code. These helpers can be used as a shorthand for the kinds
// representing builtin data types.
private val HIR_BOOL = HirKind.Value(BOOL_TYPE_NAME.hirIdent())
private val HIR_INT32 = HirKind.Value(INT32_TYPE_NAME.hirIdent())
private val HIR_INT64 = HirKind.Value(INT64_TYPE_NAME.hirIdent())
private val HIR_FLOAT32 = HirKind.Value(FLOAT32_TYPE_NAME.hirIdent())
private val HIR_FLOAT64 = HirKind.Value(FLOAT64_TYPE_NAME.hirIdent())
private val HIR_STR = HirKind.Value(STR_TYPE_NAME.hirIdent())

private val THIR_BOOL = ThirType.Bool
private val THIR_INT32 = ThirType.Int32
private val THIR_INT64 = ThirType.Int64
private val THIR_FLOAT32 = ThirType.Float32
private val THIR_FLOAT64 = ThirType.Float64
private val THIR_STR = ThirType.Str

class TestSolver
{
    private val scope = Builtin.generateScope()
    private val context = Context()
    private val solver = Solver(context, scope)
    
    /**
     * Runs the type-checker on all [constants] provided. All symbols are added to the same module, then the entire
     * module is type-checked as one unit.
     */
    private fun solve(
        constants: List<HirDeclaration.ConstantDecl> = emptyList(),
    ): Result<Unit, Outcome>
    {
        val segment = hirSegmentOf(constants = constants)
        val module = hirModuleOf(segments = listOf(segment))
        
        return solver.solve(module)
    }
    
    @Test
    fun `Given primitive types, when solving, then correctly solved`()
    {
        val cases = listOf(
            THIR_BOOL to false.hir,
            THIR_INT32 to 0.hir,
            THIR_INT64 to 0L.hir,
            THIR_FLOAT32 to 0.0f.hir,
            THIR_FLOAT64 to 0.0.hir,
            THIR_STR to "".hir,
        )
        
        for ((expected, input) in cases)
        {
            val constant = hirConstOf(value = input)
            
            assertSuccess(solve(constants = listOf(constant)))
            assertEquals(expected, context.environment[constant.id])
        }
    }
    
    @Test
    fun `Given primitive operators, when solving, then correctly solved`()
    {
        val cases = listOf(
            // Bool
            THIR_BOOL to (false hirAnd false),
            THIR_BOOL to (false hirOr false),
            THIR_BOOL to (false hirXor false),
            
            // Int32
            THIR_INT32 to (0 hirAdd 0),
            THIR_INT32 to (0 hirDiv 0),
            THIR_INT32 to (0 hirMod 0),
            THIR_INT32 to (0 hirMul 0),
            THIR_INT32 to (0 hirSub 0),
            THIR_BOOL to (0 hirEq 0),
            THIR_BOOL to (0 hirNe 0),
            THIR_BOOL to (0 hirGe 0),
            THIR_BOOL to (0 hirGt 0),
            THIR_BOOL to (0 hirLe 0),
            THIR_BOOL to (0 hirLt 0),
            
            // Int64
            THIR_INT64 to (0L hirAdd 0L),
            THIR_INT64 to (0L hirDiv 0L),
            THIR_INT64 to (0L hirMod 0L),
            THIR_INT64 to (0L hirMul 0L),
            THIR_INT64 to (0L hirSub 0L),
            THIR_BOOL to (0L hirEq 0L),
            THIR_BOOL to (0L hirNe 0L),
            THIR_BOOL to (0L hirGe 0L),
            THIR_BOOL to (0L hirGt 0L),
            THIR_BOOL to (0L hirLe 0L),
            THIR_BOOL to (0L hirLt 0L),
            
            // Float32
            THIR_FLOAT32 to (0.0f hirAdd 0.0f),
            THIR_FLOAT32 to (0.0f hirDiv 0.0f),
            THIR_FLOAT32 to (0.0f hirMod 0.0f),
            THIR_FLOAT32 to (0.0f hirMul 0.0f),
            THIR_FLOAT32 to (0.0f hirSub 0.0f),
            THIR_BOOL to (0.0f hirEq 0.0f),
            THIR_BOOL to (0.0f hirNe 0.0f),
            THIR_BOOL to (0.0f hirGe 0.0f),
            THIR_BOOL to (0.0f hirGt 0.0f),
            THIR_BOOL to (0.0f hirLe 0.0f),
            THIR_BOOL to (0.0f hirLt 0.0f),
            
            // Float64
            THIR_FLOAT64 to (0.0 hirAdd 0.0),
            THIR_FLOAT64 to (0.0 hirDiv 0.0),
            THIR_FLOAT64 to (0.0 hirMod 0.0),
            THIR_FLOAT64 to (0.0 hirMul 0.0),
            THIR_FLOAT64 to (0.0 hirSub 0.0),
            THIR_BOOL to (0.0 hirEq 0.0),
            THIR_BOOL to (0.0 hirNe 0.0),
            THIR_BOOL to (0.0 hirGe 0.0),
            THIR_BOOL to (0.0 hirGt 0.0),
            THIR_BOOL to (0.0 hirLe 0.0),
            THIR_BOOL to (0.0 hirLt 0.0),
            
            // Str
            THIR_STR to ("" hirAdd ""),
        )
        
        for ((expected, input) in cases)
        {
            val constant = hirConstOf(value = input)
            
            assertSuccess(solve(constants = listOf(constant)))
            assertEquals(expected, context.environment[constant.id])
        }
    }
    
    @Nested
    inner class Constants
    {
        @Test
        fun `Given valid, when solving, then symbol is added to environment`()
        {
            val constant = hirConstOf()
            
            assertSuccess(solve(constants = listOf(constant)))
            assertNotNull(context.declarations[constant.id])
        }
        
        @Test
        fun `Given no kind, when solving, then correctly solved`()
        {
            val constant = hirConstOf(kind = HirKind.Deduce, value = 0.hir)
            
            assertSuccess(solve(constants = listOf(constant)))
            assertEquals(THIR_INT32, context.environment[constant.id])
        }
        
        @Test
        fun `Given correct kind, when solving, then correctly solved`()
        {
            val constant = hirConstOf(kind = HIR_INT32, value = 0.hir)
            
            assertSuccess(solve(constants = listOf(constant)))
            assertEquals(THIR_INT32, context.environment[constant.id])
        }
        
        @Test
        fun `Given incorrect kind, when solving, then error`()
        {
            val constant = hirConstOf(kind = HIR_BOOL, value = 0.hir)
            
            assertFailure(solve(constants = listOf(constant)))
        }
    }
}
