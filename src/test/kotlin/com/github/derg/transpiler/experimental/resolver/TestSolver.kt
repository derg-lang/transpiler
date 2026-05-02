package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

// Various helpers for reducing the amount of boilerplate code. These helpers can be used as a shorthand for the kinds
// representing builtin data types.
private val HIR_BOOL = HirKind.Value(HirType.Expression(BOOL_TYPE_NAME.hirIdent()))
private val HIR_INT32 = HirKind.Value(HirType.Expression(INT32_TYPE_NAME.hirIdent()))
private val HIR_INT64 = HirKind.Value(HirType.Expression(INT64_TYPE_NAME.hirIdent()))
private val HIR_FLOAT32 = HirKind.Value(HirType.Expression(FLOAT32_TYPE_NAME.hirIdent()))
private val HIR_FLOAT64 = HirKind.Value(HirType.Expression(FLOAT64_TYPE_NAME.hirIdent()))
private val HIR_STR = HirKind.Value(HirType.Expression(STR_TYPE_NAME.hirIdent()))

private val THIR_BOOL = ThirKind.Value(ThirType.Bool)
private val THIR_INT32 = ThirKind.Value(ThirType.Int32)
private val THIR_INT64 = ThirKind.Value(ThirType.Int64)
private val THIR_FLOAT32 = ThirKind.Value(ThirType.Float32)
private val THIR_FLOAT64 = ThirKind.Value(ThirType.Float64)
private val THIR_STR = ThirKind.Value(ThirType.Str)

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
