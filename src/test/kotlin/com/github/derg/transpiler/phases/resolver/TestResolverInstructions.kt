package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

/**
 * Simulates what a thir call on [this] function, doing the best we can to simulate the proper value and error
 * types.
 */
private fun HirFunction.thirCall(): ThirValue
{
    val type = ThirType.Function(null, null, emptyList())
    
    return ThirCall(null, null, ThirLoad(type, id, emptyList()), emptyList())
}

private infix fun HirVariable.thirAssign(that: Any) =
    ThirAssign(id, that.thir)

class TestResolverInstruction
{
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    private val engine = ResolutionEngine()
    
    /**
     * Generates and registers a new HIR function with the given [name], which returns a [value] and [error] type.
     */
    private fun registerFun(name: String, value: HirType? = null, error: HirType? = null): HirFunction =
        hirFunOf(name = name, value = value, error = error).also(scope::register)
    
    private fun registerVar(name: String, type: HirType? = null): HirVariable =
        hirVarOf(name = name, type = type).also(scope::register)
    
    /**
     * Converts the [instruction] if possible, ensuring that all global scopes are registered into the engine before
     * performing the test.
     */
    private fun run(instruction: HirInstruction): Result<ThirInstruction, ResolveError>
    {
        engine.prepare(Builtin.GLOBAL_SCOPE).onFailure { return it.toFailure() }
        engine.prepare(scope).onFailure { return it.toFailure() }
        
        return engine.resolve(scope, instruction)
    }
    
    @Nested
    inner class Assign
    {
        @Test
        fun `Given no type, when resolving, then correct outcome`()
        {
            val variable = registerVar("var", type = null)
            
            assertSuccess(variable thirAssign 1, run(variable hirAssign 1))
        }
        
        @Test
        fun `Given valid type, when resolving, then correct outcome`()
        {
            val variable = registerVar("var", type = Builtin.INT32_TYPE)
            
            assertSuccess(variable thirAssign 1, run(variable hirAssign 1))
        }
        
        @Test
        fun `Given unknown variable, when resolving, then correct error`()
        {
            val instance = HirLoad("unknown", emptyList())
            val expected = UnknownVariable("unknown")
            
            assertFailure(expected, run(HirAssign(instance, 0.hir)))
        }
        
        @Test
        fun `Given ambiguous variable, when resolving, then correct error`()
        {
            val instance = HirLoad("var", emptyList())
            val expected = AmbiguousVariable(instance.name, 0.hir)
            
            registerVar(instance.name, type = Builtin.INT32_TYPE)
            registerVar(instance.name, type = Builtin.INT32_TYPE)
            
            assertFailure(expected, run(HirAssign(instance, 0.hir)))
        }
    }
    
    @Nested
    inner class Branch
    {
        @Test
        fun `Given valid predicate, when resolving, then correct outcome`()
        {
            assertSuccess(false.thirBranch(), run(false.hirBranch()))
        }
        
        @Test
        fun `Given success branch, when resolving, then correct outcome`()
        {
            assertSuccess(false.thirBranch(success = listOf(ThirReturn)), run(false.hirBranch(success = listOf(HirReturn))))
        }
        
        @Test
        fun `Given failure branch, when resolving, then correct outcome`()
        {
            assertSuccess(false.thirBranch(failure = listOf(ThirReturn)), run(false.hirBranch(failure = listOf(HirReturn))))
        }
    }
    
    @Nested
    inner class Evaluate
    {
        @Test
        fun `Given valid statement, when resolving, then correct outcome`()
        {
            val function = registerFun("fun")
            
            assertSuccess(function.thirCall().thirEval, run(function.hirCall().hirEval))
        }
    }
    
    @Nested
    inner class Return
    {
        @Test
        fun `Given nothing, When resolving, then correct outcome`()
        {
            assertSuccess(ThirReturn, run(HirReturn))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirReturnValue, run(1.hirReturnValue))
        }
        
        @Test
        fun `Given error, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirReturnError, run(1.hirReturnError))
        }
    }
}
