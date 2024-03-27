package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Builtin.BOOL
import com.github.derg.transpiler.source.hir.Builtin.BOOL_TYPE
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

/**
 * Helper function for registering [this] symbol into the given [scope]. Returns [this] symbol for convenience.
 */
private fun <Type : HirSymbol> Type.register(scope: Scope): Type = also { scope.register(it) }

class TestResolverType
{
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    private val resolver = ResolverType(scope)
    
    @Nested
    inner class Struct
    {
        @Test
        fun `Given builtin, when resolving, then correct outcome`()
        {
            val expected = ThirTypeData(BOOL.id, Mutability.IMMUTABLE, emptyList())
            
            assertSuccess(expected, resolver.resolve(BOOL_TYPE))
        }
        
        @Test
        fun `Given user-defined, when resolving, then correct outcome`()
        {
            val struct = hirStructOf().register(scope)
            val expected = ThirTypeData(struct.id, Mutability.IMMUTABLE, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeData(struct)))
        }
        
        @Test
        fun `Given unknown struct, when resolving, then correct error`()
        {
            val struct = hirStructOf("struct")
            val expected = UnknownStruct(struct.name)
            
            assertFailure(expected, resolver.resolve(hirTypeData(struct)))
        }
        
        @Test
        fun `Given ambiguous struct, when resolving, then correct error`()
        {
            val structs = listOf(
                hirStructOf("struct").register(scope),
                hirStructOf("struct").register(scope),
            )
            val expected = AmbiguousStruct(structs[0].name)
            
            assertFailure(expected, resolver.resolve(hirTypeData(structs[0])))
        }
    }
    
    @Nested
    inner class Function
    {
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            val value = ThirTypeData(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirTypeCall(value, null, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(value = BOOL_TYPE)))
        }
        
        @Test
        fun `Given error, when resolving, then correct outcome`()
        {
            val error = ThirTypeData(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirTypeCall(null, error, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(error = BOOL_TYPE)))
        }
        
        @Test
        fun `Given parameter, when resolving, then correct outcome`()
        {
            val param = ThirTypeData(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirTypeCall(null, null, listOf("" to param))
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(parameters = listOf(BOOL_TYPE))))
        }
    }
}
