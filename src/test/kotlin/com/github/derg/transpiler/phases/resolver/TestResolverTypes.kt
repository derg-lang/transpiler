package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Builtin.BOOL
import com.github.derg.transpiler.source.hir.Builtin.BOOL_TYPE
import com.github.derg.transpiler.source.hir.Builtin.INT32
import com.github.derg.transpiler.source.hir.Builtin.INT32_TYPE
import com.github.derg.transpiler.source.hir.Builtin.INT64
import com.github.derg.transpiler.source.hir.Builtin.INT64_TYPE
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
            val expected = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            
            assertSuccess(expected, resolver.resolve(BOOL_TYPE))
        }
        
        @Test
        fun `Given user-defined, when resolving, then correct outcome`()
        {
            val struct = hirStructOf().register(scope)
            val expected = ThirType.Structure(struct.id, Mutability.IMMUTABLE, emptyList())
            
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
            val value = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirType.Function(value, null, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(value = BOOL_TYPE)))
        }
        
        @Test
        fun `Given error, when resolving, then correct outcome`()
        {
            val error = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirType.Function(null, error, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(error = BOOL_TYPE)))
        }
        
        @Test
        fun `Given parameter, when resolving, then correct outcome`()
        {
            val param = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            val expected = ThirType.Function(null, null, listOf(ThirParameterDynamic("", param, Passability.IN)))
            
            assertSuccess(expected, resolver.resolve(hirTypeCall(parameters = listOf(BOOL_TYPE))))
        }
    }
    
    @Nested
    inner class Union
    {
        @Test
        fun `Given empty, when resolving, then correct outcome`()
        {
            val expected = ThirType.Union(emptySet())
            
            assertSuccess(expected, resolver.resolve(hirTypeUnion()))
        }
        
        @Test
        fun `Given single, when resolving, then correct outcome`()
        {
            val expected = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeUnion(BOOL_TYPE)))
        }
        
        @Test
        fun `Given multiple, when resolving, then correct outcome`()
        {
            val inner = setOf(
                ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList()),
                ThirType.Structure(INT32.id, Mutability.IMMUTABLE, emptyList()),
            )
            val expected = ThirType.Union(inner)
            
            assertSuccess(expected, resolver.resolve(hirTypeUnion(BOOL_TYPE, INT32_TYPE)))
        }
        
        @Test
        fun `Given nested, when resolving, then correct outcome`()
        {
            val expected = ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList())
            
            assertSuccess(expected, resolver.resolve(hirTypeUnion(hirTypeUnion(BOOL_TYPE))))
        }
        
        @Test
        fun `Given union, when resolving, then correct outcome`()
        {
            val inner = setOf(
                ThirType.Structure(BOOL.id, Mutability.IMMUTABLE, emptyList()),
                ThirType.Structure(INT32.id, Mutability.IMMUTABLE, emptyList()),
                ThirType.Structure(INT64.id, Mutability.IMMUTABLE, emptyList()),
            )
            val expected = ThirType.Union(inner)
            
            assertSuccess(expected, resolver.resolve(hirTypeUnion(hirTypeUnion(BOOL_TYPE, INT32_TYPE), hirTypeUnion(BOOL_TYPE, INT64_TYPE))))
        }
    }
}
