package com.github.derg.transpiler.experimental.toy_solver

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import java.util.*

private val Boolean.node get() = Hir.BoolLiteral(UUID.randomUUID(), this)
private val Float.node get() = Hir.FloatLiteral(UUID.randomUUID(), this)
private val Int.node get() = Hir.IntLiteral(UUID.randomUUID(), this)
private val String.node get() = Hir.StringLiteral(UUID.randomUUID(), this)

private fun String.load(vararg arguments: Pair<String?, Hir.Expr>) =
    Hir.NameReference(UUID.randomUUID(), this, arguments.map { Hir.Argument(it.first, it.second) })

private fun Hir.Expr.call(vararg arguments: Pair<String?, Hir.Expr>) =
    Hir.Call(UUID.randomUUID(), this, arguments.map { Hir.Argument(it.first, it.second) })

private fun functionOf(
    id: UUID = UUID.randomUUID(),
    name: String = "function-${UUID.randomUUID()}",
    typeParameters: List<Hir.TypeParameterDecl> = emptyList(),
    parameters: List<Hir.ParameterDecl> = emptyList(),
    returnKind: Hir.Kind = Hir.KNothing,
) = Hir.FunctionDecl(
    id = id,
    name = name,
    typeParameters = typeParameters,
    parameters = parameters,
    returnKind = returnKind,
    statements = emptyList(),
)

private fun parameterOf(
    id: UUID = UUID.randomUUID(),
    name: String = "parameter-${UUID.randomUUID()}",
    kind: Hir.Kind? = null,
    default: Hir.Expr? = null,
) = Hir.ParameterDecl(
    id = id,
    name = name,
    kind = kind,
    default = default,
)

private fun structureOf(
    id: UUID = UUID.randomUUID(),
    name: String = "structure-${UUID.randomUUID()}",
    typeParameters: List<Hir.TypeParameterDecl> = emptyList(),
) = Hir.StructureDecl(
    id = id,
    name = name,
    typeParameters = typeParameters,
    fields = emptyList(),
    methods = emptyList(),
    constructor = functionOf(),
    implements = emptyList(),
)

private fun typeParameterOf(
    id: UUID = UUID.randomUUID(),
    name: String = "type-parameter-${UUID.randomUUID()}",
    kind: Hir.Kind? = null,
    default: Hir.Expr? = null,
) = Hir.TypeParameterDecl(
    id = id,
    name = name,
    kind = kind,
    default = default,
)

private fun Hir.FunctionDecl.register(environment: Environment): Hir.FunctionDecl =
    also { environment.functions.getOrPut(name) { mutableListOf() }.add(this) }

private fun Hir.StructureDecl.register(environment: Environment): Hir.StructureDecl =
    also { environment.structures[name] = this }

private fun Hir.TypeParameterDecl.register(environment: Environment): Hir.TypeParameterDecl =
    also { environment.typeParameters[name] = this }

class TestSolver
{
    private val env = Environment()
    private val solver = Solver(env)
    
    private val bool = structureOf(id = Builtin.BOOL.id, name = Builtin.BOOL.name).register(env)
    private val float32 = structureOf(id = Builtin.FLOAT32.id, name = Builtin.FLOAT32.name).register(env)
    private val int32 = structureOf(id = Builtin.INT32.id, name = Builtin.INT32.name).register(env)
    
    @Test
    fun `Given literals, then types are trivial`()
    {
        assertSuccess(TBool, solver.infer(false.node))
        assertSuccess(TFloat, solver.infer(0.0f.node))
        assertSuccess(TInt, solver.infer(0.node))
        assertSuccess(TString, solver.infer("".node))
    }
    
    @Nested
    inner class Functions
    {
        @Test
        fun `Given basic function`()
        {
            val function = functionOf(returnKind = Hir.KNothing).register(env)
            
            assertSuccess(TNothing, solver.infer(function.name.load().call()))
        }
        
        @Test
        fun `Given builtin return type`()
        {
            val function = functionOf(returnKind = Hir.KValue(int32.name.load())).register(env)
            
            assertSuccess(TInt, solver.infer(function.name.load().call()))
        }
        
        @Test
        fun `Given custom return type`()
        {
            val structure = structureOf().register(env)
            val function = functionOf(returnKind = Hir.KValue(structure.name.load())).register(env)
            
            assertSuccess(TConstructor(structure.id, emptyList()), solver.infer(function.name.load().call()))
        }
        
        @Test
        fun `Given missing parameter`()
        {
            val parameter = parameterOf(kind = Hir.KValue(int32.name.load()))
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertFailure(solver.infer(function.name.load().call()))
        }
        
        @Test
        fun `Given excess parameters`()
        {
            val parameter = parameterOf(kind = Hir.KValue(int32.name.load()))
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertFailure(solver.infer(function.name.load().call(null to 1.node, null to 2.node)))
        }
        
        @Test
        fun `Given mismatching parameter type`()
        {
            val parameter = parameterOf(kind = Hir.KValue(int32.name.load()))
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertFailure(solver.infer(function.name.load().call(null to true.node)))
        }
        
        @Test
        fun `Given matching parameter type`()
        {
            val parameter = parameterOf(kind = Hir.KValue(int32.name.load()))
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertSuccess(TNothing, solver.infer(function.name.load().call(null to 1.node)))
        }
        
        @Test
        fun `Given mismatching parameter value`()
        {
            val parameter = parameterOf(default = 1.node)
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertFailure(solver.infer(function.name.load().call(null to true.node)))
        }
        
        @Test
        fun `Given matching parameter value`()
        {
            val parameter = parameterOf(default = 1.node)
            val function = functionOf(parameters = listOf(parameter)).register(env)
            
            assertSuccess(TNothing, solver.infer(function.name.load().call(null to 1.node)))
        }
        
        @Test
        fun `Given no matching overload`()
        {
            functionOf(name = "overload", parameters = listOf(parameterOf(default = 0.node))).register(env)
            functionOf(name = "overload", parameters = listOf(parameterOf(default = 0.0f.node))).register(env)
            
            assertFailure(solver.infer("overload".load().call(null to false.node)))
        }
        
        @Test
        fun `Given one matching overload`()
        {
            functionOf(name = "overload", parameters = listOf(parameterOf(default = false.node))).register(env)
            functionOf(name = "overload", parameters = listOf(parameterOf(default = 0.0f.node))).register(env)
            
            assertSuccess(TNothing, solver.infer("overload".load().call(null to false.node)))
        }
        
        @Test
        fun `Given multiple matching overload`()
        {
            functionOf(name = "overload", parameters = listOf(parameterOf(default = false.node))).register(env)
            functionOf(name = "overload", parameters = listOf(parameterOf(default = false.node))).register(env)
            
            assertFailure(solver.infer("overload".load().call(null to false.node)))
        }
        
        @Test
        fun `Given absent type parameter`()
        {
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val function = functionOf(typeParameters = listOf(typeParameter)).register(env)
            
            assertFailure(solver.infer(function.name.load().call()))
        }
        
        @Test
        fun `Given excess type parameter`()
        {
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val function = functionOf(typeParameters = listOf(typeParameter)).register(env)
            
            assertFailure(solver.infer(function.name.load(null to int32.name.load(), null to bool.name.load()).call()))
        }
        
        @Test
        @Disabled
        fun `Given wrong type parameter`()
        {
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val parameter = parameterOf(kind = Hir.KValue(typeParameter.name.load()))
            val function = functionOf(typeParameters = listOf(typeParameter), parameters = listOf(parameter)).register(env)
            
            assertFailure(solver.infer(function.name.load(null to bool.name.load()).call(null to 1.node)))
        }
        
        @Test
        @Disabled
        fun `Given explicit type parameter`()
        {
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val function = functionOf(
                typeParameters = listOf(typeParameter),
                returnKind = Hir.KValue(typeParameter.name.load()),
            ).register(env)
            
            assertSuccess(TInt, solver.infer(function.name.load(null to int32.name.load()).call()))
            assertSuccess(TFloat, solver.infer(function.name.load(null to float32.name.load()).call()))
            assertSuccess(TBool, solver.infer(function.name.load(null to bool.name.load()).call()))
        }
        
        @Test
        fun `Given inferred type parameter`()
        {
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val parameter = parameterOf(kind = Hir.KValue(typeParameter.name.load()))
            val function = functionOf(
                typeParameters = listOf(typeParameter),
                parameters = listOf(parameter),
                returnKind = Hir.KValue(typeParameter.name.load()),
            ).register(env)
            
            assertSuccess(TInt, solver.infer(function.name.load().call(null to 1.node)))
            assertSuccess(TFloat, solver.infer(function.name.load().call(null to 1.0f.node)))
            assertSuccess(TBool, solver.infer(function.name.load().call(null to true.node)))
        }
        
        @Test
        @Disabled
        fun `Given wrapped type parameter`()
        {
            val wrapperParam = typeParameterOf(kind = Hir.KType).register(env)
            val wrapper = structureOf(typeParameters = listOf(wrapperParam)).register(env)
            
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val function = functionOf(
                typeParameters = listOf(typeParameter),
                returnKind = Hir.KValue(typeParameter.name.load()),
            ).register(env)
            
            val wrapped = wrapper.name.load(null to int32.name.load())
            val expected = TConstructor(wrapper.id, listOf(TInt))
            
            assertSuccess(expected, solver.infer(function.name.load(null to wrapped).call()))
        }
        
        @Test
        @Disabled
        fun `Given deeply wrapped type parameter`()
        {
            val wrapperParam = typeParameterOf(kind = Hir.KType).register(env)
            val wrapper = structureOf(typeParameters = listOf(wrapperParam)).register(env)
            
            val typeParameter = typeParameterOf(kind = Hir.KType).register(env)
            val function = functionOf(
                typeParameters = listOf(typeParameter),
                returnKind = Hir.KValue(typeParameter.name.load()),
            ).register(env)
            
            val wrapped = wrapper.name.load(null to wrapper.name.load(null to wrapper.name.load(null to int32.name.load())))
            val expected = TConstructor(wrapper.id, listOf(TConstructor(wrapper.id, listOf(TConstructor(wrapper.id, listOf(TInt))))))
            
            assertSuccess(expected, solver.infer(function.name.load(null to wrapped).call()))
        }
    }
}
