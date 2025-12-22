package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * The collection of all builtin types, functions, literals, and everything else. The builtin scope defines everything
 * which is needed to compile any source code into a valid program.
 */
object Builtin
{
    private val functions = mutableListOf<ThirDeclaration.Function>()
    private val parameters = mutableListOf<ThirDeclaration.Parameter>()
    private val structures = mutableListOf<ThirDeclaration.Structure>()
    
    val BOOL = registerPrimitive(BOOL_TYPE_NAME)
    val BOOL_AND = registerBinary(BinaryOperator.AND, ThirType.Bool, ThirType.Bool)
    val BOOL_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Bool, ThirType.Bool)
    val BOOL_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Bool, ThirType.Bool)
    val BOOL_NOT = registerUnary(UnaryOperator.NOT, ThirType.Bool, ThirType.Bool)
    val BOOL_OR = registerBinary(BinaryOperator.OR, ThirType.Bool, ThirType.Bool)
    val BOOL_XOR = registerBinary(BinaryOperator.XOR, ThirType.Bool, ThirType.Bool)
    
    val INT32 = registerPrimitive(INT32_TYPE_NAME)
    val INT32_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Int32, ThirType.Bool)
    val INT32_GE = registerBinary(BinaryOperator.GREATER_EQUAL, ThirType.Int32, ThirType.Bool)
    val INT32_GT = registerBinary(BinaryOperator.GREATER, ThirType.Int32, ThirType.Bool)
    val INT32_LE = registerBinary(BinaryOperator.LESS_EQUAL, ThirType.Int32, ThirType.Bool)
    val INT32_LT = registerBinary(BinaryOperator.LESS, ThirType.Int32, ThirType.Bool)
    val INT32_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Int32, ThirType.Bool)
    val INT32_ADD = registerBinary(BinaryOperator.ADD, ThirType.Int32, ThirType.Int32)
    val INT32_DIV = registerBinary(BinaryOperator.DIVIDE, ThirType.Int32, ThirType.Int32) // TODO: divide-by-zero
    val INT32_MOD = registerBinary(BinaryOperator.MODULO, ThirType.Int32, ThirType.Int32) // TODO: divide-by-zero
    val INT32_MUL = registerBinary(BinaryOperator.MULTIPLY, ThirType.Int32, ThirType.Int32)
    val INT32_NEG = registerUnary(UnaryOperator.MINUS, ThirType.Int32, ThirType.Int32)
    val INT32_POS = registerUnary(UnaryOperator.PLUS, ThirType.Int32, ThirType.Int32)
    val INT32_SUB = registerBinary(BinaryOperator.SUBTRACT, ThirType.Int32, ThirType.Int32)
    
    val INT64 = registerPrimitive(INT64_TYPE_NAME)
    val INT64_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Int64, ThirType.Bool)
    val INT64_GE = registerBinary(BinaryOperator.GREATER_EQUAL, ThirType.Int64, ThirType.Bool)
    val INT64_GT = registerBinary(BinaryOperator.GREATER, ThirType.Int64, ThirType.Bool)
    val INT64_LE = registerBinary(BinaryOperator.LESS_EQUAL, ThirType.Int64, ThirType.Bool)
    val INT64_LT = registerBinary(BinaryOperator.LESS, ThirType.Int64, ThirType.Bool)
    val INT64_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Int64, ThirType.Bool)
    val INT64_ADD = registerBinary(BinaryOperator.ADD, ThirType.Int64, ThirType.Int64)
    val INT64_DIV = registerBinary(BinaryOperator.DIVIDE, ThirType.Int64, ThirType.Int64) // TODO: divide-by-zero
    val INT64_MOD = registerBinary(BinaryOperator.MODULO, ThirType.Int64, ThirType.Int64) // TODO: divide-by-zero
    val INT64_MUL = registerBinary(BinaryOperator.MULTIPLY, ThirType.Int64, ThirType.Int64)
    val INT64_NEG = registerUnary(UnaryOperator.MINUS, ThirType.Int64, ThirType.Int64)
    val INT64_POS = registerUnary(UnaryOperator.PLUS, ThirType.Int64, ThirType.Int64)
    val INT64_SUB = registerBinary(BinaryOperator.SUBTRACT, ThirType.Int64, ThirType.Int64)
    
    val FLOAT32 = registerPrimitive(FLOAT32_TYPE_NAME)
    val FLOAT32_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Float32, ThirType.Bool)
    val FLOAT32_GE = registerBinary(BinaryOperator.GREATER_EQUAL, ThirType.Float32, ThirType.Bool)
    val FLOAT32_GT = registerBinary(BinaryOperator.GREATER, ThirType.Float32, ThirType.Bool)
    val FLOAT32_LE = registerBinary(BinaryOperator.LESS_EQUAL, ThirType.Float32, ThirType.Bool)
    val FLOAT32_LT = registerBinary(BinaryOperator.LESS, ThirType.Float32, ThirType.Bool)
    val FLOAT32_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Float32, ThirType.Bool)
    val FLOAT32_ADD = registerBinary(BinaryOperator.ADD, ThirType.Float32, ThirType.Float32)
    val FLOAT32_DIV = registerBinary(BinaryOperator.DIVIDE, ThirType.Float32, ThirType.Float32) // TODO: divide-by-zero
    val FLOAT32_MOD = registerBinary(BinaryOperator.MODULO, ThirType.Float32, ThirType.Float32) // TODO: divide-by-zero
    val FLOAT32_MUL = registerBinary(BinaryOperator.MULTIPLY, ThirType.Float32, ThirType.Float32)
    val FLOAT32_NEG = registerUnary(UnaryOperator.MINUS, ThirType.Float32, ThirType.Float32)
    val FLOAT32_POS = registerUnary(UnaryOperator.PLUS, ThirType.Float32, ThirType.Float32)
    val FLOAT32_SUB = registerBinary(BinaryOperator.SUBTRACT, ThirType.Float32, ThirType.Float32)
    
    val FLOAT64 = registerPrimitive(FLOAT64_TYPE_NAME)
    val FLOAT64_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Float64, ThirType.Bool)
    val FLOAT64_GE = registerBinary(BinaryOperator.GREATER_EQUAL, ThirType.Float64, ThirType.Bool)
    val FLOAT64_GT = registerBinary(BinaryOperator.GREATER, ThirType.Float64, ThirType.Bool)
    val FLOAT64_LE = registerBinary(BinaryOperator.LESS_EQUAL, ThirType.Float64, ThirType.Bool)
    val FLOAT64_LT = registerBinary(BinaryOperator.LESS, ThirType.Float64, ThirType.Bool)
    val FLOAT64_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Float64, ThirType.Bool)
    val FLOAT64_ADD = registerBinary(BinaryOperator.ADD, ThirType.Float64, ThirType.Float64)
    val FLOAT64_DIV = registerBinary(BinaryOperator.DIVIDE, ThirType.Float64, ThirType.Float64) // TODO: divide-by-zero
    val FLOAT64_MOD = registerBinary(BinaryOperator.MODULO, ThirType.Float64, ThirType.Float64) // TODO: divide-by-zero
    val FLOAT64_MUL = registerBinary(BinaryOperator.MULTIPLY, ThirType.Float64, ThirType.Float64)
    val FLOAT64_NEG = registerUnary(UnaryOperator.MINUS, ThirType.Float64, ThirType.Float64)
    val FLOAT64_POS = registerUnary(UnaryOperator.PLUS, ThirType.Float64, ThirType.Float64)
    val FLOAT64_SUB = registerBinary(BinaryOperator.SUBTRACT, ThirType.Float64, ThirType.Float64)
    
    val STR = registerPrimitive(STR_TYPE_NAME)
    val STR_EQ = registerBinary(BinaryOperator.EQUAL, ThirType.Str, ThirType.Bool)
    val STR_NE = registerBinary(BinaryOperator.NOT_EQUAL, ThirType.Str, ThirType.Bool)
    val STR_ADD = registerBinary(BinaryOperator.ADD, ThirType.Str, ThirType.Str)
    val STR_PRINTLN = registerConsumer(FUN_PRINTLN_NAME, ThirType.Str)
    
    /**
     * Generates a new environment in which all global symbols reside. The environment should be modified during
     * compilation to include all new symbols discovered while compiling.
     */
    fun generateEnvironment(): Environment
    {
        val environment = Environment()
        functions.forEach { environment.declarations[it.id] = it }
        parameters.forEach { environment.declarations[it.id] = it }
        structures.forEach { environment.declarations[it.id] = it }
        return environment
    }
    
    /**
     * Generates a stack frame which contains the value of all builtin globals. This frame should be used during
     * compilation to ensure all globals are given proper values initially.
     */
    fun generateGlobals(): StackFrame
    {
        val globals = StackFrame()
        globals[BOOL.id] = ThirExpression.Type(ThirType.Bool)
        globals[INT32.id] = ThirExpression.Type(ThirType.Int32)
        globals[INT64.id] = ThirExpression.Type(ThirType.Int64)
        globals[FLOAT32.id] = ThirExpression.Type(ThirType.Float32)
        globals[FLOAT64.id] = ThirExpression.Type(ThirType.Float64)
        globals[STR.id] = ThirExpression.Type(ThirType.Str)
        return globals
    }
    
    /**
     * Generates a new scope in which all global symbols are visible. This scope should be used as the base of all
     * scopes when compiling any program.
     */
    fun generateScope(): Scope
    {
        val scope = Scope(null)
        functions.forEach { scope.register(it.id, it.name) }
        structures.forEach { scope.register(it.id, it.name) }
        return scope
    }
    
    /**
     * Registers a new builtin primitive with the given [name] to the global system. Primitives are the most fundamental
     * building blocks in any program, representing the simplest bits of data possible.
     */
    private fun registerPrimitive(name: String): ThirDeclaration.Structure
    {
        val primitive = ThirDeclaration.Structure(
            id = UUID.randomUUID(),
            name = name,
            typeParameterIds = emptyList(),
            ctorEntryIds = emptyList(),
            fieldIds = emptyList(),
            def = ThirDeclaration.StructureDef(null),
        )
        
        structures += primitive
        return primitive
    }
    
    /**
     * Registers a new builtin binary [operator] between two different types. The operator takes two [inputType]s and
     * emits a single [outputType].
     */
    private fun registerBinary(operator: BinaryOperator, inputType: ThirType, outputType: ThirType): ThirDeclaration.Function
    {
        val lhs = ThirDeclaration.Parameter(
            id = UUID.randomUUID(),
            name = "lhs",
            passability = Passability.IN,
            kind = ThirKind.Value(inputType),
            def = ThirDeclaration.ParameterDef(default = null)
        )
        val rhs = ThirDeclaration.Parameter(
            id = UUID.randomUUID(),
            name = "rhs",
            passability = Passability.IN,
            kind = ThirKind.Value(inputType),
            def = ThirDeclaration.ParameterDef(default = null)
        )
        val function = ThirDeclaration.Function(
            id = UUID.randomUUID(),
            name = operator.symbol,
            valueKind = ThirKind.Value(outputType),
            errorKind = ThirKind.Nothing,
            typeParameterIds = emptyList(),
            parameterIds = listOf(lhs.id, rhs.id),
            def = ThirDeclaration.FunctionDef(statements = emptyList())
        )
        
        parameters += lhs
        parameters += rhs
        functions += function
        return function
    }
    
    /**
     * Registers a new builtin unary [operator] between two different types. The operator takes one [inputType] and
     * emits a single [outputType].
     */
    private fun registerUnary(operator: UnaryOperator, inputType: ThirType, outputType: ThirType): ThirDeclaration.Function
    {
        val rhs = ThirDeclaration.Parameter(
            id = UUID.randomUUID(),
            name = "rhs",
            passability = Passability.IN,
            kind = ThirKind.Value(inputType),
            def = ThirDeclaration.ParameterDef(default = null)
        )
        val function = ThirDeclaration.Function(
            id = UUID.randomUUID(),
            name = operator.symbol,
            valueKind = ThirKind.Value(outputType),
            errorKind = ThirKind.Nothing,
            typeParameterIds = emptyList(),
            parameterIds = listOf(rhs.id),
            def = ThirDeclaration.FunctionDef(statements = emptyList())
        )
        
        parameters += rhs
        functions += function
        return function
    }
    
    /**
     * Registers a new builtin function with the given [name], which consumes a single [inputType].
     */
    private fun registerConsumer(name: String, inputType: ThirType): ThirDeclaration.Function
    {
        val input = ThirDeclaration.Parameter(
            id = UUID.randomUUID(),
            name = "input",
            passability = Passability.IN,
            kind = ThirKind.Value(inputType),
            def = ThirDeclaration.ParameterDef(default = null)
        )
        val function = ThirDeclaration.Function(
            id = UUID.randomUUID(),
            name = name,
            valueKind = ThirKind.Nothing,
            errorKind = ThirKind.Nothing,
            typeParameterIds = emptyList(),
            parameterIds = listOf(input.id),
            def = ThirDeclaration.FunctionDef(statements = emptyList())
        )
        
        parameters += input
        functions += function
        return function
    }
}
