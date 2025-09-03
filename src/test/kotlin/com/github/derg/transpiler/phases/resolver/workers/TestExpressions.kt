package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.params.*
import org.junit.jupiter.params.provider.*
import java.math.*
import java.util.*

class TestBoolDefiner
{
    private object ValidExpressions : ArgumentProvider(
        false.hir to false.thir,
        true.hir to true.thir,
    )
    
    @ParameterizedTest
    @ArgumentsSource(ValidExpressions::class)
    fun `Given valid expression, when processing, then success`(input: Pair<HirExpression.Bool, ThirExpression>)
    {
        val worker = BoolDefiner(input.first)
        
        assertSuccess(input.second, worker.process())
    }
}

class TestIntegerDefiner
{
    private object ValidExpressions : ArgumentProvider(
        0.hir to 0.thir,
        1.hir to 1.thir,
        Int.MIN_VALUE.hir to Int.MIN_VALUE.thir,
        Int.MAX_VALUE.hir to Int.MAX_VALUE.thir,
        0L.hir to 0L.thir,
        1L.hir to 1L.thir,
        Long.MIN_VALUE.hir to Long.MIN_VALUE.thir,
        Long.MAX_VALUE.hir to Long.MAX_VALUE.thir,
    )
    
    private object InvalidExpressions : ArgumentProvider(
        // hirIntegerOf(literal = "something-invalid"), // TODO: This case is impossible, value cannot be user-provided.
        hirIntegerOf(literal = INT32_LIT_NAME, value = INT32_MIN - 1) to Outcome.InvalidInteger(INT32_MIN - 1),
        hirIntegerOf(literal = INT32_LIT_NAME, value = INT32_MAX + 1) to Outcome.InvalidInteger(INT32_MAX + 1),
        hirIntegerOf(literal = INT64_LIT_NAME, value = INT64_MIN - 1) to Outcome.InvalidInteger(INT64_MIN - 1),
        hirIntegerOf(literal = INT64_LIT_NAME, value = INT64_MAX + 1) to Outcome.InvalidInteger(INT64_MAX + 1),
    )
    
    @ParameterizedTest
    @ArgumentsSource(ValidExpressions::class)
    fun `Given valid expression, when processing, then success`(input: Pair<HirExpression.Integer, ThirExpression>)
    {
        val worker = IntegerDefiner(input.first)
        
        assertSuccess(input.second, worker.process())
    }
    
    @ParameterizedTest
    @ArgumentsSource(InvalidExpressions::class)
    fun `Given invalid expression, when processing, then error`(input: Pair<HirExpression.Integer, Outcome>)
    {
        val worker = IntegerDefiner(input.first)
        
        assertFailure(input.second, worker.process())
    }
}

class TestDecimalDefiner
{
    private object ValidExpressions : ArgumentProvider(
        0.0f.hir to 0.0f.thir,
        3.14f.hir to 3.14f.thir,
        Float.MIN_VALUE.hir to Float.MIN_VALUE.thir,
        Float.MAX_VALUE.hir to Float.MAX_VALUE.thir,
        0.0.hir to 0.0.thir,
        3.14.hir to 3.14.thir,
        Double.MIN_VALUE.hir to Double.MIN_VALUE.thir,
        Double.MAX_VALUE.hir to Double.MAX_VALUE.thir,
    )
    
    private object InvalidExpressions : ArgumentProvider(
        // hirIntegerOf(literal = "something-invalid"), // TODO: This case is impossible, value cannot be user-provided.
        hirDecimalOf(literal = FLOAT32_LIT_NAME, value = FLOAT32_MIN - BigDecimal.ONE) to Outcome.InvalidDecimal(FLOAT32_MIN - BigDecimal.ONE),
        hirDecimalOf(literal = FLOAT32_LIT_NAME, value = FLOAT32_MAX + BigDecimal.ONE) to Outcome.InvalidDecimal(FLOAT32_MAX + BigDecimal.ONE),
        hirDecimalOf(literal = FLOAT64_LIT_NAME, value = FLOAT64_MIN - BigDecimal.ONE) to Outcome.InvalidDecimal(FLOAT64_MIN - BigDecimal.ONE),
        hirDecimalOf(literal = FLOAT64_LIT_NAME, value = FLOAT64_MAX + BigDecimal.ONE) to Outcome.InvalidDecimal(FLOAT64_MAX + BigDecimal.ONE),
    )
    
    @ParameterizedTest
    @ArgumentsSource(ValidExpressions::class)
    fun `Given valid expression, when processing, then success`(input: Pair<HirExpression.Decimal, ThirExpression>)
    {
        val worker = DecimalDefiner(input.first)
        
        assertSuccess(input.second, worker.process())
    }
    
    @Disabled // TODO: This sort of test is quite difficult to write at the moment. We can wait with it until later!
    @ParameterizedTest
    @ArgumentsSource(InvalidExpressions::class)
    fun `Given invalid expression, when processing, then error`(input: Pair<HirExpression.Decimal, Outcome>)
    {
        val worker = DecimalDefiner(input.first)
        
        assertFailure(input.second, worker.process())
    }
}

class TestStringDefiner
{
    private object ValidExpressions : ArgumentProvider(
        "".hir to "".thir,
        "Hello World!".hir to "Hello World!".thir,
    )
    
    @ParameterizedTest
    @ArgumentsSource(ValidExpressions::class)
    fun `Given valid expression, when processing, then success`(input: Pair<HirExpression.Text, ThirExpression>)
    {
        val worker = StringDefiner(input.first)
        
        assertSuccess(input.second, worker.process())
    }
}

class TestIdentifierDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given declared const in scope, when processing, then success`()
    {
        val symbol = thirConstOf().register().declare()
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, symbol.type)
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given declared parameter in scope, when processing, then success`()
    {
        val symbol = thirParamOf().register().declare()
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, symbol.type)
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given declared structure in scope, when processing, then success`()
    {
        val symbol = thirStructOf().register().declare()
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, ThirType.Type)
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given unknown identifier in scope, when processing, then error`()
    {
        val worker = IdentifierDefiner("not-present".hirIdent(), env, scope, null, false)
        val expected = Outcome.UnknownIdentifier("not-present")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given undeclared identifier in scope, when processing, then error`()
    {
        val id = UUID.randomUUID()
        val worker = IdentifierDefiner("not-declared".hirIdent(), env, scope, null, false)
        val expected = Outcome.RequireDeclaration(setOf(id))
        
        scope.register(id, "not-declared")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given overloaded identifier in scope, when processing, then error`()
    {
        val worker = IdentifierDefiner("overloaded".hirIdent(), env, scope, null, false)
        val expected = Outcome.OverloadedIdentifier("overloaded")
        
        scope.register(UUID.randomUUID(), "overloaded")
        scope.register(UUID.randomUUID(), "overloaded")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given type parameters, when processing, then error`()
    {
        val worker = IdentifierDefiner("type-parameters-are".hirIdent("not" to "supported"), env, scope, null, false)
        val expected = Outcome.Unsupported("Type parameters on standalone identifiers are not supported yet")
        
        assertFailure(expected, worker.process())
    }
}

class TestCallDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Generates a simple parameter which may be used for testing purposes. The parameter will not be registered to the
     * scope, nor will it be declared or defined.
     */
    private fun paramOf(
        name: String = UUID.randomUUID().toString(),
        type: ThirType = ThirType.Int32,
    ) = thirParamOf(name = name, type = type).apply { def = null }
    
    private fun ThirDeclaration.Parameter.define(default: ThirExpression? = null): ThirDeclaration.Parameter =
        apply { def = ThirDeclaration.ParameterDef(default = default) }
    
    /**
     * Generates a simple function which may be used for testing purposes. The function will not be registered to the
     * scope, nor will it be declared or defined.
     */
    private fun funOf(
        name: String = UUID.randomUUID().toString(),
        parameters: List<ThirDeclaration.Parameter> = emptyList(),
    ) = thirFunOf(name = name, parameterIds = parameters.map { it.id }).apply { def = null }
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given no inputs or outputs, when processing, then success`()
    {
        val symbol = thirFunOf().register().declare()
        val worker = CallDefiner(symbol.name.hirIdent().hirCall(), env, scope, false)
        val expected = symbol.thirCall()
        
        assertSuccess(expected, worker.process())
    }
    
    @Nested
    inner class `Identifier cases`
    {
        @Test
        fun `Given unknown identifier in scope, when processing, then error`()
        {
            val worker = CallDefiner("not-present".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.UnknownIdentifier("not-present")
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given undeclared identifier in scope, when processing, then error`()
        {
            val id = UUID.randomUUID()
            val worker = CallDefiner("not-declared".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.RequireDeclaration(setOf(id))
            
            scope.register(id, "not-declared")
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given overloaded declared identifier in scope, when processing, then error`()
        {
            funOf(name = "overloaded").register().declare()
            funOf(name = "overloaded").register().declare()
            
            val worker = CallDefiner("overloaded".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.OverloadedIdentifier("overloaded")
            
            assertFailure(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Parameter cases`
    {
        @Test
        fun `Given undeclared required parameter, when processing, then error`()
        {
            val parameter = paramOf().register()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = Outcome.RequireDeclaration(setOf(parameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given undefined required parameter, when processing, then success`()
        {
            val parameter = paramOf().register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given undefined optional but provided parameter, when processing, then success`()
        {
            val parameter = paramOf().register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given undefined optional but omitted parameter, when processing, then failure`()
        {
            val parameter = paramOf().register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.RequireDefinition(setOf(parameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given expected positional parameter, when processing, then success`()
        {
            val parameter = paramOf().register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given expected named parameter, when processing, then success`()
        {
            val parameter = paramOf().register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(parameter.name to 0), env, scope, false)
            val expected = function.thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given missing required parameter, when processing, then error`()
        {
            val parameter = paramOf().register().declare().define(default = null)
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.MissingParameter(parameter.name)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given missing optional parameter, when processing, then success`()
        {
            val parameter = paramOf().register().declare().define(default = 42.thir)
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(), env, scope, false)
            val expected = function.thirCall(42.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected positional parameter, when processing, then error`()
        {
            val function = funOf().register().declare()
            
            val expr = 0.hir
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to expr), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter(null, expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected named parameter, when processing, then error`()
        {
            val function = funOf().register().declare()
            
            val expr = 0.hir
            val worker = CallDefiner(function.name.hirIdent().hirCall("name" to expr), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter("name", expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided positional parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf().register().declare(),
                paramOf().register().declare(),
                paramOf().register().declare(),
            )
            val function = funOf(parameters = parameters).register().declare()
            
            val expr = listOf(null to 1, null to 2, null to 3).toTypedArray()
            val worker = CallDefiner(function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but ordered parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(name = "a").register().declare(),
                paramOf(name = "b").register().declare(),
                paramOf(name = "c").register().declare(),
            )
            val function = funOf(parameters = parameters).register().declare()
            
            val expr = listOf("a" to 1, "b" to 2, "c" to 3).toTypedArray()
            val worker = CallDefiner(function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but scrambled parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(name = "a").register().declare(),
                paramOf(name = "b").register().declare(),
                paramOf(name = "c").register().declare(),
            )
            val function = funOf(parameters = parameters).register().declare()
            
            val expr = listOf("c" to 3, "a" to 1, "b" to 2).toTypedArray()
            val worker = CallDefiner(function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given mismatched type parameter, when processing, then failure`()
        {
            val parameter = paramOf(type = ThirType.Int32).register().declare()
            val function = funOf(parameters = listOf(parameter)).register().declare()
            
            val worker = CallDefiner(function.name.hirIdent().hirCall(null to false), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.MismatchedType(ThirType.Int32, ThirType.Bool)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple overloads but one is valid, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(type = ThirType.Bool).register().declare(),
                paramOf(type = ThirType.Int32).register().declare(),
            )
            val functions = listOf(
                funOf(name = "fun", parameters = listOf(parameters[0])).register().declare(),
                funOf(name = "fun", parameters = listOf(parameters[1])).register().declare(),
            )
            
            val worker = CallDefiner("fun".hirIdent().hirCall(null to 7), env, scope, false)
            val expected = functions[1].thirCall(7.thir)
            
            assertSuccess(expected, worker.process())
        }
    }
}
