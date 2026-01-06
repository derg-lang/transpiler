package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.interpreter.Stack
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

/**
 * Generates a simple parameter which may be used for testing purposes.
 */
private fun paramOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType = ThirType.Int32,
) = thirParamOf(name = name, kind = ThirKind.Value(type)).apply { def = null }

private fun ThirDeclaration.Parameter.define(default: ThirExpression? = null): ThirDeclaration.Parameter =
    apply { def = ThirDeclaration.ParameterDef(default = default) }

/**
 * Generates a simple field which may be used for testing purposes.
 */
private fun fieldOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType = ThirType.Int32,
) = thirFieldOf(name = name, kind = ThirKind.Value(type)).apply { def = null }

private fun ThirDeclaration.Field.define(default: ThirExpression? = null): ThirDeclaration.Field =
    apply { def = ThirDeclaration.FieldDef(default = default) }

/**
 * Generates a simple function which may be used for testing purposes.
 */
private fun funOf(
    name: String = UUID.randomUUID().toString(),
    typeParameters: List<ThirDeclaration.TypeParameter> = emptyList(),
    parameters: List<ThirDeclaration.Parameter> = emptyList(),
) = thirFunOf(
    name = name,
    typeParameterIds = typeParameters.map { it.id },
    parameterIds = parameters.map { it.id },
).apply { def = null }

/**
 * Generates a simple structure which may be used for testing purposes.
 */
private fun structOf(
    name: String = UUID.randomUUID().toString(),
    generics: List<ThirDeclaration.Parameter> = emptyList(),
    fields: List<ThirDeclaration.Field> = emptyList(),
) = thirStructOf(
    name = name,
    typeParameterIds = generics.map { it.id },
    fieldIds = fields.map { it.id },
).apply { def = null }

/**
 * Generates a simple type parameter which may be used for testing purposes.
 */
private fun typeParamOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType = ThirType.Int32,
) = thirTypeParamOf(name = name, kind = ThirKind.Value(type)).apply { def = null }

private fun ThirDeclaration.TypeParameter.define(default: ThirExpression.Canonical? = null): ThirDeclaration.TypeParameter =
    apply { def = ThirDeclaration.TypeParameterDef(default = default) }

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
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    
    @Test
    fun `Given declared const in scope, when processing, then success`()
    {
        val symbol = thirConstOf().register(scope).declare(env)
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, symbol.kind)
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given declared parameter in scope, when processing, then success`()
    {
        val symbol = thirParamOf().register(scope).declare(env)
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, symbol.kind)
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given declared structure in scope, when processing, then success`()
    {
        val symbol = thirStructOf().register(scope).declare(env)
        val worker = IdentifierDefiner(symbol.name.hirIdent(), env, scope, null, false)
        val expected = ThirExpression.Load(symbol.id, ThirKind.Value(ThirType.Structure(symbol.id, emptyList())))
        
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
    
    // TODO: Type parameters on identifiers without context surrounding it (i.e. function calls or structures) is not
    //       supported yet. This test does as such not offer any value, and is disabled for the time being.
    @Test
    @Disabled
    fun `Given type parameters, when processing, then success`()
    {
        Builtin.BOOL.register(scope).declare(env)
        
        val generic = paramOf().register(scope).declare(env)
        val struct = structOf(generics = listOf(generic)).register(scope).declare(env)
        
        val worker = IdentifierDefiner(struct.name.hirIdent(generic.name to BOOL_TYPE_NAME), env, scope, null, false)
        val expected = ThirExpression.Type(ThirType.Structure(struct.id, listOf(ThirExpression.Type(ThirType.Bool))))
        
        assertSuccess(expected, worker.process())
    }
}

class TestCallDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given no inputs or outputs, when processing, then success`()
    {
        val symbol = thirFunOf().register(scope).declare(env)
        val worker = CallDefiner(evaluator, symbol.name.hirIdent().hirCall(), env, scope, false)
        val expected = symbol.thirLoad().thirCall()
        
        assertSuccess(expected, worker.process())
    }
    
    @Nested
    inner class `Identifier cases`
    {
        @Test
        fun `Given unknown identifier in scope, when processing, then error`()
        {
            val worker = CallDefiner(evaluator, "not-present".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.UnknownIdentifier("not-present")
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given undeclared identifier in scope, when processing, then error`()
        {
            val id = UUID.randomUUID()
            val worker = CallDefiner(evaluator, "not-declared".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.RequireDeclaration(setOf(id))
            
            scope.register(id, "not-declared")
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given overloaded declared identifier in scope, when processing, then error`()
        {
            funOf(name = "overloaded").register(scope).declare(env)
            funOf(name = "overloaded").register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, "overloaded".hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.OverloadedIdentifier("overloaded")
            
            assertFailure(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Function type parameter cases`
    {
        @Test
        fun `Given undeclared required parameter, when processing, then require declaration`()
        {
            val typeParameter = typeParamOf().register(scope)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to 0).hirCall(), env, scope, false)
            val expected = Outcome.RequireDeclaration(setOf(typeParameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given undefined required parameter, when processing, then require definition`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to 0).hirCall(), env, scope, false)
            val expected = Outcome.RequireDefinition(setOf(typeParameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given optional but provided parameter, when processing, then success`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define(default = null)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to 0).hirCall(), env, scope, false)
            val expected = function.thirLoad(0.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given optional but omitted parameter, when processing, then success`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define(default = 0.thir)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = function.thirLoad(0.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given expected positional parameter, when processing, then success`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define()
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to 0).hirCall(), env, scope, false)
            val expected = function.thirLoad(0.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given expected named parameter, when processing, then success`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define()
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(typeParameter.name to 0).hirCall(), env, scope, false)
            val expected = function.thirLoad(0.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given missing required parameter, when processing, then error`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define(default = null)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.MissingParameter(typeParameter.name)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given missing optional parameter, when processing, then success`()
        {
            val typeParameter = typeParamOf().register(scope).declare(env).define(default = 42.thir)
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = function.thirLoad(42.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected positional parameter, when processing, then error`()
        {
            val function = funOf().register(scope).declare(env)
            
            val expr = 0.hir
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to expr).hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter(null, expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected named parameter, when processing, then error`()
        {
            val function = funOf().register(scope).declare(env)
            
            val expr = 0.hir
            val worker = CallDefiner(evaluator, function.name.hirIdent("name" to expr).hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter("name", expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided positional parameters, when processing, then success`()
        {
            val typeParameters = listOf(
                typeParamOf().register(scope).declare(env).define(),
                typeParamOf().register(scope).declare(env).define(),
                typeParamOf().register(scope).declare(env).define(),
            )
            val function = funOf(typeParameters = typeParameters).register(scope).declare(env)
            
            val expr = listOf(null to 1, null to 2, null to 3).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent(*expr).hirCall(), env, scope, false)
            val expected = function.thirLoad(1.thir, 2.thir, 3.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but ordered parameters, when processing, then success`()
        {
            val typeParameters = listOf(
                typeParamOf(name = "a").register(scope).declare(env).define(),
                typeParamOf(name = "b").register(scope).declare(env).define(),
                typeParamOf(name = "c").register(scope).declare(env).define(),
            )
            val function = funOf(typeParameters = typeParameters).register(scope).declare(env)
            
            val expr = listOf("a" to 1, "b" to 2, "c" to 3).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent(*expr).hirCall(), env, scope, false)
            val expected = function.thirLoad(1.thir, 2.thir, 3.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but scrambled parameters, when processing, then success`()
        {
            val typeParameters = listOf(
                typeParamOf(name = "a").register(scope).declare(env).define(),
                typeParamOf(name = "b").register(scope).declare(env).define(),
                typeParamOf(name = "c").register(scope).declare(env).define(),
            )
            val function = funOf(typeParameters = typeParameters).register(scope).declare(env)
            
            val expr = listOf("c" to 3, "a" to 1, "b" to 2).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent(*expr).hirCall(), env, scope, false)
            val expected = function.thirLoad(1.thir, 2.thir, 3.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given mismatched type parameter, when processing, then failure`()
        {
            val typeParameter = typeParamOf(type = ThirType.Int32).register(scope).declare(env).define()
            val function = funOf(typeParameters = listOf(typeParameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent(null to false).hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(
                function.name,
                listOf(Outcome.MismatchedKind(ThirKind.Value(ThirType.Int32), ThirKind.Value(ThirType.Bool))),
            )
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple overloads but one is valid, when processing, then success`()
        {
            val typeParameters = listOf(
                typeParamOf(type = ThirType.Bool).register(scope).declare(env).define(),
                typeParamOf(type = ThirType.Int32).register(scope).declare(env).define(),
            )
            val functions = listOf(
                funOf(name = "fun", typeParameters = listOf(typeParameters[0])).register(scope).declare(env),
                funOf(name = "fun", typeParameters = listOf(typeParameters[1])).register(scope).declare(env),
            )
            
            val worker = CallDefiner(evaluator, "fun".hirIdent(null to 7).hirCall(), env, scope, false)
            val expected = functions[1].thirLoad(7.thir).thirCall()
            
            assertSuccess(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Function parameter cases`
    {
        @Test
        fun `Given undeclared required parameter, when processing, then error`()
        {
            val parameter = paramOf().register(scope)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = Outcome.RequireDeclaration(setOf(parameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given undefined required parameter, when processing, then success`()
        {
            val parameter = paramOf().register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirLoad().thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given undefined optional but provided parameter, when processing, then success`()
        {
            val parameter = paramOf().register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirLoad().thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given undefined optional but omitted parameter, when processing, then failure`()
        {
            val parameter = paramOf().register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.RequireDefinition(setOf(parameter.id))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given expected positional parameter, when processing, then success`()
        {
            val parameter = paramOf().register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to 0), env, scope, false)
            val expected = function.thirLoad().thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given expected named parameter, when processing, then success`()
        {
            val parameter = paramOf().register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(parameter.name to 0), env, scope, false)
            val expected = function.thirLoad().thirCall(0.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given missing required parameter, when processing, then error`()
        {
            val parameter = paramOf().register(scope).declare(env).define(default = null)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.MissingParameter(parameter.name)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given missing optional parameter, when processing, then success`()
        {
            val parameter = paramOf().register(scope).declare(env).define(default = 42.thir)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(), env, scope, false)
            val expected = function.thirLoad().thirCall(42.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected positional parameter, when processing, then error`()
        {
            val function = funOf().register(scope).declare(env)
            
            val expr = 0.hir
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to expr), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter(null, expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given unexpected named parameter, when processing, then error`()
        {
            val function = funOf().register(scope).declare(env)
            
            val expr = 0.hir
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall("name" to expr), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(function.name, listOf(Outcome.UnexpectedParameter("name", expr)))
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided positional parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf().register(scope).declare(env),
                paramOf().register(scope).declare(env),
                paramOf().register(scope).declare(env),
            )
            val function = funOf(parameters = parameters).register(scope).declare(env)
            
            val expr = listOf(null to 1, null to 2, null to 3).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirLoad().thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but ordered parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(name = "a").register(scope).declare(env),
                paramOf(name = "b").register(scope).declare(env),
                paramOf(name = "c").register(scope).declare(env),
            )
            val function = funOf(parameters = parameters).register(scope).declare(env)
            
            val expr = listOf("a" to 1, "b" to 2, "c" to 3).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirLoad().thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given multiple provided named but scrambled parameters, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(name = "a").register(scope).declare(env),
                paramOf(name = "b").register(scope).declare(env),
                paramOf(name = "c").register(scope).declare(env),
            )
            val function = funOf(parameters = parameters).register(scope).declare(env)
            
            val expr = listOf("c" to 3, "a" to 1, "b" to 2).toTypedArray()
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(*expr), env, scope, false)
            val expected = function.thirLoad().thirCall(1.thir, 2.thir, 3.thir)
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given mismatched type parameter, when processing, then failure`()
        {
            val parameter = paramOf(type = ThirType.Int32).register(scope).declare(env)
            val function = funOf(parameters = listOf(parameter)).register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, function.name.hirIdent().hirCall(null to false), env, scope, false)
            val expected = Outcome.NoOverloadAvailable(
                function.name,
                listOf(Outcome.MismatchedKind(ThirKind.Value(ThirType.Int32), ThirKind.Value(ThirType.Bool))),
            )
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given multiple overloads but one is valid, when processing, then success`()
        {
            val parameters = listOf(
                paramOf(type = ThirType.Bool).register(scope).declare(env),
                paramOf(type = ThirType.Int32).register(scope).declare(env),
            )
            val functions = listOf(
                funOf(name = "fun", parameters = listOf(parameters[0])).register(scope).declare(env),
                funOf(name = "fun", parameters = listOf(parameters[1])).register(scope).declare(env),
            )
            
            val worker = CallDefiner(evaluator, "fun".hirIdent().hirCall(null to 7), env, scope, false)
            val expected = functions[1].thirLoad().thirCall(7.thir)
            
            assertSuccess(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Constructor cases`
    {
        @Test
        fun `Given structure without fields, when processing, then success`()
        {
            val structure = structOf().register(scope).declare(env)
            
            val worker = CallDefiner(evaluator, structure.name.hirIdent().hirCall(), env, scope, false)
            val kind = ThirKind.Value(ThirType.Structure(structure.id, emptyList()))
            val expected = ThirExpression.Call(
                instance = ThirExpression.Type(ThirType.Function(structure.id, emptyList(), kind, ThirKind.Nothing)),
                parameters = emptyList(),
                valueKind = kind,
                errorKind = ThirKind.Nothing,
            )
            
            assertSuccess(expected, worker.process())
        }
    }
}

class TestCatchDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Nested
    inner class Handle
    {
        @Test
        fun `Given no lhs and no rhs value type, when processing, then success`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = lhs.thirLoad().thirCall() thirCatch rhs.thirLoad().thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given any lhs but no rhs value type, when processing, then failure`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.CatchHandleKindMismatch(ThirKind.Value(ThirType.Int32), ThirKind.Nothing)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given different lhs and rhs value type, when processing, then failure`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Str), errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.Unsupported("Catch unions are not yet supported")
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given matching lhs and rhs type and no rhs error type, when processing, then success`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = lhs.thirLoad().thirCall() thirCatch rhs.thirLoad().thirCall()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given no lhs error type, when processing, then failure`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Nothing).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.CatchLeftHasNoError
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any rhs error type, when processing, then failure`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            val rhs = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Str)).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatch rhs.name.hirIdent().hirCall(), env, scope, false)
            val expected = Outcome.CatchRightHasError(ThirKind.Value(ThirType.Str))
            
            assertFailure(expected, worker.process())
        }
    }
    
    @Nested
    inner class CatchValue
    {
        @Test
        fun `Given no lhs value type but valid rhs, when processing, then success`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatchValue 0, env, scope, false)
            val expected = lhs.thirLoad().thirCall() thirCatchValue 0.thir
            
            assertSuccess(expected, worker.process())
        }
    }
    
    @Nested
    inner class CatchError
    {
        @Test
        fun `Given no lhs value type but valid rhs, when processing, then success`()
        {
            val lhs = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Value(ThirType.Bool)).register(scope).declare(env)
            
            val worker = CatchDefiner(evaluator, lhs.name.hirIdent().hirCall() hirCatchError 0, env, scope, false)
            val expected = lhs.thirLoad().thirCall() thirCatchError 0.thir
            
            assertSuccess(expected, worker.process())
        }
    }
}
