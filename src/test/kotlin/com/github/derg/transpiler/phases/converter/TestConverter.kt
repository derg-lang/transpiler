package com.github.derg.transpiler.phases.converter

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestConverter
{
    // TODO: Writing these tests are quite difficult since the various HIR nodes have a unique id, the AST nodes do not.
    //       For the time being, they can serve as a sort of compile-time test, ensuring that the code at least is
    //       valid.
    @Disabled
    @Test
    fun `Given expression, when converting, then correct outcome`()
    {
        assertEquals(true.hirNot, (true.astNot).toHir())
        assertEquals(1.hirPlus, (1.astPlus).toHir())
        assertEquals(1.hirMinus, (1.astMinus).toHir())
        
        assertEquals(1 hirEq 2, (1 astEq 2).toHir())
        assertEquals(1 hirNe 2, (1 astNe 2).toHir())
        assertEquals(1 hirGe 2, (1 astGe 2).toHir())
        assertEquals(1 hirGt 2, (1 astGt 2).toHir())
        assertEquals(1 hirLe 2, (1 astLe 2).toHir())
        assertEquals(1 hirLt 2, (1 astLt 2).toHir())
        
        assertEquals(1 hirAdd 2, (1 astAdd 2).toHir())
        assertEquals(1 hirDiv 2, (1 astDiv 2).toHir())
        assertEquals(1 hirMod 2, (1 astMod 2).toHir())
        assertEquals(1 hirMul 2, (1 astMul 2).toHir())
        assertEquals(1 hirSub 2, (1 astSub 2).toHir())
        
        assertEquals(true hirAnd false, (true astAnd false).toHir())
        assertEquals(true hirOr false, (true astOr false).toHir())
        assertEquals(true hirXor false, (true astXor false).toHir())
        
        assertEquals(true.hirNot, true.astNot.toHir())
        assertEquals(1.hirMinus, 1.astMinus.toHir())
        assertEquals(1.hirPlus, 1.astPlus.toHir())
        
        assertEquals(1 hirCatch 2, (1 astCatch 2).toHir())
        assertEquals(1 hirCatchError 2.hirReturnValue, (1 astCatchError 2.astReturnValue).toHir())
        assertEquals(1 hirCatchValue 2.hirReturnValue, (1 astCatchValue 2.astReturnValue).toHir())
    }
}
