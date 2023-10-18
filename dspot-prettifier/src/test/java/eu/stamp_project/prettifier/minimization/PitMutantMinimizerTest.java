package eu.stamp_project.prettifier.minimization;

import eu.stamp_project.Utils;
import eu.stamp_project.dspot.common.test_framework.TestFramework;
import eu.stamp_project.dspot.selector.pitmutantscoreselector.AbstractPitResult;
import eu.stamp_project.prettifier.AbstractTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import spoon.reflect.code.CtInvocation;
import spoon.reflect.declaration.CtClass;
import spoon.reflect.declaration.CtMethod;

import java.util.List;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 01/03/19
 */
public class PitMutantMinimizerTest extends AbstractTest {

    CtClass<?> testClass;
    CtMethod<?> testMethod;
    PitMutantMinimizer minimizer;

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        this.testClass = Utils.findClass("eu.stamp_project.AppTest");
        this.testMethod = Utils.findMethod(testClass, "test1");
        this.minimizer = new PitMutantMinimizer(
                testClass,
                this.configuration.getBuilderEnum().getAutomaticBuilder(this.configuration),
                this.configuration.getAbsolutePathToProjectRoot(),
                this.configuration.getClasspathClassesProject(),
                this.configuration.getAbsolutePathToTestClasses()
        );
    }

    @Test
    public void test() {

        /*
               Test the minimization
         */

        final CtMethod<?> minimize = minimizer.minimize(testMethod);
        System.out.println(minimize);
        Assertions.assertEquals(4, testMethod.getElements(TestFramework.ASSERTIONS_FILTER).size());
        Assertions.assertEquals(1, minimize.getElements(TestFramework.ASSERTIONS_FILTER).size());
        Assertions.assertEquals(8, testMethod.getBody().getStatements().size());
        Assertions.assertEquals(4, minimize.getBody().getStatements().size());
    }

    @Test
    public void testOnTryCatchAssertion() {
        this.testMethod = Utils.findMethod(testClass, "test2_failAssert0");
        final CtMethod<?> minimize = minimizer.minimize(testMethod);
    }

    @Test
    public void testPrintCompileAndRunPit() {
        /*
            Test that the Minimizer is able to print, compile and run PIT
         */

        final List<AbstractPitResult> abstractPitResults = minimizer.printCompileAndRunPit(testClass);
        Assertions.assertTrue(abstractPitResults.size() > 0);
        System.out.println(abstractPitResults);
    }

    @Test
    public void testRemoveCloneAndInsert() {
        /*
            Test the method removeCloneAndInsert()
                This method should let the method given in parameter the same
                The returned method should be a clone of the given method, minus one assertions
         */

        Assertions.assertEquals(8, testMethod.getBody().getStatements().size());

        final List<CtInvocation<?>> assertions =
                testMethod.getElements(TestFramework.ASSERTIONS_FILTER);
        final String beforeString = testMethod.toString();
        CtMethod<?> ctMethod = minimizer.removeCloneAndInsert(assertions, testMethod, 0);
        String afterString = testMethod.toString();
        Assertions.assertEquals(beforeString, afterString);
        Assertions.assertEquals(7, ctMethod.getBody().getStatements().size());
        Assertions.assertEquals(8, testMethod.getBody().getStatements().size());

        ctMethod = minimizer.removeCloneAndInsert(assertions, testMethod, 1);
        afterString = testMethod.toString();
        Assertions.assertEquals(beforeString, afterString);
        Assertions.assertEquals(7, ctMethod.getBody().getStatements().size());
        Assertions.assertEquals(8, testMethod.getBody().getStatements().size());

        ctMethod = minimizer.removeCloneAndInsert(assertions, testMethod, 2);
        afterString = testMethod.toString();
        Assertions.assertEquals(beforeString, afterString);
        Assertions.assertEquals(7, ctMethod.getBody().getStatements().size());
        Assertions.assertEquals(8, testMethod.getBody().getStatements().size());
    }
}
