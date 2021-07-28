package eu.stamp_project.dspot.selector.extendedcoverageselector;

import eu.stamp_project.testrunner.listener.Coverage;
import eu.stamp_project.testrunner.listener.impl.CoverageImpl;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExtendedCoverageTest {

    String execution_path_1 = "package.package.class1:method1+()V+1,0,3,0|method2+(LCallback;)V+0,4" +
            "-package.package.class2:method1+()V+4,3,0";
    String execution_path_2 = "package.package.class1:method1+()V+1,0,3,5|method2+(LCallback;)V+0,1" +
            "-package.package.class2:method1+()V+4,3,0";
    String execution_path_3 = "package.package.class1:method1+()V+1,0,3,5|method2+(LCallback;)V+0,4" +
            "-package.package.class2:method1+()V+4,3,0";

    private ExtendedCoverage genExtendedCoverageWithPath(String executionPath) {
        Coverage coverage = new CoverageImpl(8, 10);
        coverage.setExecutionPath(executionPath);

        return new ExtendedCoverage(coverage);
    }

    @Test
    public void constructExtendedCoverageFromCoverage() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);

        ProjectCoverageMap instructionsCoveredPerClass = new ProjectCoverageMap();
        ClassCoverageMap instructionsCoveredPerMethod = new ClassCoverageMap();
        instructionsCoveredPerMethod.addMethodCoverage("method1", new MethodCoverage(Arrays.asList(1, 0, 3, 0), "()V"));
        instructionsCoveredPerMethod.addMethodCoverage("method2", new MethodCoverage(Arrays.asList(0, 4), "(LCallback;)V"));
        instructionsCoveredPerClass.addClassCoverage("package.package.class1", instructionsCoveredPerMethod);
        ClassCoverageMap instructionsCoveredPerMethod2 = new ClassCoverageMap();
        instructionsCoveredPerMethod2.addMethodCoverage("method1", new MethodCoverage(Arrays.asList(4, 3, 0),
                "()V"));
        instructionsCoveredPerClass.addClassCoverage("package.package.class2", instructionsCoveredPerMethod2);

        Assert.assertEquals(extendedCoverage.getInstructionsProjectCoverageMap(), instructionsCoveredPerClass);
    }

    @Test
    public void testAccumulate() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);
        ExtendedCoverage extendedCoverage2 = genExtendedCoverageWithPath(execution_path_2);

        extendedCoverage.accumulate(extendedCoverage2);

        Assert.assertEquals(extendedCoverage, genExtendedCoverageWithPath(execution_path_3));
    }

    @Test
    public void testBetterThan() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);
        ExtendedCoverage extendedCoverage2 = genExtendedCoverageWithPath(execution_path_2);

        // execution_path_1 covers more in class1.method2
        Assert.assertTrue(extendedCoverage.isBetterThan(extendedCoverage2));
        // execution_path_2 covers more in class1.method1
        Assert.assertTrue(extendedCoverage2.isBetterThan(extendedCoverage));
    }


    String execution_path_missing_a_class = "package.package.class1:method1+()V+1,0,3,0|method2+(LCallback;)V+0,4";
    @Test
    public void testBetterThanForNotExistingClassInFirstCoverage() {
        ExtendedCoverage extendedCoverage_missing = genExtendedCoverageWithPath(execution_path_missing_a_class);
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);

        Assert.assertFalse(extendedCoverage_missing.isBetterThan(extendedCoverage));
        Assert.assertTrue(extendedCoverage.isBetterThan(extendedCoverage_missing));
    }

    @Test
    public void testBetterThanForNotExistingClassInSecondCoverage() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);
        ExtendedCoverage extendedCoverage_missing = genExtendedCoverageWithPath(execution_path_missing_a_class);

        Assert.assertTrue(extendedCoverage.isBetterThan(extendedCoverage_missing));
        Assert.assertFalse(extendedCoverage_missing.isBetterThan(extendedCoverage));
    }


    String execution_path_missing_a_method = "package.package.class1:method1+()V+1,0,3,0" +
                                            "-package.package.class2:method1+()V+4,3,0";
    @Test
    public void testBetterThanForNotExistingMethodInFirstCoverage() {
        ExtendedCoverage extendedCoverage_missing = genExtendedCoverageWithPath(execution_path_missing_a_method);
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);

        Assert.assertFalse(extendedCoverage_missing.isBetterThan(extendedCoverage));
        Assert.assertTrue(extendedCoverage.isBetterThan(extendedCoverage_missing));
    }

    @Test
    public void testBetterThanForNotExistingMethodInSecondCoverage() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);
        ExtendedCoverage extendedCoverage_missing = genExtendedCoverageWithPath(execution_path_missing_a_method);

        Assert.assertTrue(extendedCoverage.isBetterThan(extendedCoverage_missing));
        Assert.assertFalse(extendedCoverage_missing.isBetterThan(extendedCoverage));

    }

    @Test
    public void testBetterThanNotReflexive() {
        ExtendedCoverage extendedCoverage = genExtendedCoverageWithPath(execution_path_1);

        Assert.assertFalse(extendedCoverage.isBetterThan(extendedCoverage));
    }

    @Test
    public void testCoveragePerLineMap() {
        MethodCoverage methodCoverage = genExtendedCoverageWithPath(execution_path_1)
                .getInstructionsProjectCoverageMap().getCoverageForClass("package.package.class1")
                .getCoverageForMethod("method1");

        Map<Integer, Integer> coveragePerLine = methodCoverage.coveragePerLine();
        Assert.assertEquals(coveragePerLine.get(0).intValue(), 1);
        Assert.assertEquals(coveragePerLine.get(2).intValue(), 3);

        System.out.println(methodCoverage.coveragePerLine().toString());
    }
}
