package eu.stamp_project.prettifier.prioritize;

import eu.stamp_project.dspot.common.test_framework.TestFramework;
import eu.stamp_project.prettifier.AbstractTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import spoon.Launcher;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtNamedElement;
import spoon.reflect.factory.Factory;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class MostAddedCoveragePrioritizerTest extends AbstractTest {

    private Factory factory;

    @BeforeEach
    @Override
    public void setUp() throws Exception {
        super.setUp();
        Launcher launcher = new Launcher();
        launcher.addInputResource(getAbsolutePathToProjectRoot() + "src/test/java/eu/stamp_project/AppTest.java");
        launcher.buildModel();
        TestFramework.init(launcher.getFactory());
        factory = launcher.getFactory();
    }

    @Test
    public void testPrioritizeMostCoverage() {
        MostAddedCoveragePrioritizer mostAddedCoveragePrioritizer = new MostAddedCoveragePrioritizer();
        List<CtMethod<?>> methods = new ArrayList<>();

        methods.add(factory.Class().get("eu.stamp_project.AppTest")
                .getMethodsByName("testAddingOneLineOfCoverageInOneMethod").get(0)); // Total Coverage: 5
        methods.add(factory.Class().get("eu.stamp_project.AppTest")
                .getMethodsByName("testAddingTwoLinesOfCoverageInOneMethod").get(0)); // Total Coverage: 12
        methods.add(factory.Class().get("eu.stamp_project.AppTest")
                .getMethodsByName("testAddingOneLineOfCoverageInTwoMethods").get(0)); // Total Coverage: 10
        List<CtMethod<?>> prioritizedTests = mostAddedCoveragePrioritizer.prettify(methods);
        String[] desiredOrder = {"testAddingTwoLinesOfCoverageInOneMethod", "testAddingOneLineOfCoverageInTwoMethods", "testAddingOneLineOfCoverageInOneMethod"};
        assertArrayEquals(desiredOrder, prioritizedTests.stream().map(CtNamedElement::getSimpleName).toArray());
    }

}