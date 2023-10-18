package eu.stamp_project.dspot.selector;

import eu.stamp_project.dspot.common.automaticbuilder.AutomaticBuilder;
import eu.stamp_project.dspot.common.configuration.UserInput;
import eu.stamp_project.dspot.common.configuration.options.CommentEnum;
import eu.stamp_project.dspot.common.miscellaneous.AmplificationHelper;
import eu.stamp_project.dspot.common.miscellaneous.Counter;
import eu.stamp_project.dspot.common.miscellaneous.DSpotUtils;
import eu.stamp_project.dspot.common.report.output.selector.TestSelectorElementReport;
import eu.stamp_project.dspot.common.report.output.selector.TestSelectorElementReportImpl;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestCaseJSON;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.dspot.selector.extendedcoverageselector.CoverageImprovement;
import eu.stamp_project.dspot.selector.extendedcoverageselector.ExtendedCoverage;
import eu.stamp_project.testrunner.EntryPoint;
import eu.stamp_project.testrunner.listener.CoveragePerTestMethod;
import eu.stamp_project.testrunner.runner.ParserOptions;
import spoon.reflect.code.CtComment;
import spoon.reflect.declaration.CtClass;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtNamedElement;
import spoon.reflect.declaration.CtType;

import java.util.*;
import java.util.concurrent.TimeoutException;

/**
 * Selects amplified test cases based on whether they cover new instructions in the code.
 */
public class ExtendedCoverageSelector extends TakeAllSelector {

    ExtendedCoverage initialCoverage;

    ExtendedCoverage cumulativeAmplifiedCoverage;

    Map<CtMethod<?>, CoverageImprovement> coverageImprovementPerAmplifiedMethod;

    Map<CtMethod<?>, ExtendedCoverage> fullCoveragePerAmplifiedMethod;

    private TestSelectorElementReport lastReport;

    public ExtendedCoverageSelector(AutomaticBuilder automaticBuilder, UserInput configuration) {
        super(automaticBuilder, configuration);
        this.coverageImprovementPerAmplifiedMethod = new HashMap<>();
        this.fullCoveragePerAmplifiedMethod = new HashMap<>();
        EntryPoint.coverageDetail = ParserOptions.CoverageTransformerDetail.METHOD_DETAIL;
    }

    public ExtendedCoverageSelector(AutomaticBuilder automaticBuilder, UserInput configuration, CtType<?> testClass) {
        this(automaticBuilder, configuration);
        this.currentClassTestToBeAmplified = testClass;
    }

    @Override
    public List<CtMethod<?>> selectToAmplify(CtType<?> classTest, List<CtMethod<?>> testsToBeAmplified) {
        this.currentClassTestToBeAmplified = classTest;
        // calculate existing coverage of the whole test suite
        try {
            this.initialCoverage = new ExtendedCoverage(EntryPoint
                    .runCoverage(classpath + AmplificationHelper.PATH_SEPARATOR + targetClasses,
                            this.targetClasses,
                            this.currentClassTestToBeAmplified.getQualifiedName()));
        } catch (TimeoutException e) {
            throw new RuntimeException(e);
        }
        this.cumulativeAmplifiedCoverage = this.initialCoverage.clone();
        return testsToBeAmplified;
    }

    @Override
    public List<CtMethod<?>> selectToKeep(List<CtMethod<?>> amplifiedTestToBeKept) {
        CoveragePerTestMethod coveragePerTestMethod = computeCoverageForGivenTestMethods(amplifiedTestToBeKept);

        final List<CtMethod<?>> methodsKept = new ArrayList<>();
        for (CtMethod<?> ctMethod : amplifiedTestToBeKept) {
            ExtendedCoverage newCoverage = new ExtendedCoverage(coveragePerTestMethod
                    .getCoverageOf(ctMethod.getParent(CtClass.class).getQualifiedName() + "#" + ctMethod.getSimpleName()));

            if (newCoverage.isBetterThan(this.cumulativeAmplifiedCoverage)) {
                // Note: we still explain to users that the coverage improves compared to the initial coverage, even
                // though we compare to the cumulative up top (to not have overlapping proposed test cases)
                // Should we adapt the coverage improvement to also be calculated against the cumulative coverage?
                // !!! Changing this will break the current ExtendedCoverageMinimizer
                CoverageImprovement coverageImprovement = newCoverage.coverageImprovementOver(this.initialCoverage);
                DSpotUtils.addComment(ctMethod, coverageImprovement.toString(), CtComment.CommentType.BLOCK, CommentEnum.Coverage);
                methodsKept.add(ctMethod);
                this.coverageImprovementPerAmplifiedMethod.put(ctMethod, coverageImprovement);
                this.fullCoveragePerAmplifiedMethod.put(ctMethod, newCoverage);
                this.cumulativeAmplifiedCoverage.accumulate(newCoverage);
            }
        }
        this.selectedAmplifiedTest.addAll(methodsKept);

        return methodsKept;
    }

    public CoveragePerTestMethod computeCoverageForGivenTestMethods(List<CtMethod<?>> testsToBeAmplified) {
        final String[] methodNames = testsToBeAmplified.stream().map(CtNamedElement::getSimpleName)
                .toArray(String[]::new);
        try {
            return EntryPoint.runCoveragePerTestMethods(
                    this.classpath + AmplificationHelper.PATH_SEPARATOR + this.targetClasses,
                    this.targetClasses,
                    this.currentClassTestToBeAmplified.getQualifiedName(),
                    methodNames);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public TestSelectorElementReport report() {
        if (currentClassTestToBeAmplified == null) {
            return lastReport;
        }
        final String report = "Amplification results with " + this.selectedAmplifiedTest.size() + " new tests.";
        lastReport = new TestSelectorElementReportImpl(report, jsonReport(), Collections.emptyList(), "");
        reset();
        return lastReport;
    }

    private TestClassJSON jsonReport() {
        TestClassJSON testClassJSON;
        testClassJSON = new TestClassJSON(this.initialCoverage,
                this.cumulativeAmplifiedCoverage.coverageImprovementOver(this.initialCoverage),
                this.cumulativeAmplifiedCoverage);
        this.selectedAmplifiedTest.stream()
                .map(ctMethod -> new TestCaseJSON(ctMethod.getSimpleName(),
                        Counter.getAssertionOfSinceOrigin(ctMethod),
                        Counter.getInputOfSinceOrigin(ctMethod),
                        this.coverageImprovementPerAmplifiedMethod.get(ctMethod),
                        this.fullCoveragePerAmplifiedMethod.get(ctMethod),
                        AmplificationHelper.getOriginalTestMethod(ctMethod).getSimpleName()))
                .forEach(testClassJSON::addTestCase);

        return testClassJSON;
    }


}
