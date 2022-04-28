package eu.stamp_project.prettifier.filter;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import eu.stamp_project.dspot.common.report.output.ClassModificationReport;
import eu.stamp_project.dspot.common.report.output.assertiongenerator.ExceptionAssertionReport;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestCaseJSON;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.dspot.selector.extendedcoverageselector.MethodCoverage;
import eu.stamp_project.prettifier.Main;
import eu.stamp_project.prettifier.Prettifier;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Filters test cases with the aim to only keep test cases that are valued by developers.
 * Requires test cases that were selected by the ExtendedCoverageSelector.
 * <p>
 * Keeps:
 * - tests for an exception
 * <p>
 * Discards:
 * - tests of simple getters or setters (name starts with get/set + only instruction improvement in first line)
 * - tests of hashCode (as they are simple value comparisons until now)
 */
public class DevFriendlyTestFilter implements Prettifier {

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {

        boolean coverageReportPresent = Main.report.isExtendedCoverageReportPresent(this.getClass().getSimpleName());
        if (!coverageReportPresent) {
            return amplifiedTestsToBePrettified;
        }
        TestClassJSON amplificationReport = Main.report.extendedCoverageReport;
        Map<String, TestCaseJSON> mapTestNameToResult = amplificationReport.mapTestNameToResult();

        boolean modificationReportPresent = Main.report.isModificationReportPresent(this.getClass().getSimpleName());
        if (!modificationReportPresent) {
            return amplifiedTestsToBePrettified;
        }
        ClassModificationReport modificationReport = Main.report.modificationReport;

        List<CtMethod<?>> prettifiedTests = new ArrayList<>(filterExceptionTests(amplifiedTestsToBePrettified, modificationReport));
        Main.report.filterReport.numberOfIncludedExceptionTests = prettifiedTests.size();

        List<CtMethod<?>> excludedTestsForGettersSetters = filterSimpleGetterSetterTests(amplifiedTestsToBePrettified,
                mapTestNameToResult);
        Main.report.filterReport.numberOfExcludedGetterSetterTests = excludedTestsForGettersSetters.size();

        List<CtMethod<?>> excludedTestsForHashCode = filterHashCodeTests(amplifiedTestsToBePrettified, mapTestNameToResult);
        Main.report.filterReport.numberOfExcludedHashCodeTests = excludedTestsForHashCode.size();

        // remaining tests that passed all filters
        prettifiedTests.addAll(amplifiedTestsToBePrettified);
        Main.report.filterReport.totalNumberOfIncludedTests = prettifiedTests.size();

        return prettifiedTests;
    }

    /**
     * Detects all tests that check for an exception being thrown.
     *
     * @param tests the tests to be filtered, the filtered tests are removed from this list.
     * @return all tests where the assertion checks for an exception.
     */
    private List<CtMethod<?>> filterExceptionTests(List<CtMethod<?>> tests, ClassModificationReport modificationReport) {
        List<CtMethod<?>> filteredTests = new ArrayList<>();
        for (CtMethod<?> test : tests) {
            List<AmplifierReport> modificationsForTest = modificationReport.getModificationsForTest(test);
            if (modificationsForTest.stream().anyMatch(reports ->
                    reports.getReportType().equals(ExceptionAssertionReport.class.getCanonicalName()))) {
                // an assertion expecting an exception was generated
                filteredTests.add(test);
            }
        }
        tests.removeAll(filteredTests);
        return filteredTests;
    }

    /**
     * Detects all tests that test a simple getter or setter.
     *
     * @param tests the tests to be filtered, the filtered tests are removed from this list.
     * @return all tests that only increase coverage in the first and only line of a method named "get..." or "set..."
     */
    private List<CtMethod<?>> filterSimpleGetterSetterTests(List<CtMethod<?>> tests, Map<String, TestCaseJSON> mapTestNameToResult) {
        List<CtMethod<?>> filteredTests = new ArrayList<>();
        for (CtMethod<?> test : tests) {
            TestCaseJSON coverageResult = mapTestNameToResult.get(test.getSimpleName());
            if (testsOnlySimpleGetterOrSetter(test, coverageResult)) {
                filteredTests.add(test);
            }
        }
        tests.removeAll(filteredTests);
        return filteredTests;
    }

    private boolean testsOnlySimpleGetterOrSetter(CtMethod<?> test, TestCaseJSON coverageResult) {
        List<MethodCoverage> getterSetterCoverage = coverageResult.getCoverageImprovement().getInstructionImprovement().getCoverageForMethodsMatching(
                "(get|set|is).*");
        if (getterSetterCoverage.isEmpty()) {
            // covers no getters or setters
            return false;
        }

        List<MethodCoverage> notGetterSetterCoverage =
                coverageResult.getCoverageImprovement().getInstructionImprovement().getCoverageForMethodsMatching(
                        "^(?!(get|set|is)).+");
        if (!notGetterSetterCoverage.isEmpty()) {
            // covers other methods than getters & setters
            return false;
        }

        // check if covered getter / setter is "simple" -> has only one line
        for (MethodCoverage methodCoverage : getterSetterCoverage) {
            if (methodCoverage.lineCoverage.size() > 1) {
                // tests at least one complex getter/setter
                return false;
            }
        }
        return true;
    }

    /**
     * Detects all tests that test the hashCode method.
     *
     * @param tests the tests to be filtered, the filtered tests are removed from this list.
     * @return all tests that only increase coverage a method named "hashCode".
     */
    private List<CtMethod<?>> filterHashCodeTests(List<CtMethod<?>> tests, Map<String, TestCaseJSON> mapTestNameToResult) {
        List<CtMethod<?>> filteredTests = new ArrayList<>();
        for (CtMethod<?> test : tests) {
            TestCaseJSON coverageResult = mapTestNameToResult.get(test.getSimpleName());
            if (!coverageResult.getCoverageImprovement().getInstructionImprovement().getCoverageForMethodsMatching(
                    "hashCode").isEmpty()) {
                filteredTests.add(test);
            }
        }
        tests.removeAll(filteredTests);
        return filteredTests;
    }

}
