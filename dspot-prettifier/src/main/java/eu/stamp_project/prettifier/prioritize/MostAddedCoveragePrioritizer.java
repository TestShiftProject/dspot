package eu.stamp_project.prettifier.prioritize;

import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestCaseJSON;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.prettifier.Main;
import eu.stamp_project.prettifier.Prettifier;
import spoon.reflect.declaration.CtMethod;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * Prioritize test cases according to which one contributes the most additional coverage.
 * Requires test cases that were selected by the ExtendedCoverageSelector.
 */
public class MostAddedCoveragePrioritizer implements Prettifier {

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        boolean coverageReportPresent = Main.report.isExtendedCoverageReportPresent(this.getClass().getSimpleName());
        if (!coverageReportPresent) {
            return amplifiedTestsToBePrettified;
        }
        TestClassJSON amplificationReport = Main.report.extendedCoverageReport;
        Map<String, TestCaseJSON> mapTestNameToResult = amplificationReport.mapTestNameToResult();


        amplifiedTestsToBePrettified.sort(Comparator.comparingInt(test ->
                mapTestNameToResult.get(test.getSimpleName()).getCoverageImprovement().totalCoverageIncrease()));
        Collections.reverse(amplifiedTestsToBePrettified);
        return amplifiedTestsToBePrettified;
    }

}
