package eu.stamp_project.dspot.common.report.output.selector.branchcoverage.json;


import eu.stamp_project.dspot.selector.branchcoverageselector.BranchCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.Coverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.LineCoverage;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TestClassJSON implements eu.stamp_project.dspot.common.report.output.selector.TestClassJSON {


    private List<TestCaseJSON> testCases;
//    private final Coverage initialCoverage;
    List<BranchCoverage> initialBranchCoverage;
    List<LineCoverage> initialLineCoverage;
//    Map<CtMethod<?>, List<BranchCoverage>> branchCoveragePerTestCase;
//    Map<CtMethod<?>, List<LineCoverage>> lineCoveragePerPerTestCase;

    public TestClassJSON(List<BranchCoverage> initialBranchCoverage, List<LineCoverage> initialLineCoverage, Map<CtMethod<?>, List<BranchCoverage>> branchCoveragePerTestCase,
            Map<CtMethod<?>, List<LineCoverage>> lineCoveragePerPerTestCase) {
        this.initialBranchCoverage = initialBranchCoverage;
        this.initialLineCoverage = initialLineCoverage;
//        this.branchCoveragePerTestCase = branchCoveragePerTestCase;
//        this.lineCoveragePerPerTestCase = lineCoveragePerPerTestCase;
    }

    public boolean addTestCase(TestCaseJSON testCaseJSON) {
        if (this.testCases == null) {
            this.testCases = new ArrayList<>();
        }
        return this.testCases.add(testCaseJSON);
    }

    public List<TestCaseJSON> getTestCases() {
        return this.testCases;
    }

//    public Coverage getInitialCoverage() {
//        return initialCoverage;
//    }


    public List<BranchCoverage> getInitialBranchCoverage() {
        return initialBranchCoverage;
    }

    public List<LineCoverage> getInitialLineCoverage() {
        return initialLineCoverage;
    }

//    public Map<CtMethod<?>, List<BranchCoverage>> getBranchCoveragePerTestCase() {
//        return branchCoveragePerTestCase;
//    }
//
//    public Map<CtMethod<?>, List<LineCoverage>> getLineCoveragePerPerTestCase() {
//        return lineCoveragePerPerTestCase;
//    }
}
