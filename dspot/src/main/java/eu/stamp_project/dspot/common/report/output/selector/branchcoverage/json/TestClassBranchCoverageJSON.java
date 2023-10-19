package eu.stamp_project.dspot.common.report.output.selector.branchcoverage.json;


import eu.stamp_project.dspot.selector.branchcoverageselector.BranchCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.Coverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.LineCoverage;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TestClassBranchCoverageJSON implements eu.stamp_project.dspot.common.report.output.selector.TestClassJSON {


    private List<TestCaseBranchCoverageJSON> testCases;
//    private final Coverage initialCoverage;
    List<BranchCoverage> initialBranchCoverage;
    List<LineCoverage> initialLineCoverage;

    public TestClassBranchCoverageJSON(List<BranchCoverage> initialBranchCoverage, List<LineCoverage> initialLineCoverage) {
        this.initialBranchCoverage = initialBranchCoverage;
        this.initialLineCoverage = initialLineCoverage;
    }

    public boolean addTestCase(TestCaseBranchCoverageJSON testCaseJSON) {
        if (this.testCases == null) {
            this.testCases = new ArrayList<>();
        }
        return this.testCases.add(testCaseJSON);
    }

    public List<TestCaseBranchCoverageJSON> getTestCases() {
        return this.testCases;
    }



    public List<BranchCoverage> getInitialBranchCoverage() {
        return initialBranchCoverage;
    }

    public List<LineCoverage> getInitialLineCoverage() {
        return initialLineCoverage;
    }
}
