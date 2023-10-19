package eu.stamp_project.dspot.common.report.output.selector.branchcoverage.json;

import eu.stamp_project.dspot.selector.branchcoverageselector.BranchCoverage;
import eu.stamp_project.dspot.selector.branchcoverageselector.LineCoverage;
import eu.stamp_project.dspot.selector.extendedcoverageselector.CoverageImprovement;
import eu.stamp_project.dspot.selector.extendedcoverageselector.ExtendedCoverage;

import java.util.List;

public class TestCaseJSON {
    private final String name;
    private final int nbAssertionAdded;
    private final int nbInputAdded;
    private final List<BranchCoverage> branchCoverageList;
    private final List<LineCoverage> lineCoverageList;

    public TestCaseJSON(String name, int nbAssertionAdded, int nbInputAdded, List<BranchCoverage> branchCoverageList, List<LineCoverage> lineCoverageList) {
        this.name = name;
        this.nbAssertionAdded = nbAssertionAdded;
        this.nbInputAdded = nbInputAdded;
        this.branchCoverageList = branchCoverageList;
        this.lineCoverageList = lineCoverageList;
    }

    public String getName() {
        return name;
    }

    public int getNbAssertionAdded() {
        return nbAssertionAdded;
    }

    public int getNbInputAdded() {
        return nbInputAdded;
    }

    public List<BranchCoverage> getBranchCoverageList() {
        return branchCoverageList;
    }

    public List<LineCoverage> getLineCoverageList() {
        return lineCoverageList;
    }
}
