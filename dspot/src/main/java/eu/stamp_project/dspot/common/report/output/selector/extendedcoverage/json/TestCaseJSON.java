package eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json;

import eu.stamp_project.dspot.selector.extendedcoverageselector.CoverageImprovement;
import eu.stamp_project.dspot.selector.extendedcoverageselector.ExtendedCoverage;

public class TestCaseJSON {

    private final String name;
    private final int nbAssertionAdded;
    private final int nbInputAdded;
    private final CoverageImprovement coverageImprovement;
    private final ExtendedCoverage fullCoverage;
    private final String description;
    private final String nameOfBaseTestCase;

    public TestCaseJSON(String name, int nbAssertionAdded, int nbInputAdded, CoverageImprovement coverageImprovement,
                        ExtendedCoverage fullCoverage, String nameOfBaseTestCase) {
        this.name = name;
        this.nbAssertionAdded = nbAssertionAdded;
        this.nbInputAdded = nbInputAdded;
        this.coverageImprovement = coverageImprovement;
        this.fullCoverage = fullCoverage;
        this.nameOfBaseTestCase = nameOfBaseTestCase;
        this.description = "";
    }

    public TestCaseJSON(String name, int nbAssertionAdded, int nbInputAdded, CoverageImprovement coverageImprovement,
                        ExtendedCoverage fullCoverage, String description, String nameOfBaseTestCase) {
        this.name = name;
        this.nbAssertionAdded = nbAssertionAdded;
        this.nbInputAdded = nbInputAdded;
        this.coverageImprovement = coverageImprovement;
        this.fullCoverage = fullCoverage;
        this.description = description;
        this.nameOfBaseTestCase = nameOfBaseTestCase;
    }

    public TestCaseJSON copyAndUpdateName(String name) {
        return new TestCaseJSON(name, this.nbAssertionAdded, this.nbInputAdded, this.coverageImprovement,
                this.fullCoverage, this.description, this.nameOfBaseTestCase);
    }

    public TestCaseJSON copyAndUpdateDescription(String description) {
        return new TestCaseJSON(this.name, this.nbAssertionAdded, this.nbInputAdded, this.coverageImprovement,
                this.fullCoverage, description, this.nameOfBaseTestCase);
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

    public CoverageImprovement getCoverageImprovement() {
        return coverageImprovement;
    }

    public ExtendedCoverage getFullCoverage() {
        return fullCoverage;
    }

    public String getDescription() {
        return description;
    }

    public String getNameOfBaseTestCase() {
        return nameOfBaseTestCase;
    }
}
