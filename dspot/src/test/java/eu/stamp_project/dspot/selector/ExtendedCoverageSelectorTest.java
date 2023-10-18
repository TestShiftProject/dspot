package eu.stamp_project.dspot.selector;

import eu.stamp_project.testrunner.EntryPoint;
import eu.stamp_project.testrunner.runner.ParserOptions;

public class ExtendedCoverageSelectorTest extends AbstractSelectorRemoveOverlapTest {

    @Override
    protected TestSelector getTestSelector() {
        return new ExtendedCoverageSelector(
                this.builder,
                this.configuration
        );
    }

    @Override
    protected String getContentReportFile() {
        return "Amplification results with 2 new tests.";
    }

    @Override
    public void setUp() {
        super.setUp();
        EntryPoint.coverageDetail = ParserOptions.CoverageTransformerDetail.METHOD_DETAIL;
    }
}
