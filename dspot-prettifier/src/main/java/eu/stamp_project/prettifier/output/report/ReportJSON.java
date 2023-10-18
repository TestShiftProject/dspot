package eu.stamp_project.prettifier.output.report;

import eu.stamp_project.dspot.common.report.output.ClassModificationReport;
import eu.stamp_project.dspot.common.report.output.ModificationReportImpl;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestCaseJSON;
import eu.stamp_project.dspot.common.report.output.selector.extendedcoverage.json.TestClassJSON;
import eu.stamp_project.prettifier.Main;
import eu.stamp_project.prettifier.Util;
import eu.stamp_project.prettifier.configuration.UserInput;
import eu.stamp_project.prettifier.output.report.minimization.extendedcoverage.ExtendedCoverageMinimizationJSON;
import eu.stamp_project.prettifier.output.report.minimization.general.GeneralMinimizationJSON;
import eu.stamp_project.prettifier.output.report.minimization.pit.PitMinimizationJSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.declaration.CtType;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 25/02/19
 */
public class ReportJSON {

    private static final Logger LOGGER = LoggerFactory.getLogger(ReportJSON.class);

    public final GeneralMinimizationJSON generalMinimizationJSON;
    public final PitMinimizationJSON pitMinimizationJSON;
    public final ExtendedCoverageMinimizationJSON extendedCoverageMinimizationJSON;

    public final TestClassJSON extendedCoverageReport;
    public final ClassModificationReport modificationReport;
    public final RenamingReport renamingReport;
    public final FilterReport filterReport;

    public int nbTestMethods;

    public double medianNbStatementBefore;

    public double medianNbStatementAfter;

    public ReportJSON(UserInput configuration) {
        this.generalMinimizationJSON = new GeneralMinimizationJSON();
        this.pitMinimizationJSON = new PitMinimizationJSON();
        this.extendedCoverageReport = Util.readExtendedCoverageResultJSON(configuration);
        this.modificationReport = Util.readModificationReport(configuration);
        this.extendedCoverageMinimizationJSON = new ExtendedCoverageMinimizationJSON();
        this.renamingReport = new RenamingReport();
        this.filterReport = new FilterReport();
    }

    public void output(UserInput configuration, CtType<?> amplifiedTestClass) {
        Util.writeReportJSON(configuration, this, "_prettifier");
    }

    /**
     * @param prettifierToApply name of the prettifier that needs the report (used in error message)
     * @return whether the {@link eu.stamp_project.dspot.selector.ExtendedCoverageSelector} report from DSpot was found
     * at the configured DSpot output location. Logs an error if not.
     */
    public boolean isExtendedCoverageReportPresent(String prettifierToApply) {
        if (extendedCoverageReport == null) {
            LOGGER.error("No json from the ExtendedCoverageSelector found under configured DSpot output path! " +
                    prettifierToApply + " not applied");
            return false;
        }
        return true;
    }

    /**
     * @param prettifierToApply name of the prettifier that needs the report (used in error message)
     * @return whether the {@link ModificationReportImpl} from DSpot was found
     * at the configured DSpot output location. Logs an error if not.
     */
    public boolean isModificationReportPresent(String prettifierToApply) {
        ClassModificationReport report = Main.report.modificationReport;
        if (report == null) {
            LOGGER.error("No modification report found under configured DSpot output path! " + prettifierToApply
                    + " not applied");
            return false;
        }
        return true;
    }

    /**
     * Updates the reports provided by DSpot to use the newName to identify a test case instead of oldName.
     * Changes the extendedCoverageReport and the modificationReport.
     */
    public void updateReportsForNewTestName(String oldName, String newName) {
        if (isExtendedCoverageReportPresent("update report")) {
            TestCaseJSON testCaseJSON = extendedCoverageReport.mapTestNameToResult().get(oldName);
            extendedCoverageReport.updateTestCase(testCaseJSON, testCaseJSON.copyAndUpdateName(newName));
        }

        if (isModificationReportPresent("update report")) {
            modificationReport.updateNameOfTest(oldName, newName);
        }
    }
}
