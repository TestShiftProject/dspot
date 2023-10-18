package eu.stamp_project.dspot.common.report;

import eu.stamp_project.dspot.common.report.error.Error;
import eu.stamp_project.dspot.common.report.error.ErrorReport;
import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import eu.stamp_project.dspot.common.report.output.ModificationReport;
import eu.stamp_project.dspot.common.report.output.ModificationReportImpl;
import eu.stamp_project.dspot.common.report.output.OutputReport;
import eu.stamp_project.dspot.common.report.output.selector.TestSelectorElementReport;
import eu.stamp_project.dspot.common.report.output.selector.TestSelectorReport;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.util.List;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 29/10/18
 */
public class GlobalReport implements Report, ErrorReport, OutputReport, TestSelectorReport, ModificationReport {

    private final OutputReport outputReport;

    private final ErrorReport errorReport;

    private final TestSelectorReport testSelectorReport;

    private final ModificationReportImpl modificationReport;

    public GlobalReport(OutputReport outputReport, ErrorReport errorReport, TestSelectorReport testSelectorReport,
                        ModificationReportImpl modificationReport) {
        this.outputReport = outputReport;
        this.errorReport = errorReport;
        this.testSelectorReport = testSelectorReport;
        this.modificationReport = modificationReport;
    }

    /* REPORT METHODS */

    @Override
    public void output(String outputDirectory) {
        this.testSelectorReport.output(outputDirectory);
        this.errorReport.output(outputDirectory);
        this.outputReport.output(outputDirectory);
        this.modificationReport.output(outputDirectory);
    }

    @Override
    public void outputForClass(String outputDirectory, CtType<?> testClass) {
        this.testSelectorReport.outputForClass(outputDirectory, testClass);
        this.errorReport.outputForClass(outputDirectory, testClass);
        this.outputReport.outputForClass(outputDirectory, testClass);
        this.modificationReport.outputForClass(outputDirectory, testClass);
    }

    @Override
    public void reset() {
        this.testSelectorReport.reset();
        this.errorReport.reset();
        this.outputReport.reset();
        this.modificationReport.reset();
    }

    /* ERROR REPORT METHODS */

    @Override
    public void addInputError(Error error) {
        this.errorReport.addInputError(error);
    }

    @Override
    public void addError(Error error) {
        this.errorReport.addError(error);
    }

    @Override
    public List<Error> getErrors() {
        return this.errorReport.getErrors();
    }

    @Override
    public List<Error> getInputError() {
        return this.errorReport.getInputError();
    }

    /* TEST SELECTOR REPORT METHODS */

    @Override
    public void addTestSelectorReportForTestClass(CtType<?> testClass, TestSelectorElementReport report) {
        this.testSelectorReport.addTestSelectorReportForTestClass(testClass, report);
    }

    /* OUTPUT REPORT METHODS */

    @Override
    public void addNumberAmplifiedTestMethodsToTotal(int numberOfAmplifiedTestMethods) {
        this.outputReport.addNumberAmplifiedTestMethodsToTotal(numberOfAmplifiedTestMethods);
    }

    @Override
    public void addPrintedTestClasses(String line, boolean printedAmplifiedTestClass) {
        this.outputReport.addPrintedTestClasses(line, printedAmplifiedTestClass);
    }

    /* MODIFICATION REPORT METHODS */

    @Override
    public void reportModification(CtType<?> testClass, String testNameBeforeModification,
                                   String testNameAfterModification, AmplifierReport report) {
        this.modificationReport.reportModification(testClass, testNameBeforeModification, testNameAfterModification,
                report);
    }

    @Override
    public void filterModifications(CtType<?> testClass, List<CtMethod<?>> selectedTests) {
        this.modificationReport.filterModifications(testClass, selectedTests);
    }
}
