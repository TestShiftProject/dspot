package eu.stamp_project.dspot.common.report.output.assertiongenerator;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;

public class ExceptionAssertionReport extends AmplifierReport {

    private String exceptionName;

    public ExceptionAssertionReport(String exceptionName) {
        this.exceptionName = exceptionName;
    }

    @Override
    public boolean isAssertionReport() {
        return true;
    }

    public String getExceptionName() {
        return exceptionName;
    }
}
