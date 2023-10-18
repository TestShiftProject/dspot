package eu.stamp_project.dspot.common.report.output;

public class AmplifierReport {

    public AmplifierReport() {}

    protected boolean assertionReport = isAssertionReport();
    protected String reportType = getReportType();

    public boolean isAssertionReport() {
        return false;
    }

    public String getReportType() {
        return this.getClass().getCanonicalName();
    }

}
