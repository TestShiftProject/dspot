package eu.stamp_project.dspot.common.report.output.amplifiers;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;

public class MethodDuplicationAmplifierReport extends AmplifierReport {

    private final String duplicatedCall;

    public MethodDuplicationAmplifierReport(String duplicatedCall) {
        this.duplicatedCall = duplicatedCall;
    }

    public String getDuplicatedCall() {
        return duplicatedCall;
    }
}
