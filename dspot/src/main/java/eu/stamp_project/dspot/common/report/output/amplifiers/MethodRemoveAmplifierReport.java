package eu.stamp_project.dspot.common.report.output.amplifiers;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;

public class MethodRemoveAmplifierReport extends AmplifierReport {

    private final String removedCall;

    public MethodRemoveAmplifierReport(String removedCall) {
        this.removedCall = removedCall;
    }

    public String getRemovedCall() {
        return removedCall;
    }
}
