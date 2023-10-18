package eu.stamp_project.dspot.common.report.output.amplifiers;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;

public class AddLocalVariableAmplifierReport extends AmplifierReport {

    private final String variableName;
    private final String variableValue;
    private final boolean fromAssertion;

    public AddLocalVariableAmplifierReport(String variableName, String variableValue, boolean fromAssertion) {
        this.variableName = variableName;
        this.variableValue = variableValue;
        this.fromAssertion = fromAssertion;
    }

    public String getVariableName() {
        return variableName;
    }

    public String getVariableValue() {
        return variableValue;
    }

    public boolean isFromAssertion() {
        return fromAssertion;
    }

    @Override
    public boolean isAssertionReport() {
        return fromAssertion;
    }
}
