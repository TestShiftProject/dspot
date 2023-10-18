package eu.stamp_project.dspot.common.report.output.assertiongenerator;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import eu.stamp_project.dspot.common.test_framework.TestFramework;
import eu.stamp_project.dspot.common.test_framework.assertions.AssertEnum;
import spoon.reflect.code.CtInvocation;
import spoon.reflect.code.CtStatement;
import spoon.reflect.declaration.CtMethod;

/**
 * Reports about a generated assertion that compares the result of a method call to a value.
 */
public class ValueAssertionReport extends AmplifierReport {

    private final String assertMethodName;
    private final String assertMethodDeclaredType;
    private final String testedValue;
    private final String expectedValue;
    private final AssertEnum assertionType;

    public ValueAssertionReport(CtStatement assertStatement, CtMethod<?> test) {
        CtInvocation<?> assertInvocation = (CtInvocation<?>) assertStatement;
        this.assertMethodName = assertInvocation.getExecutable().getSimpleName();
        this.assertMethodDeclaredType = assertInvocation.getExecutable().getDeclaringType().getQualifiedName();

        // TODO for single argument, testedValue should be assigend and expected value should be null (if changed:
        //  change in description generator too)

        if (assertInvocation.getArguments().size() > 1) {
            this.testedValue = assertInvocation.getArguments().get(1).toString();
        } else {
            this.testedValue = null;
        }
        this.expectedValue = assertInvocation.getArguments().get(0).toString();
        this.assertionType = TestFramework.get().classifyAssertMethod(this.assertMethodName, test);
    }

    public String getAssertMethodName() {
        return assertMethodName;
    }

    public String getAssertMethodDeclaredType() {
        return assertMethodDeclaredType;
    }

    public String getTestedValue() {
        return testedValue;
    }

    public String getExpectedValue() {
        return expectedValue;
    }

    @Override
    public boolean isAssertionReport() {
        return true;
    }

    public AssertEnum getAssertionType() {
        return assertionType;
    }
}
