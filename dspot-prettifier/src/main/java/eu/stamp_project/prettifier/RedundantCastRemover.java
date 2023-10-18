package eu.stamp_project.prettifier;

import eu.stamp_project.dspot.common.test_framework.TestFramework;
import spoon.reflect.code.CtExpression;
import spoon.reflect.code.CtInvocation;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.reference.CtTypeReference;

import java.util.ArrayList;
import java.util.List;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 04/02/19
 */
public class RedundantCastRemover implements Prettifier {

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        List<CtMethod<?>> prettifiedTests = new ArrayList<>();

        for (CtMethod<?> test : amplifiedTestsToBePrettified) {
            prettifiedTests.add(remove(test));
        }

        return prettifiedTests;
    }

    public CtMethod<?> remove(CtMethod<?> testMethod) {
        final List<CtInvocation<?>> assertions = testMethod.getElements(TestFramework.ASSERTIONS_FILTER);
        for (CtInvocation<?> assertion : assertions) {
            // check if assertion compares two values
            // at the moment DSpot does not add a message to the assertion, so if there are two values we expect both to
            // be the compared values
            if (assertion.getArguments().size() >= 2) {
                this.removeCastsInComparingAssertion(assertion, testMethod);
            } else { // assertTrue or assertFalse, assertNull has no assertions generated
                removeCastsInBooleanAssertion(assertion, testMethod);
            }
        }
        return testMethod;
    }

    private void removeCastsInComparingAssertion(CtInvocation<?> assertion, CtMethod<?> testMethod) {
        final CtExpression<?> actualValue = assertion.getArguments().get(assertion.getArguments().size() - 1);
        final CtExpression<?> expectedValue = assertion.getArguments().get(assertion.getArguments().size() - 2);
        // save expression of actual value to report cast reduction
        String oldExpression = actualValue.toString();
        // top cast compared to the expected value
        if (!actualValue.getTypeCasts().isEmpty() &&
                actualValue.getTypeCasts().get(0).equals(expectedValue.getType())) {
            actualValue.getTypeCasts().remove(0);
        }
        // inner casts that can be removed
        removeCastInvocations(actualValue);
        Main.report.renamingReport.addCastReduction(testMethod, oldExpression, actualValue.toString());
    }

    private void removeCastsInBooleanAssertion(CtInvocation<?> assertion, CtMethod<?> testMethod) {
        final CtExpression<?> actualValue = assertion.getArguments().get(0);
        // save expression of actual value to report cast reduction
        String oldExpression = actualValue.toString();
        // in the produced tests there is no final casting of the result value to boolean, so we skip removing casts
        // on the actualValue (and only on the invocations it contains)
        removeCastInvocations(actualValue);
        Main.report.renamingReport.addCastReduction(testMethod, oldExpression, actualValue.toString());
    }

    private void removeCastInvocations(CtExpression<?> current) {
        while (current instanceof CtInvocation<?>) {
            current = ((CtInvocation) current).getTarget();
            if (!current.getTypeCasts().isEmpty() &&
                    matchTypes(current.getTypeCasts().get(0), current.getType())) {
                current.getTypeCasts().remove(0);
            }
        }
    }

    private boolean matchTypes(CtTypeReference<?> toBeMatched, CtTypeReference<?> type) {
        if (type ==  null) {
            return false;
        } else if (toBeMatched.equals(type)) {
            return true;
        } else {
            return matchTypes(toBeMatched, type.getSuperclass());
        }
    }

}
